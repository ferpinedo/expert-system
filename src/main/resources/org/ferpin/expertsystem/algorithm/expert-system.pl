% Autor: Fernando Josué Pinedo Orta

:- dynamic rule/1.
:- dynamic premise/3.
:- dynamic conclusion/2.
:- dynamic status/3. % premises 
:- dynamic finalConclusion/1. 

%%%%    Read utilities  %%%%
loadRules(FilePath):-
    open(FilePath , read, Stream),
    % "F:\\Repos\\expert-system\\src\\main\\resources\\org\\ferpin\\expertsystem\\algorithm\\expert-system.pl"
    %open('F:\\Repos\\expert-system\\src\\main\\resources\\org\\ferpin\\expertsystem\\algorithm\\expert-system.pl' , read, Stream),
    readLines(Stream, _),
    close(Stream).

loadRules:-
    open('../knowledge/rulesEnfermedadesUno.txt' , read, Stream),
    readLines(Stream, _),
    close(Stream).

% initializeStatus:-
%     writeln('Starting initialization process'),
%     premise(Rule, PremiseNumber, _),
%     assert(status(Rule, PremiseNumber, empty)),
%     false.

initializeRuleStatus([]).

initializeRuleStatus([FirstRule|Left]):-
    findall(Premise, premise(FirstRule, Premise, _), Premises),
    initializePremiseStatus(FirstRule, Premises),
    initializeRuleStatus(Left).

initializePremiseStatus(_, []).

initializePremiseStatus(Rule, [FirstPremise|Left]):-
    assert(status(Rule, FirstPremise, empty)),
    initializePremiseStatus(Rule, Left).

clean:-
    retractall(premise(_, _, _)),
    retractall(rule(_)),
    retractall(conclusion(_, _)),
    retractall(status(_, _, _)),
    retractall(finalConclusion(_)).
    
    
readLines(Stream, []):-
    at_end_of_stream(Stream).

readLines(Stream, [Line|L]):-
    not(at_end_of_stream(Stream)),
    read(Stream, Line),
    assert(Line),
    readLines(Stream, L).

init:-
    clean,
    loadRules,
    getAllRules(Rules),
    initializeRuleStatus(Rules).


%%%%   General Utils   %%%%
isEqual(A,A). 

isNotEqual(A,B):- A\=B.

% There is such a control construct in ISO Prolog, called ->. You use it like this:

% ( condition -> then_clause ; else_clause )
% Here is an example that uses a chain of else-if-clauses:

% (   X < 0 ->
%     writeln('X is negative.  That's weird!  Failing now.'),
%     fail
% ;   X =:= 0 ->
%     writeln('X is zero.')
% ;   writeln('X is positive.')
% )


% Rules as premises of others rules
% 1. What if an evidence is a Rule?
%    The rule premises should be set to true (and all of the other premises of the other rules). DONE
% 2. The next question to ask should be ...
% 
% Rule status can be obtained by its probability percentage

% DOUBTS: hay alguna otra forma de recorrer nodos ademas de false y findall?
%       como funciona el truco de poner false?

%%%%    Algorithm   %%%%

processEvidence(Evidence):-
    (conclusion(Rule, Evidence) -> % DOUBT: is there any other way to do this??
        % writeln('It is a conclusion also'),
        setAllRulePremisesToTrue(Rule),
        findTruePremises(Evidence),

        searchForTrueRules % Too expensive
    ;
        % writeln('Finding premises'),
        findTruePremises(Evidence),

        searchForTrueRules % Too expensive
    ).

 searchForTrueRules:-
    % writeln("Starting search"),
    foreach(rule(Rule), evaluateRule(Rule)).

evaluateRule(Rule):-
    % write("Evaluating rule "), writeln(Rule),
    getProbability(Rule, Percentage),

    (Percentage =:= 1 -> 
        (finalConclusion(Rule) ->
            write('Concluido, el paciente tiene: '),
            conclusion(Rule, Conclusion),
            writeln(Conclusion)
        ;  
            writeln('Sub-rule concluded, setting premises to true'),
            conclusion(Rule, Evidence),
            findTruePremises(Evidence),
            writeln('Deleting sub-rule...'),
            retractall(premise(Rule, _, _)),
            retractall(rule(Rule)),
            retractall(conclusion(Rule, _)),
            retractall(status(Rule, _, _))
        )
    ; 
        % writeln('Is not 100%'),
        true
    ).  

    
setPremiseTrue(Rule, Premise):-
    retract(status(Rule, Premise, _)),
    assert(status(Rule, Premise, true)).
    % Instead of evaluate Rule...
    % getProbability(Rule, Percentage),
    % (Percentage =:= 1 -> 
    %     finalConclusion(Rule)).


findTruePremises(Evidence):-
    forall(premise(Rule, Premise, Evidence), setPremiseTrue(Rule, Premise)).
    % Before (false trick)
    % premise(Rule, Premise, Evidence),
    % retractall(status(Rule, Premise, _)),
    % assert(status(Rule, Premise, true)),
    % false.

setAllRulePremisesToTrue(Rule):- % Sets to true all premises
    forall(premise(Rule, _, PremiseContent), findTruePremises(PremiseContent)).
    % Before (false trick)
    % premise(Rule, Premise, _),
    % retractall(status(Rule, Premise, _)),
    % assert(status(Rule, Premise, true)),
    % false.


findAndDeleteFalseRules(Evidence):- % DOUBT: que tan bueno es borrar las reglas falsas
    premise(Rule, Premise, Evidence),
    retract(premise(Rule, _, _)),
    retract(rule(Rule)),
    retract(conclusion(Rule, _)),
    retract(status(Rule, Premise, _)),
    false.


getAllRules(Rules):-
    findall(Rule, rule(Rule), Rules).

getPremises(Rule, Premises):-
    findall(Premise, premise(Rule, Premise, _), Premises).

getPremisesLength(Rule, Size):-
    findall(Rule, premise(Rule, _, _), List),
    length(List, Size).

getTruePremisesLength(Rule, Size):-
    findall(Rule, status(Rule, _, true), List),
    length(List, Size).

getProbability(Rule, Percentage) :-
    rule(Rule),
    getTruePremisesLength(Rule, TrueSize),
    getPremisesLength(Rule, FullSize),
    Percentage is TrueSize / FullSize.

getProbabilities(List):-
    findall([Percentage, Rule], getProbability(Rule, Percentage), List).

getMostProbableRule(List, Rule):-
    sort(List, Sorted),
    reverse(Sorted, [[_, Rule] | _]) .

getUnconfirmedPremise(Rule, PremiseNumber):-
    findall([Number], status(Rule, Number, empty), [[PremiseNumber]|_]).


start:-
    write('>'),
    readln(Line),
    writeln(Line),
    processEvidence(Line),
   
    showMostProbableRule,
    ask.


ask:-
    getProbabilities(List),
    getMostProbableRule(List, MostProbableRule),
    getUnconfirmedPremise(MostProbableRule, PremiseNumber),
    premise(MostProbableRule, PremiseNumber, [PremiseContent]),
    
    % isThisPremiseAConclusion(PremiseContent)
    (conclusion(Rule, [PremiseContent]) -> % DOUBT: is there any other way to do this??
        getUnconfirmedPremise(Rule, Number),
        premise(Rule, Number, [Content]),

        %%% TODO TODO TODO Agregar recursividad para revisar si esta premisa no es a su vez otra sub-regla

        write('El paciente tiene '),
        write(Content),
        writeln('?')
    ;

        write('El paciente tiene '),
        write(PremiseContent),
        writeln('?')
    ).


answer(yes):-
    getProbabilities(List),
    getMostProbableRule(List, MostProbableRule),
    getUnconfirmedPremise(MostProbableRule, PremiseNumber),
    premise(MostProbableRule, PremiseNumber, PremiseContent),

    (conclusion(Rule, PremiseContent) ->
        write('Preguntando por sub-regla, num: '), writeln(Rule),
        getUnconfirmedPremise(Rule, Number),
        writeln(PremiseNumber),
        premise(Rule, Number, Content),

        %%% TODO TODO TODO Agregar recursividad para revisar si esta premisa no es a su vez otra sub-regla

        writeln('Finding true premises'),
        processEvidence(Content)
    ;
        writeln('Finding true premises'),
        processEvidence(PremiseContent)
    ).
    
    

answer(no):-
    getProbabilities(List),
    getMostProbableRule(List, MostProbableRule),
    getUnconfirmedPremise(MostProbableRule, PremiseNumber),
    premise(MostProbableRule, PremiseNumber, PremiseContent),

    % TODO: Al contestar no, además de eliminar las reglas que la tengan como premisa, 
    % checar si corresponde a una sub-regla, si es así, entonces también eliminar las 
    % reglas que la incluyan como parte de sus premisas
    
    % TAMBIEN RECURSIVO
    
    writeln('Retracting false rules'),
    findAndDeleteFalseRules(PremiseContent).


showProbabilities:-
    getProbabilities(List),
    writeln(List).

showMostProbableRule:-
    getProbabilities(List),
    getMostProbableRule(List, MostProbableRule),
    conclusion(MostProbableRule, Conclusion),
    write('The most probable rule is '),
    writeln(Conclusion).
