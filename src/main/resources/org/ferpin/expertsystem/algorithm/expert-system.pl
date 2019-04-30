% Autor: Fernando Josué Pinedo Orta


% :- dynamic rule/3. % las condiciones y las conclusiones de la rule
:- dynamic rule/1.
:- dynamic premise/3.
:- dynamic conclusion/2.
:- dynamic status/3. % premises  

%%% Read rules

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
    retractall(status(_, _, _)).
    
    
readLines(Stream, []):-
    at_end_of_stream(Stream).

readLines(Stream, [Line|L]):-
    not(at_end_of_stream(Stream)),
    read(Stream, Line),
    assert(Line),
    readLines(Stream, L).

initialize:-
    clean,
    loadRules,
    getAllRules(Rules),
    initializeRuleStatus(Rules).

%%% Utils
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
%    The rule premises should be set to true (and all of the other premises of the other rules).
% 2. The next question to ask should be ...
% 
% Rule status can be obtained by its probability percentage

% TODO: dudas a resolver: hay alguna otra forma de recorrer nodos ademas de false y findall?
%       como funciona el truco de poner false?

%%%% Algorithm

processEvidence(Evidence):-
    (conclusion(Rule, Evidence) -> % TODO: is there any other way to do this??
        writeln('It is a conclusion also'),
        setAllRulePremisesToTrue(Rule),
        findTruePremises(Evidence),

        writeln('Deleting sub-rule...'),
        retractall(premise(Rule, _, _)),
        retractall(rule(Rule)),
        retractall(conclusion(Rule, _)),
        retractall(status(Rule, _, _)),

        searchForTrueRules % Too expensive
    ;
        writeln('Finding premises'),
        findTruePremises(Evidence),

        searchForTrueRules % Too expensive
    ).

 searchForTrueRules:- % TODO TODO TODO
    .   
    

findTruePremises(Evidence):-
    forall(premise(Rule, Premise, Evidence), setPremiseTrue(Rule, Premise)).
    % Before (false trick)
    % premise(Rule, Premise, Evidence),
    % retractall(status(Rule, Premise, _)),
    % assert(status(Rule, Premise, true)),
    % false.

setPremiseTrue(Rule, Premise):-
    retract(status(Rule, Premise, _)),
    assert(status(Rule, Premise, true)).

setAllRulePremisesToTrue(Rule):- % Sets to true all premises
    forall(premise(Rule, _, PremiseContent), findTruePremises(PremiseContent)).
    % Before (false trick)
    % premise(Rule, Premise, _),
    % retractall(status(Rule, Premise, _)),
    % assert(status(Rule, Premise, true)),
    % false.



findAndDeleteFalseRules(Evidence):- % TODO: que tan bueno es borrar las reglas falsas
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
    (conclusion(Rule, [PremiseContent]) -> % TODO: is there any other way to do this??
        getUnconfirmedPremise(Rule, PremiseNumber),
        premise(Rule, PremiseNumber, [PremiseContent]),

        write('El paciente tiene '),
        write(PremiseContent),
        writeln('?')
    ;

        write('El paciente tiene '),
        write(PremiseContent),
        writeln('?')
    ).

    


% answer(Answer):-
%     getProbabilities(List),
%     getMostProbableRule(List, MostProbableRule),
%     getUnconfirmedPremise(MostProbableRule, PremiseNumber),
%     premise(MostProbableRule, PremiseNumber, PremiseContent),
%     (   Answer = yes ->
%         writeln('Finding true premises'),
%         processEvidence(PremiseContent)

%     ;   Answer = no -> 
%         writeln('Retracting false rules'),
%         findAndDeleteFalseRules(PremiseContent)

%     ;   writeln('Type only yes/no')
%     ).

answer(yes):-
    getProbabilities(List),
    getMostProbableRule(List, MostProbableRule),
    getUnconfirmedPremise(MostProbableRule, PremiseNumber),
    premise(MostProbableRule, PremiseNumber, PremiseContent),
    
    writeln('Finding true premises'),
    processEvidence(PremiseContent).

answer(no):-
    getProbabilities(List),
    getMostProbableRule(List, MostProbableRule),
    getUnconfirmedPremise(MostProbableRule, PremiseNumber),
    premise(MostProbableRule, PremiseNumber, PremiseContent),
    
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
