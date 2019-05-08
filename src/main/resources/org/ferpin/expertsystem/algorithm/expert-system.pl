% Autor: Fernando Josué Pinedo Orta

:- dynamic rule/1.
:- dynamic premise/3.
:- dynamic conclusion/2.
:- dynamic status/3. % premises 
:- dynamic finalConclusion/1. % to distinguish sub-rules from rules

% DOUBT: si no tiene agitacion entonces no tiene ansiedad y si no tiene ansiedad entonces no tiene preocupacion???

:- dynamic premiseProbability/3.
:- dynamic premisesProbabilitiesSum/2.

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
    % initialize sum of premises probabilities
    assert(premisesProbabilitiesSum(FirstRule, 0)),
    initializePremiseStatus(FirstRule, Premises),
    initializeRuleStatus(Left).

initializePremiseStatus(_, []).

initializePremiseStatus(Rule, [FirstPremise|Left]):-
    assert(status(Rule, FirstPremise, empty)),
    % initialize premise probability
    assert(premiseProbability(Rule, FirstPremise, 0)),
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
    findall(RuleFound, premise(RuleFound, _, Evidence), Rules),
    deleteTheseRules(Rules).

deleteTheseRules([Rule | LeftRules]):-
    conclusion(Rule, Conclusion),

    retractall(premise(Rule, _, _)),
    retractall(rule(Rule)),
    retractall(conclusion(Rule, _)),
    retractall(status(Rule, _, _)),

    %detect if the conclusion of this premise is a premise of an outer rule/conclusion
    (premise(_, _, Conclusion) ->
        write('This conclusion is a premise also: '), writeln(Conclusion),
        %volver a buscar
        findAndDeleteFalseRules(Conclusion)
    ;
        write('Delete done')
    ),

    deleteTheseRules(LeftRules).

deleteTheseRules([]).

getAllRules(Rules):-
    findall(Rule, rule(Rule), Rules).

getPremises(Rule, Premises):-
    findall(Premise, premise(Rule, Premise, _), Premises).

getPremisesLength(Rule, Size):-
    findall(Rule, premise(Rule, _, _), List),
    length(List, Size).

%%% Funcional!

getTruePremisesLength(Rule, Size):-
    findall(Premise, status(Rule, Premise, true), List),
    % TODO: check if premise is a conclusion
    length(List, Size).


%%% No funcional: Intento con recursividad
% Probar con predicado DINAMICO 

% getTruePremisesLength(Rule, Size):-
%     findall(Premise, status(Rule, Premise, _), List),
%     evaluatePremises(Rule, List, 0, Size).

%         % TODO: check if premise is a conclusion
%     % length(List, Size).

% evaluatePremises(OuterRule, [FirstPremise | PremisesLeft], InitialValue, TruePremises):-
%     premise(OuterRule, FirstPremise, PremiseContent),

%     (conclusion(InnerRule, PremiseContent)->
%         getProbability(InnerRule, Percentage),
%         Temporal is InitialValue + Percentage,
%         evaluatePremises(OuterRule, PremisesLeft, Temporal, Temporal)
%     ;
%         (status(OuterRule, FirstPremise, true) ->
%             Temporal is InitialValue + 1,
%             evaluatePremises(OuterRule, PremisesLeft, Temporal, Temporal)
%         ;
%             evaluatePremises(OuterRule, PremisesLeft, InitialValue, InitialValue)
%         )
%     ).

% evaluatePremises(_, [], _).


getProbability(Rule, Percentage) :-
    rule(Rule),
    getTruePremisesLength(Rule, TrueSize),
    getPremisesLength(Rule, FullSize),
    Percentage is TrueSize / FullSize.

%%% Intento 3
getRuleProbability(Rule, Probability):-
    
    write('Getting probability of rule '), writeln(Rule),
    findall(Premise, premise(Rule, Premise, _), Premises),
    forall(member(Member, Premises), getPremiseProbability(Rule, Member)),
    
    sumPremisesProbabilities(Rule),
    premisesProbabilitiesSum(Rule, ProbabilitiesSum),

    length(Premises, Size),
    write('The sum of premises probabilities is '), writeln(ProbabilitiesSum),
    write('The size is '), writeln(Size),
    Probability is ProbabilitiesSum / Size.

getPremiseProbability(Rule, Premise):-
    write('Getting probability of premise '), write(Rule), write(', '), writeln(Premise),
    (status(Rule, Premise, true) ->
        retract(premiseProbability(Rule, Premise, _)),
        assert(premiseProbability(Rule, Premise, 1))
    ;
        premise(Rule, Premise, PremiseContent),
        (conclusion(InnerRule, PremiseContent) ->
            getRuleProbability(InnerRule, InnerRuleProbability),

            retract(premiseProbability(Rule, Premise, _)),
            assert(premiseProbability(Rule, Premise, InnerRuleProbability))
        ;
            true
        )
    ).

sumPremisesProbabilities(Rule):-
    write('Summing premises'),
    premiseProbability(Rule, Premise, PremiseProbability),
    premisesProbabilitiesSum(Rule, Sum),
    Temporal is Sum + PremiseProbability,
    retract(premiseProbability(Rule, Premise, _)),
    retract(premisesProbabilitiesSum(Rule, _)),
    assert(premisesProbabilitiesSum(Rule, Temporal)),
    write('The sum of premises probabilities till the moment is '), writeln(Temporal).








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
    premise(MostProbableRule, PremiseNumber, PremiseContent),
    
    getInnerUnconfirmedPremise(PremiseContent, InnerPremise),

    write('El paciente tiene '),
    write(InnerPremise),
    writeln('?').


getInnerUnconfirmedPremise(OuterPremise, InnerPremise):-
    (conclusion(Rule, OuterPremise) ->
        write('This premise is a conclusion also: '), writeln(OuterPremise),
        getUnconfirmedPremise(Rule, Number),
        premise(Rule, Number, Content),

        getInnerUnconfirmedPremise(Content, InnerPremise)
    ;
        write('Inner premise found: '), writeln(OuterPremise),
        premise(RuleNumber, PremiseNumber, OuterPremise),
        premise(RuleNumber, PremiseNumber, InnerPremise)
    ).

answer(yes):-
    getProbabilities(List),
    getMostProbableRule(List, MostProbableRule),
    getUnconfirmedPremise(MostProbableRule, PremiseNumber),
    premise(MostProbableRule, PremiseNumber, PremiseContent),


    getInnerUnconfirmedPremise(PremiseContent, InnerPremise),
    processEvidence(InnerPremise).
    
    

answer(no):-
    getProbabilities(List),
    getMostProbableRule(List, MostProbableRule),
    getUnconfirmedPremise(MostProbableRule, PremiseNumber),
    premise(MostProbableRule, PremiseNumber, PremiseContent),
    
    writeln('Retracting false rules'),

    getInnerUnconfirmedPremise(PremiseContent, InnerPremise),
    findAndDeleteFalseRules(InnerPremise).


showProbabilities:-
    getProbabilities(List),
    writeln(List).

showMostProbableRule:-
    getProbabilities(List),
    getMostProbableRule(List, MostProbableRule),
    conclusion(MostProbableRule, Conclusion),
    write('The most probable rule is '),
    writeln(Conclusion).
