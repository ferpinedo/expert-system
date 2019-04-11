
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

initialize:-
    premise(Rule, PremiseNumber, _),
    assert(status(Rule, PremiseNumber, empty)),
    false.

restart:-
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

demo:-
    restart,
    loadRules,
    initialize.

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


%%%% Algorithm

findTruePremises(Evidence):-
    premise(Rule, Premise, Evidence),
    retract(status(Rule, Premise, _)),
    assert(status(Rule, Premise, true)),
    false.

findAndDeleteFalseRules(Evidence):- % que tan bueno es borrar las reglas falsas
    premise(Rule, Premise, Evidence),
    retract(premise(Rule, _, _)),
    retract(rule(Rule)),
    retract(conclusion(Rule, _)),
    retract(status(Rule, Premise, _)),
    false.

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
    findTruePremises(Line),
   
    getProbabilities(List),
    getMostProbableRule(List, MostProbableRule),

    conclusion(MostProbableRule, Conclusion),
    write('The most probable is '),
    writeln(Conclusion),


    getUnconfirmedPremise(MostProbableRule, PremiseNumber),
    premise(MostProbableRule, PremiseNumber, PremiseContent),
    write(PremiseContent),
    writeln('?').
    



ask:-
    getProbabilities(List),
    getMostProbableRule(List, MostProbableRule),
    getUnconfirmedPremise(MostProbableRule, PremiseNumber),
    premise(MostProbableRule, PremiseNumber, [PremiseContent]),
    write('El paciente tiene '),
    write(PremiseContent),
    writeln('?').


% answer(Answer):-
%     getProbabilities(List),
%     getMostProbableRule(List, MostProbableRule),
%     getUnconfirmedPremise(MostProbableRule, PremiseNumber),
%     premise(MostProbableRule, PremiseNumber, PremiseContent),
%     (   Answer = yes ->
%         writeln('Finding true premises'),
%         findTruePremises(PremiseContent)

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
    findTruePremises(PremiseContent).

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