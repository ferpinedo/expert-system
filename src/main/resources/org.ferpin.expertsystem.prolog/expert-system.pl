
% :- dynamic rule/3. % las condiciones y las conclusiones de la rule
:- dynamic rule/1.
:- dynamic premise/3.
:- dynamic conclusion/2.
:- dynamic status/2. % rules
:- dynamic status/3. % premises  



loadRules:- 
    open('./rulesEnfermedadesUno.txt' , read, Stream),
    readLines(Stream, _),
    close(Stream).

readLines(Stream, []):- 
    at_end_of_stream(Stream).

readLines(Stream, [Line|L]):-
    not(at_end_of_stream(Stream)),
    read(Stream, Line),
    assert(Line),
    readLines(Stream, L).


updatePremises(Evidence):- 
    premise(Rule, Premise, Evidence),
    assert(status(Rule, Premise, true)).

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
    
    

start:- 
    write('>'),
    readln(Line),
    %writeln(Line),
    updatePremises(Line),
    getProbabilities(List),   
    getMostProbableRule(List ,MostProbableRule),
    conclusion(MostProbableRule, Conclusion),
    write('The most probable is '),
    write(Conclusion).
    

               