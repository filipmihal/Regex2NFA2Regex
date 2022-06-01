% TODO: Fix non-determinism for regex2NFA

% REGEX format in Prolog
% R+S = or(R,S)
% RS = and(R,S)
% R* = iter(R)

% Regex example
% (0+10)* = iter(or(0, and(1,0)))

% λ-NFA format in Prolog
% graph represented by a list of edges where each edge has following structure: 
% edge(FromNode, ToNode, Symbol)
% Last node (with the hgihest number) is the final state

% λ-NFA example
% iter(and(0, 1))
% [edge(0, 1, lambda), edge(0, 4, lambda), edge(3, 4, lambda), edge(3, 1, lambda), edge(1, 2, 0), edge(2, 3, 1)]


% regex2NFARecursive(+Expression, +PreviousNode, +-NewNode, +Accumulator, -Result)
regex2NFARecursive(and(Expr1, Expr2), PrevNode, NewNode, Acc, Result) :-
    regex2NFARecursive(Expr1, PrevNode, NewNodeMid, [], Result1),
    regex2NFARecursive(Expr2, NewNodeMid, NewNode, [], Result2),
    append(Result1, Result2, Result3),
    append(Result3, Acc, Result).

regex2NFARecursive(or(Expr1, Expr2), PrevNode, NewNode, Acc, Result) :-
    EnteringNode1 is PrevNode + 1,
    regex2NFARecursive(Expr1, EnteringNode1, NewEnd1, [], Result1),
    EnteringNode2 is NewEnd1 + 1,
    regex2NFARecursive(Expr2, EnteringNode2, NewEnd2, [], Result2),
    append(Acc, [edge(PrevNode, EnteringNode1, lambda), edge(PrevNode, EnteringNode2, lambda)], AccStart),
    NewNode is NewEnd2 + 1,
    append(Result1, Result2, Body),
    append(AccStart, Body, AccEnd),
    append(AccEnd, [edge(NewEnd1, NewNode, lambda), edge(NewEnd2, NewNode, lambda)], Result).

    
regex2NFARecursive(iter(Expr), PrevNode, NewNode, Acc, Result) :-
    EnteringNode is PrevNode + 1,
    regex2NFARecursive(Expr, EnteringNode, EndIterNode, [], IterResult),
    NewNode is EndIterNode + 1,
    append(Acc,[edge(PrevNode, EnteringNode, lambda),
            edge(PrevNode, NewNode, lambda),
            edge(EndIterNode, NewNode, lambda),
            edge(EndIterNode, EnteringNode, lambda)], AccLambdas),
    append(AccLambdas, IterResult, Result).  
 
regex2NFARecursive(Symbol, PrevNode, NewNode, Acc, Result) :-
    NewNode is PrevNode + 1, append(Acc, [edge(PrevNode, NewNode, Symbol)], Result).

% regex2NFA(+Expression, -Result) 
regex2NFA(Expression, Result) :-
    regex2NFARecursive(Expression, 0, _, [], Result).

nfa2Regex([edge(_,_,Regex)], Regex).
% in list, the order does not matter

getPairs(List1, List2, Pairs) :-
    findall((A,B), (member(A, List1), member(B, List2)), Pairs).


% DS from which I can remove 1st level elements, edit and add 2nd level elements
% ripOutState(INs, OUTs, SelfEdge, Result) :-
%         e(From, _, Symbol1) 