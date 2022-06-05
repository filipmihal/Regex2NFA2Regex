
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
    regex2NFARecursive(Expression, 0, _, [], Result), !.

findInsOuts(Node, [], AccIn, AccOut, Node-AccIn-AccOut) :- !.
findInsOuts(Node, [edge(From, To, _)|RestEdges], AccIn, AccOut, Result) :-
    From = Node, To \= Node, findInsOuts(Node, RestEdges, AccIn, [To|AccOut], Result), !.
findInsOuts(Node, [edge(From, To, _)|RestEdges], AccIn, AccOut, Result) :-
    From \= Node, To = Node, findInsOuts(Node, RestEdges, [From|AccIn], AccOut, Result), !.
findInsOuts(Node, [_|RestEdges], AccIn, AccOut, Result) :-
    findInsOuts(Node, RestEdges, AccIn, AccOut, Result).

containsNode(Node, edge(_, Node,_)) :- !.
containsNode(Node, edge(Node, _,_)).

isEdgeInPairs(Pairs, edge(From, To, _)) :-
    select((From, To), Pairs, _).

deleteEdges(Node, Pairs, Edges, NewEdges) :-
    exclude(containsNode(Node), Edges, ExEdges),
    exclude(isEdgeInPairs(Pairs), ExEdges, NewEdges).

cutNode(Edges, StartNode, _, Result) :-
    select(edge(From, _, _), Edges, _), From \== StartNode,
    findInsOuts(From, Edges, [], [], Node-In-Out),
    constructNewEdges(Edges, Node-In-Out, NewEdges, Pairs),
    deleteEdges(Node, Pairs, Edges, StrippedEdges),
    append(StrippedEdges, NewEdges, Result).

% nfa2Regex(+NFA, +StartNode, +FinishNode, -RegexExpression).
nfa2Regex([edge(S,F,Exp)], S, F, Exp).
nfa2Regex(Edges,S,F,Exp) :-
    cutNode(Edges,S,F,NewEdges),
    nfa2Regex(NewEdges, S,F,Exp).

% NFA preconditions:
% valid NFA
% single input state
% single final state
% no unreachable states
% nfa2Regex([edge(S,E,Regex)], S, E, Regex).
% the order of edges in a list should not matter

eachPairRev(_, [], []):- !.
eachPairRev(Node, [A|Rest], [(Node,A)|Pairs]) :-
    eachPairRev(Node, Rest, Pairs).
        
eachPair([], _, []).
eachPair([A|Rest], List, Pairs) :-
    eachPairRev(A,List, APairs), eachPair(Rest, List, RestPairs), append(APairs, RestPairs, Pairs).

constructNewEdges(Edges, Node-Ins-Outs,NewEdges, Pairs):-
    (select(edge(Node, Node, LoopExp), Edges, _); LoopExp = nil),!,
    eachPair(Ins, Outs, Pairs), maplist(newRegexEdge(Node, LoopExp, Edges), Pairs, NewEdges).

newRegex(FromExp, ToExp, nil, nil, Result):-
    beautifyAnd(FromExp, ToExp, Result),!.
newRegex(FromExp, ToExp, nil, ExistingExp, Result):-
beautifyAnd(FromExp, ToExp, AndResult),
    Result = or(AndResult,ExistingExp), !.
newRegex(FromExp, ToExp, LoopExp, nil, Result):-
    beautifyIter(LoopExp, Iter),
    beautifyAnd(Iter, ToExp, AndRes),
    beautifyAnd(FromExp, AndRes, Result), !.
newRegex(FromExp, ToExp, LoopExp, ExistingExp, or(AndRes2, ExistingExp)):-
    beautifyIter(LoopExp, Iter),
    beautifyAnd(Iter, ToExp, AndRes),
    beautifyAnd(FromExp, AndRes, AndRes2), !.


newRegexEdge(Node, LoopExp, Edges, (From, To), edge(From, To, NewExp)) :-
    select(edge(From, Node, FromExp), Edges, NewEdges), 
    select(edge(Node, To, ToExp), NewEdges, Q),
    (select(edge(From, To,ExistingExp), Q, _); ExistingExp = nil),!,
    newRegex(FromExp, ToExp,LoopExp, ExistingExp, NewExp).

beautifyAnd(lambda, lambda, lambda) :- !.
beautifyAnd(lambda, Exp, Exp) :- !.
beautifyAnd(Exp, lambda, Exp) :- !.
beautifyAnd(Exp,Exp2,and(Exp, Exp2)).

beautifyIter(lambda, lambda) :- !.
beautifyIter(A, iter(A)).