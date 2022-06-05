# Regex2NFA2Regex

## Regex format
To simplify the parsing process, my script uses a Prolog friendly term-based regex format:

* R+S = or(R,S)
* RS = and(R,S)
* R* = iter(R)

<br/>

<b>Regex example:</b>

`(0+10)* = iter(or(0, and(1,0)))`

<br/>

## λ-NFA format
NFA is represented by a graph, which is represented by a list of edges where each edge has following structure: 
`edge(FromNode, ToNode, Symbol)`. Node 0 is the input state.
Last node (with the highest number) is the final state


## Regex -> NFA
To convert Regex to NFA, use `regex2NFA(+Expression, -Result)` functor.

<br/>

<b>Regex -> λ-NFA example:</b>

```Prolog
?- regex2NFA(iter(and(0, 1)),NFA).
NFA = [edge(0, 1, lambda), edge(0, 4, lambda), edge(3, 4, lambda), edge(3, 1, lambda), edge(1, 2, 0), edge(2, 3, 1)].

```

<br/>

## λ-NFA -> Regex
Use `nfa2Regex(+NFA, +StartNode, +FinishNode, -RegexExpression)` functor to convert NFA to Regex.

<b> NFA preconditions: </b>
- NFA must be valid
- single input state
- single final state
- no unreachable states

<i>order of edges in a list is not relevant</i>


`nfa2Regex` uses a [ripping out](https://courses.engr.illinois.edu/cs374/fa2017/extra_notes/01_nfa_to_reg.pdf) method and can return multiple correct regular expressions. Many of those expressions might be duplicated. Be aware that my script shortens them a little bit but a proper shortening should be applied to the returned expressions.

<b>λ-NFA -> Regex example:</b>

```Prolog

?- nfa2Regex([edge(0,1,lambda),edge(0,3,lambda),edge(1,2,a),edge(3,4,x),edge(4,5,y),edge(2,6,lambda),edge(5,6,lambda)], 0, 6, Regex).
Regex = or(and(x,y),a) ;
Regex = or(a,and(x,y))

?- nfa2Regex([edge(0,1,lambda),edge(0,3,lambda),edge(1,3,a),edge(1,1,a)], 0,3,P).
P = or(and(iter(a), a), lambda)


```
