% Prolog Tutorial: Beginner to Advanced
% This tutorial covers Prolog basics and advanced concepts step by step.
% Each section is commented with detailed explanations.

% ===== Section 1: Facts =====
% Facts represent basic truths about the world.
% Syntax: predicate(arguments).

likes(john, pizza).  % John likes pizza.
likes(mary, sushi).  % Mary likes sushi.
likes(susan, pizza). % Susan also likes pizza.

% ===== Section 2: Queries =====
% Queries are used to ask Prolog questions about the facts.
% Example queries (to be entered in the Prolog REPL):
% ?- likes(john, pizza). % True, because it's a fact.
% ?- likes(mary, pizza). % False, no such fact.

% ===== Section 3: Rules =====
% Rules define relationships between facts using logical implication (:-).

% A rule stating that someone likes food if it's popular:
likes(X, Y) :- popular(Y).

popular(pizza). % Pizza is popular.

% Example query:
% ?- likes(john, X). % What does John like?

% ===== Section 4: Variables =====
% Variables are placeholders starting with uppercase letters.
% They unify with values during queries.

% Who likes pizza?
% ?- likes(Who, pizza). % Will return john and susan.

% ===== Section 5: Recursion =====
% Prolog supports recursion to define rules that call themselves.

% Defining ancestor relationships:
parent(john, mary). % John is Mary's parent.
parent(mary, susan). % Mary is Susan's parent.

ancestor(X, Y) :- parent(X, Y). % Base case: a parent is an ancestor.
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y). % Recursive case.

% Example query:
% ?- ancestor(john, susan). % True, John is Susan's ancestor.

% ===== Section 6: Lists =====
% Lists are fundamental in Prolog. They are denoted by square brackets [].

% Facts about lists:
member(X, [X|_]). % X is the head of the list.
member(X, [_|Tail]) :- member(X, Tail). % X is in the tail of the list.

% Example queries:
% ?- member(3, [1, 2, 3, 4]). % True, 3 is in the list.
% ?- member(5, [1, 2, 3, 4]). % False, 5 is not in the list.

% ===== Section 7: Arithmetic =====
% Prolog supports arithmetic operations and comparisons.

% Define a rule to check if a number is even:
even(Number) :- 0 is Number mod 2.

% Example queries:
% ?- even(4). % True.
% ?- even(5). % False.

% Define a rule to calculate the sum of two numbers:
sum(A, B, Sum) :- Sum is A + B.

% Example query:
% ?- sum(3, 5, X). % X = 8.

% ===== Section 8: Backtracking =====
% Prolog explores multiple possibilities using backtracking.

% Define sibling relationships:
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

% Example query:
% ?- sibling(mary, X). % Prolog will find all siblings of Mary.

% ===== Section 9: Constraints =====
% Prolog allows constraint logic programming for solving specific problems.

% A simple constraint example:
greater_than(X, Y) :- X > Y.

% Example query:
% ?- greater_than(5, 3). % True.
% ?- greater_than(2, 4). % False.

% ===== Section 10: Advanced Example =====
% Solving a problem: Finding the maximum number in a list.

max_in_list([X], X). % Base case: the max of a single-element list is the element.
max_in_list([Head|Tail], Max) :-
    max_in_list(Tail, TailMax),
    Max is max(Head, TailMax).

% Example query:
% ?- max_in_list([3, 5, 2, 8, 6], Max). % Max = 8.

% ===== Section 11: Applications =====
% Practical applications of Prolog include:
% 1. Natural Language Processing
% 2. Expert Systems
% 3. Knowledge Representation
% 4. Logic Puzzles

% Example: Solving a logic puzzle
% Define the zebra puzzle:
% (Details omitted for brevity, but can be added for advanced users)

% ===== End of Tutorial =====
