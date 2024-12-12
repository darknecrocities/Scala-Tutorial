% Prolog Tutorial Part 2: Intermediate Concepts
% This tutorial expands on Part 1 with more practical and intermediate-level Prolog concepts.

% ===== Section 12: Dynamic Predicates =====
% Dynamic predicates allow adding, modifying, or removing facts/rules at runtime.
% Declare a predicate as dynamic before modifying it:

:- dynamic fact/1.

% Adding facts dynamically:
add_fact(Fact) :- assertz(fact(Fact)).

% Removing facts dynamically:
remove_fact(Fact) :- retract(fact(Fact)).

% Example usage:
% ?- add_fact(hello).
% ?- fact(hello). % True, because we added it.
% ?- remove_fact(hello).
% ?- fact(hello). % False, because we removed it.

% ===== Section 13: Working with Strings =====
% Strings in Prolog are represented as atoms or lists of characters.

% Reverse a string represented as a list of characters:
reverse_string(String, Reversed) :- reverse(String, Reversed).

% Example query:
% ?- reverse_string([h, e, l, l, o], X). % X = [o, l, l, e, h].

% Check if a word is a palindrome:
palindrome(Word) :- reverse(Word, Word).

% Example query:
% ?- palindrome([r, a, d, a, r]). % True.

% ===== Section 14: Input and Output =====
% Reading and writing data in Prolog.

% Writing output to the console:
write_example :-
    write('Hello, Prolog!'), nl.

% Reading input from the user:
read_example :-
    write('Enter your name: '),
    read(Name),
    write('Hello, '), write(Name), nl.

% Example usage:
% ?- write_example.
% ?- read_example.

% ===== Section 15: DCGs (Definite Clause Grammars) =====
% DCGs simplify writing parsers and grammars.
% They are used for processing natural language or structured text.

% Define a simple grammar for recognizing a noun phrase:
np --> det, noun.
det --> [the].
noun --> [cat].
noun --> [dog].

% Example query:
% ?- phrase(np, [the, cat]). % True.
% ?- phrase(np, [the, bird]). % False, no rule for 'bird'.

% ===== Section 16: Meta-Programming =====
% Prolog allows manipulation of its own code through meta-predicates.

% Find all solutions to a query:
find_all_examples :-
    findall(X, likes(john, X), Results),
    write(Results), nl.

% Check if a predicate exists:
predicate_exists(Predicate/Arity) :-
    current_predicate(Predicate/Arity).

% Example queries:
% ?- find_all_examples.
% ?- predicate_exists(likes/2). % True if likes/2 is defined.

% ===== Section 17: Constraint Logic Programming =====
% Prolog can handle constraints using libraries like CLP(FD) for finite domains.
:- use_module(library(clpfd)).

% Define constraints for Sudoku:
sudoku(Puzzle, Solution) :-
    Puzzle = Solution,
    Solution ins 1..9,
    all_distinct(Solution).

% Example query:
% ?- sudoku([X, Y, Z], [1, 2, 3]).

% ===== Section 18: Knowledge Representation =====
% Use Prolog to represent complex knowledge systems.
% Example: Representing family relationships with more details.

% Define rules for extended relationships:
uncle(Uncle, Person) :-
    parent(Parent, Person), sibling(Parent, Uncle).

% Example query:
% ?- uncle(X, mary). % Find Mary’s uncles.

% ===== End of Part 2 =====