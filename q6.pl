% Signature: sub(Sublist, List)/2
% Purpose: All elements in Sublist appear in List in the same order.

% Precondition: List is fully instantiated (queries do not include variables in their first argument).

% Example:
% ?- sub(X, [1, 2, 3]).

% X = [1, 2, 3];

% X = [1, 2];

% X = [1, 3];
% X = [2, 3];

% X = [1];

% X = [2];

% X = [3];

% X = [];

% false

%the true cases:
%1- subList is null

%empty sub=true
sub([],_).
%if sub.head==list.head ->rec call with tails
sub([H|SubTail],[H|ListTail]) :- sub(SubTail,ListTail).
%if sub.tail!=list.tail ->call rec call while sub is the same, list.tail
sub([H|SubTail],[_|ListTail]) :- append(_,[H|F],ListTail),sub(SubTail,F).


