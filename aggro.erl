-module(aggro).

-export([new/1, shine/2, grow/1, to_list/1, to_tree/1, demo/0]).

% public
new(Rules) when is_list(Rules) ->
	{Sym,_,_,_,_}=hd(Rules),
	{add_branch(Sym,{0,0},dict:new()),Rules}.

shine(_,_) ->
	todo.

grow(_) ->
	todo.

to_list(_) ->
	todo.

to_tree(_) ->
	todo.

demo() ->
	todo.

% private
add_branch(_,_,_) ->
	todo.
