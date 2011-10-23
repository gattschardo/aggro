-module(aggro).

-export([new/1, shine/2, grow/1, to_list/1, to_tree/1, demo/0]).

% blarg
% -----

% a plant is represented like so:
% {BranchStore,{MinX,MaxX,MinY,MaxY},Rules}
% Rules are stored in a dict indexed by their syms, a rule is
% {Cost,Block,End,[Children]}

% each branch will take the form:
% {Symbol,CanGrow,Food,[Children]}
% e.g.: {$|,true,1,[{1,1},{1,-1}]}
% they are stored in two levels of dicts, first dict is indexed by Y
% coordinates, second level indexed by X (this is to make shine/2 more
% efficient).

% public
% ------

new(Rules) when is_list(Rules) ->
  {Sym,_,_,_,_}=hd(Rules),
  {add_branch(Sym,{0,0},dict:new()),{0,0,0,0},rules_to_dict(Rules)}.

shine(Strength,{BS,M,Rs}) ->
  {MinX,MaxX,MinY,MaxY}=M,
  ND=dict:new(),
  F=fun(Y,{R,SunDict}) ->
      F1=fun(X,{Sym,CG,F,Cs},{RD,SunD}) ->
          {_,Bl,_,_}=dict:fetch(Sym,Rs),
          Sun=dict:fetch(X,SunD),
          Get=Sun*Bl,
          Drop=Sun*(1-Bl),
          {dict:store(X,{Sym,CG,F+Get,Cs}),dict:store(X,Drop)}
      end,
      XDict=dict:fetch(Y,BS),
      {RXD,RSunD}=dict:fold(F1,{ND,SunDict},XDict),
      {dict:store(Y,RXD),RSunD}
  end,
  SD=sun_dict(MinX,MaxX,Strength),
  {R,_RSD}=lists:foldl(F,{ND,SD},lists:seq(MinY,MaxY)),
  {R,M,Rs}.

grow(P) ->
  let_grow({0,0},P).

to_list(_Plant) ->
  todo.

to_tree(_Plant) ->
  todo.

demo() ->
  C=1,B=0.5,E=$O,
  new([{$|,C,B,E,[{$_,{-1,1}},{$\\,{-1,1}},{$|,{0,1}},{$/,{1,1}},{$/,{1,1}}]},
      {$\\,C,B,E,[{$_,{-1,1}},{$\\,{-1,1}},{$|,{-1,1}},{$/,{0,1}}]},
      {$/,C,B,E,[{$\\,{0,1}},{$|,{1,1}},{$/,{1,1}},{$_,{1,1}}]},
      {$_,C,B,E,[{$_,{-1,0}},{$\\,{-1,0}},{$|,{-1,0}},{$|,{1,0}},{$/,{1,0}},{$_,{1,0}}]},
      {$O,0,B,E,[]}]).

% private
% -------

% O(log N), N = |Store|
add_branch(Sym,{X,Y},Store) ->
  YS=case dict:find(Y,Store) of
    {ok,YStore} ->
      YStore;
    error ->
      dict:new()
  end,
  NYS=dict:store(X,{Sym,true,0,[]},YS),
  dict:store(Y,NYS,Store).

% O(N), N = |Rules|
rules_to_dict(Rules) ->
  F=fun({S,C,B,E,Ch},A) ->
      dict:store(S,{C,B,E,Ch},A)
  end,
  lists:foldl(F,dict:new(),Rules).

sun_dict(Min,Max,Strength) ->
  F=fun(X,D) ->
      dict:store(X,Strength,D)
  end,
  lists:foldl(F,dict:new(),lists:seq(Min,Max)).

let_grow({Pos,{BS,_M,_Rs}=P) ->
  B={_S,_CG,_F,Cs}=get_branch(Pos,BS),
  lists:foldl(fun(Po,CP) -> let_grow(Po,CP) end,update(Pos,B,P),Cs).

update(_Pos,{_S,false,_F,[]},P) ->
  P;
update(Pos,{S,false,F,Cs},{BS,M,Rs}) ->
  NBS=distribute_food(F,Cs,BS),
  {put_branch(Pos,NBS,{S,false,0,Cs}),M,Rs};
update(Pos,{S,true,F,Cs},{BS,M,Rs}=P) ->
  {GO,Cont}=growth_options(Pos,F,Rs),
  case Cont of
    true ->
      case GO of
        [] ->
          P;
        Some ->
          grow_branch(Some)
      end;
    false ->
      grow_end.
  end.

% ...
get_branch({X,Y},BS) ->
  dict:fetch(X,dict:fetch(Y,BS)).

put_branch({X,Y},BS,B) ->
  dict:store(Y,dict:store(X,B,dict:fetch(Y,BS)),BS).

% vim: se ai sts=2 sw=2 et:
