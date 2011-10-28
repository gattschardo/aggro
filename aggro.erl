-module(aggro).

-export([new/1, shine/2, grow/1, to_list/1, to_tree/1, demo/0]).

%-compile(export_all).

% aggro
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
          {dict:store(X,{Sym,CG,F+Get,Cs},RD),dict:store(X,Drop,SunD)}
      end,
      XDict=dict:fetch(Y,BS),
      {RXD,RSunD}=dict:fold(F1,{ND,SunDict},XDict),
      {dict:store(Y,RXD,R),RSunD}
  end,
  SD=sun_dict(MinX,MaxX,Strength),
  {R,_RSD}=lists:foldl(F,{ND,SD},lists:seq(MaxY,MinY,-1)),
  {R,M,Rs}.

grow(P) ->
  let_grow({0,0},P).

to_list({BS,_M,_Rs}) ->
  F=fun(Y,YS,R) ->
      FI=fun(X,{S,_CG,_F,_Cs},RI) ->
          [{{X,Y},S}|RI]
      end,
      dict:fold(FI,R,YS)
  end,
  dict:fold(F,[],BS).

to_tree({BS,_M,_RS}) ->
  insert_children({0,0},BS).

demo() ->
  C=1,B=0.5,E=$O,
  new([{$|,C,B,E,[{$_,{-1,1}},{$\\,{-1,1}},{$|,{0,1}},{$/,{1,1}},{$/,{1,1}}]},
      {$\\,C,B,E,[{$_,{-1,1}},{$\\,{-1,1}},{$|,{-1,1}},{$/,{0,1}}]},
      {$/,C,B,E,[{$\\,{0,1}},{$|,{1,1}},{$/,{1,1}},{$_,{1,1}}]},
      {$_,C,B,E,[{$_,{-1,0}},{$\\,{-1,0}},{$|,{-1,0}},{$|,{1,0}},{$/,{1,0}},{$_,{1,0}}]},
      {$O,0,B,E,[]}]).

% private
% -------

insert_children(Pos,BS) ->
  {S,_CG,_F,Cs}=get_branch(Pos,BS),
  F=fun(P,R) ->
      [insert_children(P,BS)|R]
  end,
  {S,lists:foldl(F,[],Cs)}.

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

let_grow(Pos,{BS,_M,_Rs}=P) ->
  B={_S,_CG,_F,Cs}=get_branch(Pos,BS),
  lists:foldl(fun(Po,CP) -> let_grow(Po,CP) end,update(Pos,B,P),Cs).

update(_Pos,{_S,false,_F,[]},P) ->
  P;
update(Pos,{S,false,F,Cs},{BS,M,Rs}) ->
  NBS=distribute_food(F,Cs,BS),
  {put_branch(Pos,NBS,{S,false,0,Cs}),M,Rs};
update(Pos,{S,true,F,Cs},{BS,M,Rs}=P) ->
  {GO,CG}=growth_options(Pos,S,F,BS,Rs),
  case CG of
    true ->
      case GO of
        []   -> P;
        Some -> grow_branch(Some,Pos,BS,S,Cs,M,Rs)
      end;
    false ->
      case Cs of
        [] ->
          {grow_end(Pos,S,F,BS,Rs),M,Rs};
        _ -> 
          NB={S,false,F,Cs},
          % trigger food distribution by updating again:
          update(Pos,NB,{put_branch(Pos,BS,NB),M,Rs})
      end
  end.

distribute_food(Fo,Cs,IBS) ->
  Food=Fo/length(Cs),
  F=fun(C,BS) ->
      {S,CG,OF,CCs}=get_branch(C,BS),
      put_branch(C,BS,{S,CG,OF+Food,CCs})
  end,
  lists:foldl(F,IBS,Cs).

% finds all options for symbol S to grow from Pos. They are computed from given
% as a rule dict in Rs and the current amount of food F. If S can grow, it will
% return the list of options as coordinate/symbol/food tuple and true, if S
% could grow, but needs more food it will return {[],true}. Only if there is
% no more option for S to grow, it will return {[],false}.
growth_options({X,Y},S,F,BS,Rs) ->
  {Cost,_B,_E,Cs}=dict:fetch(S,Rs),
  Fn=fun({CS,{Cx,Cy}},{R,_CG}=A) ->
      NP={X+Cx,Y+Cy},
      case has_branch(NP,BS) of
        true -> A;
        false ->
          if
            Cost >  F -> {R,true};
            Cost =< F -> {[{NP,F-Cost,CS}|R],true}
          end
      end
  end,
  lists:foldl(Fn,{[],false},Cs).

% chooses one of the growth option in Opts and grows it into a new branch from
% Pos. Returns the plant with the new branch, possibly updating M.
grow_branch(Opts,Pos,BS,S,Cs,{MinX,MaxX,MinY,MaxY},Rs) ->
  L=length(Opts),
  {{Cx,Cy}=CPos,F,CS}=lists:nth(random:uniform(L),Opts),
  CG=if
    L == 1 -> false;
    L >  1 -> true
  end,
  BSp=put_branch(Pos,BS,{S,CG,F,[CPos|Cs]}),
  BSc=add_branch(CS,CPos,BSp),
  M={min(MinX,Cx),max(MaxX,Cx),min(MinY,Cy),max(MaxY,Cy)},
  {BSc,M,Rs}.

% grows the branch at position Pos into an end symbol, as specified by the rules
% Rs, with F food in the branch store BS. Returns the updated branch store.
grow_end(Pos,S,F,BS,Rs) ->
  {_C,_B,ES,_Cs}=dict:fetch(S,Rs),
  put_branch(Pos,BS,{ES,false,F,[]}).

% branch store helpers
% --------------------

% returns true is there is a branch present at {X,Y} in the branch store BS,
% false otherwise.
has_branch({X,Y},BS) ->
  case dict:find(Y,BS) of
    error -> false;
    {ok,YS} ->
      case dict:find(X,YS) of
        error -> false;
        {ok,_B} -> true
      end
  end.

% ...
get_branch({X,Y},BS) ->
  dict:fetch(X,dict:fetch(Y,BS)).

put_branch({X,Y},BS,B) ->
  dict:store(Y,dict:store(X,B,dict:fetch(Y,BS)),BS).

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

% vim: se ai sts=2 sw=2 et:
