-module(a_shell).

-export([start/0]).

-define(PROMPT,"").
-define(SUN,2).

start() ->
  shell(?SUN,[],[aggro:demo()]).

shell(Sun,[Last|_]=Past,[]) ->
  shell(Sun,Past,[season(Sun,Last)]);
shell(Sun,Past,[Now|Future]=F) ->
  Cmd=io:get_line(?PROMPT),
  case hd(Cmd) of
    $p -> % print
      print(aggro:to_list(Now)),
      shell(Sun,Past,F);
    $k -> % next
      if
        Past == [] ->
          io:put_chars(standard_error,"Cannot go past generation 0\n"),
          shell(Sun,Past,F);
        Past /= [] ->
          shell(Sun,tl(Past),[hd(Past)|F])
      end;
    $j -> % previous
      shell(Sun,[Now|Past],Future);
    $q -> % quit
      Now;
    $n -> % new
      start();
    _Any ->
      io:put_chars(standard_error,"unknown input, use pkjq\n"),
      shell(Sun,Past,F)
  end.

season(Sun,Plant) ->
  aggro:grow(aggro:shine(Sun,Plant)).

print(L) ->
  io:format("~w~n",[length(L)]),
  F=fun({{X,Y},C}) ->
      io:format("~w ~w ~c~n",[X,Y,C])
  end,
  lists:foreach(F,L).

% vim: se ai sts=2 sw=2 et:
