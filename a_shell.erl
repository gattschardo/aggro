-module(a_shell).

-export([start/0]).

-define(PROMPT,"").
-define(SUN,2).

start() ->
  new(demo).

new(demo) ->
  shell(demo,?SUN,[],[aggro:demo()]);
new(Rules) ->
  shell(Rules,?SUN,[],[aggro:new(Rules)]).

shell(Rs,Sun,[Last|_]=Past,[]) ->
  shell(Rs,Sun,Past,[season(Sun,Last)]);
shell(Rs,Sun,Past,[Now|Future]=F) ->
  Cmd=io:get_line(?PROMPT),
  case hd(Cmd) of
    $p -> % print
      print(aggro:to_list(Now)),
      shell(Rs,Sun,Past,F);
    $k -> % next
      if
        Past == [] ->
          io:put_chars(standard_error,"cannot go past generation 0\n"),
          shell(Rs,Sun,Past,F);
        Past /= [] ->
          shell(Rs,Sun,tl(Past),[hd(Past)|F])
      end;
    $j -> % previous
      shell(Rs,Sun,[Now|Past],Future);
    $q -> % quit
      Now;
    $n -> % new
      new(Rs);
    $r -> % read rules
      Ts=string:tokens(Cmd," \n"),
      case length(Ts) of
        2 ->
          case file:consult(RF=lists:nth(2,Ts)) of
            {ok,Rules} ->
              case aggro:check_rules(Rules) of
                ok ->
                  new(Rules);
                {error,E} ->
                  io:format(standard_error,
                    "error in rules file `~s': ~w~n",[RF,E]),
                  shell(Rs,Sun,Past,F)
              end;
            _Error ->
              io:format(standard_error,
                "cannot read rules from file `~s'~n",[RF]),
              shell(Rs,Sun,Past,F)
          end;
        _Any ->
          io:format(standard_error,
            "invalid command `~s', use `r filename'~n",[Cmd]),
          shell(Rs,Sun,Past,F)
      end;
    _Any ->
      io:put_chars(standard_error,"unknown input, use pkjqnr\n"),
      shell(Rs,Sun,Past,F)
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
