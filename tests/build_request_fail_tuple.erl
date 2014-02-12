#!/usr/bin/escript
%%! -pa ebin

description() ->
  "fail to build a request - tuple".

expected() ->
  {error,badarg}.

data() ->
  {fail}.

main(_) ->
  etap:plan(1),
  Result = try
    case xmerlrpc_xml:request('func.name', data(), []) of
      {ok, XML} -> {ok};
      {error, Reason} -> {error, Reason}
    end
  catch
    T:E -> {trycatch,T,E}
  end,
  etap:is(Result, expected(), description()).
