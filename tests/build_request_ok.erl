#!/usr/bin/escript
%%! -pa ebin

description() ->
  "build a request".

expected() ->
  {ok}.

data() ->
  "some text".

main(_) ->
  etap:plan(1),
  Result = try
    {ok, XML} = xmerlrpc_xml:request('func.name', data(), []),
    {ok}
  catch
    T:E -> {trycatch,T,E}
  end,
  etap:is(Result, expected(), description()).
