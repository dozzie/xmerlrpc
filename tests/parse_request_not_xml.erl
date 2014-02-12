#!/usr/bin/escript
%%! -pa ebin

description() ->
  "not an XML document".

expected() ->
  {error,bad_xml}.

data() ->
  "some text".

main(_) ->
  etap:plan(1),
  Result = try
    xmerlrpc_xml:parse(data(), [])
  catch
    T:E -> {trycatch,T,E}
  end,
  etap:is(Result, expected(), description()).
