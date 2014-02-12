#!/usr/bin/escript
%%! -pa ebin

description() ->
  "two values in result XML document".

expected() ->
  {error,bad_xml_structure}.

data() ->
  "<?xml version='1.0'?>"
  "<methodResponse>"
    "<params />"
  "</methodResponse>".

main(_) ->
  etap:plan(1),
  Result = try
    xmerlrpc_xml:parse(data(), [])
  catch
    T:E -> {trycatch,T,E}
  end,
  etap:is(Result, expected(), description()).
