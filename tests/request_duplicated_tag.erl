#!/usr/bin/escript
%%! -pa ebin

description() ->
  "duplicated tag in XML document".

expected() ->
  {error,bad_xml_structure}.

data() ->
  "<?xml version='1.0'?>"
  "<methodCall>"
    "<methodName>func.name</methodName>"
    "<params/>"
    "<params/>"
  "</methodCall>".

main(_) ->
  etap:plan(1),
  Result = try
    xmerlrpc_xml:parse(data(), [])
  catch
    T:E -> {trycatch,T,E}
  end,
  etap:is(Result, expected(), description()).
