#!/usr/bin/escript
%%! -pa ebin

description() ->
  "wrong tag in XML document".

expected() ->
  {error,bad_xml_structure}.

data() ->
  "<notXMLRPCRequest/>".

main(_) ->
  etap:plan(1),
  Result = try
    xmerlrpc_xml:parse(data(), [])
  catch
    T:E -> {trycatch,T,E}
  end,
  etap:is(Result, expected(), description()).
