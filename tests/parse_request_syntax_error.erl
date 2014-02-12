#!/usr/bin/escript
%%! -pa ebin

description() ->
  "syntax error in XML document".

expected() ->
  {error,bad_xml}.

data() ->
  "<invalid> error < </invalid>".

main(_) ->
  etap:plan(1),
  Result = try
    xmerlrpc_xml:parse(data(), [])
  catch
    T:E -> {trycatch,T,E}
  end,
  etap:is(Result, expected(), description()).
