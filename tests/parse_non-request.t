#!/usr/bin/estap

-test("not an XML document").
test1() ->
  Data = "some text",
  {error,bad_xml} == xmerlrpc_xml:parse(Data).

-test("syntax error in XML document").
test2() ->
  Data = "<invalid> error < </invalid>",
  {error,bad_xml} == xmerlrpc_xml:parse(Data).

-test("unclosed tag").
test3() ->
  Data = "<unclosedTag>",
  {error,bad_xml} == xmerlrpc_xml:parse(Data).

-test("not an XML-RPC message").
test4() ->
  Data = "<notXMLRPCRequest/>",
  {error,bad_xml_structure} == xmerlrpc_xml:parse(Data).

%% vim:ft=erlang
