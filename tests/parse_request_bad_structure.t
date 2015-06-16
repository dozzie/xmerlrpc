#!/usr/bin/estap

-test("duplicated tag in XML document").
test1() ->
  {ok, XML} = file:read_file(estap:test_dir("data/request_duplicated_tag.xml")),
  {error,bad_xml_structure} == xmerlrpc_xml:parse(XML, []).

-test("duplicated tag in XML document").
test2() ->
  {ok, XML} = file:read_file(estap:test_dir("data/request_duplicated_tag2.xml")),
  {error,bad_xml_structure} == xmerlrpc_xml:parse(XML, []).

-test("excessive tag in XML document").
test3() ->
  {ok, XML} = file:read_file(estap:test_dir("data/request_excessive_tag.xml")),
  {error,bad_xml_structure} == xmerlrpc_xml:parse(XML, []).

-test("excessive tag in XML document").
test4() ->
  {ok, XML} = file:read_file(estap:test_dir("data/request_excessive_tag2.xml")),
  {error,bad_xml_structure} == xmerlrpc_xml:parse(XML, []).

-test("missing <params> tag in XML document").
test5() ->
  {ok, XML} = file:read_file(estap:test_dir("data/request_missing_params_tag.xml")),
  {error,bad_xml_structure} == xmerlrpc_xml:parse(XML, []).

-test("no result in values").
test6() ->
  {ok, XML} = file:read_file(estap:test_dir("data/result_no_values.xml")),
  {error,bad_xml_structure} == xmerlrpc_xml:parse(XML, []).

-test("two values in result XML document").
test7() ->
  {ok, XML} = file:read_file(estap:test_dir("data/result_two_values.xml")),
  {error,bad_xml_structure} == xmerlrpc_xml:parse(XML, []).

%% vim:ft=erlang
