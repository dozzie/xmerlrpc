#!/usr/bin/estap

-test("build a request").
test1() ->
  {ok, _XML} = xmerlrpc_xml:request('func.name', "some text", []).

-test("fail to build a request - int as a hash key").
test2() ->
  {error,badarg} == xmerlrpc_xml:request('func.name', [{10, true}], []).

-test("fail to build a request - tuple").
test3() ->
  {error,badarg} == xmerlrpc_xml:request('func.name', {data}, []).

-test("fail to build a request - two tuples in hash").
test4() ->
  {error,badarg} == xmerlrpc_xml:request('func.name', [{}, {}], []).

%% vim:ft=erlang
