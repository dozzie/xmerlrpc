%%%---------------------------------------------------------------------------
%%% @doc
%%%   Usage examples of xmerlrpc on client (caller) side.
%%%
%%%   == Standalone XML-RPC client ==
%%%
%%%   ```
%%%   URL = "http://server.example.com/RPC2",
%%%   case xmerlrpc:call(foo.bar, [...], [{url, URL}]) of
%%%     {ok, Value} ->
%%%       io:fwrite("Everything OK: ~p~n", [Value]);
%%%     {exception, {Code, Message}} ->
%%%       io:fwrite("Exception thrown: (code ~B)~n~s~n", [Code, Message]);
%%%     {error, Reason} ->
%%%       io:fwrite("Connection error: ~p~n", [Reason])
%%%   end.
%%%   '''
%%%
%%%   == Processing raw protocol (`httpc') ==
%%%
%%%   Note that this example, in contrast to the previous one, doesn't include
%%%   handling errors.
%%%
%%%   ```
%%%   URL = "http://server.example.com/RPC2",
%%%   {ok, ReqXML} = xmerlrpc:request(foo.bar, [...], []),
%%%   Request = {URL, [], "text/xml", iolist_to_binary(ReqXML)},
%%%   {ok, {{_,200,_}, _, Body}} = httpc:request(post, Request, [], []),
%%%   {ok, result, Result} = xmerlrpc:parse(Body, []).
%%%   '''
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(examples_client).
