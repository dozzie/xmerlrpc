%%%---------------------------------------------------------------------------
%%% @doc
%%%   Usage examples of xmerlrpc on server side.
%%%
%%%   == Processing raw protocol ==
%%%
%%%   Let's assume that function `do/3' is called to handle HTTP request,
%%%   its `Env' argument contains a dictionary with procedures to call,
%%%   and `example:call/3' finds and calls appropriate one with args of
%%%   `Args', returning `not_found' if no procedure for `ProcName' was found
%%%   and `{ok,Value}' on success.
%%%
%%%   ```
%%%   do(_Headers, Body, Env) ->
%%%     case xmerlrpc:parse_request(Body, []) of
%%%       {ok, request, {ProcName, Args}} ->
%%%         try example:call(ProcName, Args, Env) of
%%%           {ok, Result} ->
%%%             {ok, Reply} = xmerlrpc:result(Result, []),
%%%             {200, "text/xml", Reply};
%%%           not_found ->
%%%             {ok, Reply} = xmerlrpc:exception(1, "unknown procedure", []),
%%%             {200, "text/xml", Reply}
%%%         catch
%%%           error:Error ->
%%%             % if the procedure dies, exception is reported
%%%             {ok, Reply} = xmerlrpc:exception(2, example:to_string(Error), []),
%%%             {200, "text/xml", Reply}
%%%         end;
%%%       {error, Reason} ->
%%%         {400, "text/plain", example:to_string(Reason)}
%%%     end.
%%%   '''
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(examples_server).

%%% vim:nowrap
