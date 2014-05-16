%%%---------------------------------------------------------------------------
%%% @doc
%%%   Usage examples of xmerlrpc on server side.
%%%
%%%   == Raw protocol ==
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
%%%   == inets ==
%%%
%%%   === starting httpd ===
%%%
%%%   ```
%%%   start_httpd() ->
%%%     % remember to start `inets' application
%%%     {ok, Config} = load_config(),
%%%     {ok, HttpdPid} = inets:start(httpd, Config),
%%%     {ok, HttpdPid}.
%%%
%%%   load_config() ->
%%%     % proc_spec() :: {Module :: atom(), Function :: atom()}
%%%     % ProcMap :: [{Name :: binary(), Proc :: proc_spec()}]
%%%     ProcMap = [
%%%       {<<"example">>, {example_handler, call}}
%%%     ],
%%%     DocumentRoot = "/var/www",
%%%     ServerRoot = "/var/lib/www",
%%%     Config = [
%%%       % typical setup for inets/httpd
%%%       {port, 1080}, {server_name, "localhost"},
%%%       {document_root, DocumentRoot}, {server_root, ServerRoot},
%%%       {directory_index, ["index.html"]}, % requires mod_alias
%%%
%%%       % httpd needs to load `mod_xmerlrpc'
%%%       {modules, [mod_xmerlrpc, mod_alias, mod_dir, mod_get, mod_log]},
%%%
%%%       % mod_xmerlrpc's config
%%%       {xmlrpc, {"/xmlrpc", ProcMap}},
%%%
%%%       % logging
%%%       {transfer_log, "/var/log/httpd/access.log"},
%%%       {error_log,    "/var/log/httpd/error.log"},
%%%       {log_format, combined},
%%%       {error_log_format, pretty}
%%%     ],
%%%     {ok, Config}.
%%%   '''
%%%
%%%   === example handler module ===
%%%
%%%   ```
%%%   -module(example).
%%%   -export([call/2]).
%%%
%%%   call(Args, Context) ->
%%%     Result = [],
%%%     {ok, Result}.
%%%   '''
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(examples_server).

%%% vim:nowrap
