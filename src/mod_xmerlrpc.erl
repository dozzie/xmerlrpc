%%%---------------------------------------------------------------------------
%%% @doc
%%%   `inets'/`httpd' request handler module.
%%%
%%%   For example of use, see {@link examples_server}.
%%%
%%%   == Handler interface ==
%%%
%%%   <b>TODO</b>
%%%
%%%   @todo `store({Option, Value}, Config) -> {ok, {Option, NewValue}}'
%%%   @todo `remove(ConfigDB) -> ok'
%%% @end
%%%---------------------------------------------------------------------------

-module(mod_xmerlrpc).

%%% enable qlc parse transform
-include_lib("stdlib/include/qlc.hrl").

%% `httpd' module API
-export([do/1]).

%%%---------------------------------------------------------------------------

%% record
-record(mod, {
  init_data, % this field is not mentioned in doc, but exists in passed record
  data = [],
  socket_type = ip_comm,
  socket,
  config_db,
  method,
  absolute_uri,
  request_uri,
  http_version,
  request_line,
  parsed_header = [],
  entity_body,
  connection
}).

%%%---------------------------------------------------------------------------

%% @type proc_spec() = {Module :: atom(), Function :: atom()} | fun().

%% @type http_response() =
%%   {StatusCode :: integer(), Headers :: list(), Body :: iolist()}.

%%%---------------------------------------------------------------------------

%% @private
%% @doc Process HTTP request (`inets' callback).
%%
%%   Return `{proceed, ModData#mod.data}' when operation skipped.
%%
%% @spec do(#mod{}) ->
%%     {proceed, list()}
%%   | {proceed, [{response, {response, Headers, Body}}]}
%%   | {break, [{response, {response, Headers, Body}}]}

do(ModData = #mod{}) ->
  case find_prefix(ModData#mod.config_db, ModData#mod.request_uri) of
    none ->
      % didn't find anything, pass the request over
      {proceed, ModData#mod.data};
    {RootURI, DispatchTable} ->
      % intercept and consume the request

      % TODO: what else?
      %   * HTTP authentication data
      %   * SSL certificate details, if any
      %   * client IP+port
      %   * server IP+port
      %   * virtual host name
      %   * protocol (HTTP, HTTPs)
      Environment = [
        {root_uri, RootURI},
        {uri, ModData#mod.request_uri}
      ],

      % `Headers' don't contain `content_length'
      {StatusCode, Headers, Body} = step_validate_request(
        ModData#mod.method,
        ModData#mod.parsed_header,
        ModData#mod.entity_body,
        Environment,
        DispatchTable
      ),

      ContentLength = integer_to_list(iolist_size(Body)),

      Response = {
        response,
        [{code, StatusCode}, {content_length, ContentLength} | Headers],
        Body
      },

      {proceed, [{response, Response}]}
  end.

%%%---------------------------------------------------------------------------
%%% XML-RPC request processing steps
%%%---------------------------------------------------------------------------

%% @doc Validate HTTP request.
%%   Function validates HTTP part of XML-RPC protocol (method and
%%   <i>Content-Type</i> header).
%%
%% @spec step_validate_request(string(), [{string(),string()}], string(),
%%                             list(), term()) ->
%%   http_response()

step_validate_request("POST" = _Method, ReqHeaders, ReqBody,
                      Environment, DispatchTable) ->
  case proplists:lookup("content-type", ReqHeaders) of
    {_Key, "text/xml"} ->
      step_parse_xmlrpc_request(ReqBody, Environment, DispatchTable);
    {_Key, OtherContentType} ->
      error_logger:error_report(xmerlrpc, [{step, validate_request}, {error, invalid_content_type}, {content_type, OtherContentType}]),
      http_error(bad_request, ["Invalid content type: ", OtherContentType]);
    none ->
      error_logger:error_report(xmerlrpc, [{step, validate_request}, {error, no_content_type}]),
      http_error(bad_request, "No content type")
  end;

step_validate_request(Method, _ReqHeaders, _ReqBody,
                      _Environment, _DispatchTable) ->
  error_logger:error_report(xmerlrpc, [{step, validate_request}, {error, invalid_method}, {method, Method}]),
  http_error(bad_method, ["Invalid method: ", Method]).

%% @doc Parse XML-RPC request.
%%   Function parses request body as XML and extracts procedure name and
%%   arguments.
%%
%% @spec step_parse_xmlrpc_request(string(), list(), term()) ->
%%   http_response()

step_parse_xmlrpc_request(ReqBody, Environment, DispatchTable) ->
  case xmerlrpc_xml:parse_request(ReqBody, []) of
    {ok, request, {ProcName, ProcArgs}} ->
      step_execute_request(ProcName, ProcArgs, Environment, DispatchTable);
    {error, Reason} ->
      error_logger:error_report(xmerlrpc, [{step, parse_xmlrpc_request}, {error, Reason}]),
      http_error(bad_request, "Invalid XML-RPC request")
  end.

%% @doc Execute RPC request.
%%
%% @spec step_execute_request(xmerlrpc_xml:proc_name(), [xmerlrpc_xml:proc_arg()],
%%                       list(), term()) ->
%%   http_response()

step_execute_request(ProcName, ProcArgs, Environment, DispatchTable) ->
  case step_dispatch(ProcName, ProcArgs, Environment, DispatchTable) of
    {ok, Result} ->
      step_encode_result(Result);
    {exception, Exception} ->
      % exception is not something to log
      step_encode_exception(Exception);
    {error, _Reason} ->
      % this kind of errors should already be handled
      http_error(internal, "Dispatch error")
  end.

%% @doc Encode value returned by called function.
%%   Function finished successfully.
%%
%% @spec step_encode_result(xmerlrpc_xml:proc_arg()) ->
%%   http_response()

step_encode_result(Result) ->
  case xmerlrpc_xml:result(Result, []) of
    {ok, Body} ->
      http_success(Body);
    {error, Reason} ->
      error_logger:error_report(xmerlrpc, [{step, encode_result}, {error, Reason}, {data, Result}]),
      http_error(internal, "Procedure returned unserializable data")
  end.

%% @doc Encode error returned by called function.
%%   Function finished with an error.
%%
%% @spec step_encode_exception(iolist()) ->
%%   http_response()

step_encode_exception(Exception) ->
  {ok, Body} = xmerlrpc_xml:exception(1, Exception, []),
  http_success(Body).

%% @doc Find an appropriate function from dispatch table.
%%
%%   `{error,_}' is operational error. `{exception,_}' is an error reported by
%%   the called function.
%%
%% @spec step_dispatch(xmerlrpc_xml:proc_name(), [xmerlrpc_xml:proc_arg()],
%%                     [{Key :: atom(), Value :: term()}],
%%                     [{binary(), proc_spec()}]) ->
%%     {ok, xmerlrpc_xml:xmlrpc_result()}
%%   | {exception, Exception :: iolist()}
%%   | {error, Reason}

step_dispatch(ProcName, ProcArgs, Environment, DispatchTable) ->
  case proplists:lookup(ProcName, DispatchTable) of
    {ProcName, Function} when is_function(Function, 2) ->
      step_call_function(Function, ProcArgs, Environment);
    {ProcName, {_Module, _Function} = FunSpec} ->
      step_call_function(FunSpec, ProcArgs, Environment);
    none ->
      error_logger:error_report(xmerlrpc, [{step, dispatch}, {error, unknown_procedure}]),
      % unknown procedure is an exception, not a transport error
      {exception, ["Unknown procedure: ", ProcName]}
  end.

%% @doc Call specified function with arguments and environment.
%%
%% @spec step_call_function({atom(), atom()} | fun(),
%%                          [xmerlrpc_xml:proc_arg()],
%%                          list()) ->
%%     {ok, xmerlrpc_xml:proc_arg()}
%%   | {exception, Exception :: iolist()}

step_call_function(Function, Args, Environment) ->
  % {ok,_} | {error,_} are for case when error is signaled by returned value
  % _ | erlang:error() are for case when error is signaled by dying
  try do_call(Function, Args, Environment) of
    {ok, Result} ->
      {ok, Result};
    {error, Reason} ->
      error_logger:error_report(xmerlrpc, [{step, call}, {error, Reason}, {mfae, {Function, Args, Environment}}]),
      {exception, "Some error"}; % TODO: include `Reason'
    Result ->
      {ok, Result}
  catch
    % TODO: case for error:undef?
    error:Reason ->
      error_logger:error_report(xmerlrpc, [{step, call}, {exit, Reason}, {mfae, {Function, Args, Environment}}]),
      {exception, "Some error"} % TODO: include `Reason'
  end.

%% @doc Apply arguments to function.
%%   The function is either a fun object or `{Mod,Fun}' tuple.

do_call(Function, Args, Environment) when is_function(Function, 2) ->
  Function(Args, Environment);
do_call({Module, Function}, Args, Environment) ->
  Module:Function(Args, Environment).


%%%---------------------------------------------------------------------------
%%% HTTP helpers
%%%---------------------------------------------------------------------------

%% @doc Reply with HTTP success.
%%   `Result' should already be XML-encoded, either XML-RPC result or
%%   exception.

http_success(Result) ->
  {200, [{content_type, "text/xml"}], Result}.

%% @doc Reply with HTTP error.

http_error(internal = _Reason, Message) ->
  {500, [{content_type, "text/plain"}], [Message, "\n"]};
%http_error(not_implemented = _Reason, Message) ->
%  {501, [{content_type, "text/plain"}], [Message, "\n"]};
http_error(bad_request = _Reason, Message) ->
  {400, [{content_type, "text/plain"}], [Message, "\n"]};
http_error(bad_method = _Reason, Message) ->
  {405, [{content_type, "text/plain"}], [Message, "\n"]}.

%%%---------------------------------------------------------------------------

%% @doc Find the longest URI prefix in configuration that matches the request.
%%
%% @spec find_prefix(term(), string()) ->
%%   {string(), DispatchTable :: term()} | none

find_prefix(Table, RequestURI) ->
  Q = qlc:q([
    {Root, DispatchTable} ||
    {xmlrpc, {Root, DispatchTable}} <- ets:table(Table),
    is_uri_prefix_of(Root, RequestURI)
  ]),
  % thanks to the sorting (DESC), nested prefixes should work consistently
  case qlc:e(qlc:keysort(1, Q, {order, descending})) of
    [] ->
      none;
    [Prefix | _Rest] ->
      Prefix
  end.

%%%---------------------------------------------------------------------------

%% @doc Check if the `Prefix' is a prefix of `URI'.
%%
%% @spec is_uri_prefix_of(string(), string()) ->
%%   bool()

is_uri_prefix_of("" = _Prefix, "" = _URI) ->
  true;
is_uri_prefix_of("" = _Prefix, "/" ++ _URI) ->
  true;
is_uri_prefix_of("" = _Prefix, "?" ++ _URI) ->
  true;
is_uri_prefix_of("/" = _Prefix, "/" ++ _URI) ->
  true;
is_uri_prefix_of([C | Prefix], [C | URI]) ->
  is_uri_prefix_of(Prefix, URI);
is_uri_prefix_of(_Prefix, _URI) ->
  false.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
