%%%---------------------------------------------------------------------------
%%% @doc
%%%   `inets'/`httpd' request handler module.
%%%
%%%   For example of use, see {@link examples_server}.
%%%
%%%   == Handler interface ==
%%%
%%%   <b>TODO</b>
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

%% @type proc_spec() = {Module :: atom(), Function :: atom()}.

%%%---------------------------------------------------------------------------

%% @private
%% @doc Process HTTP request (`inets' callback).
%%
%%   Return `{proceed, ModData#mod.data}' when operation skipped.
%%
%% @spec do(#mod{}) ->
%%     {proceed, list()}
%%   | {proceed, [{response, {StatusCode, Body}}]}
%%   | {proceed, [{response, {response, Headers, Body}}]}
%%   | {break, [{response, {StatusCode, Body}}]}
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
      {StatusCode, Headers, Body} = handle_request(
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

%% @doc Handle HTTP request.
%%   Extract function and arguments from body and pass them to
%%   {@link dispatch/4}.
%%
%% @spec handle_request(string(), [{string(),string()}], string(),
%%                      list(), term()) ->
%%   {StatusCode :: integer(), Headers :: list(), Body :: iolist()}

handle_request("POST" = _Method, ReqHeaders, ReqBody,
               Environment, DispatchTable) ->
  case proplists:lookup("content-type", ReqHeaders) of
    {_Key, "text/xml"} ->
      % TODO: proceed
      StatusCode = 501, % Not Implemented
      Headers = [{content_type, "text/plain"}],
      Body = "This function is not implemented yet\n",
      {StatusCode, Headers, Body};
    {_Key, OtherContentType} ->
      StatusCode = 400, % Bad Request
      Headers = [{content_type, "text/plain"}],
      Body = ["Invalid content type: ", OtherContentType, "\n"],
      {StatusCode, Headers, Body};
    none ->
      StatusCode = 400, % Bad Request
      Headers = [{content_type, "text/plain"}],
      Body = "No content type\n",
      {StatusCode, Headers, Body}
  end;

handle_request(Method, _ReqHeaders, _ReqBody, _Environment, _DispatchTable) ->
  StatusCode = 405, % Method Not Allowed
  Headers = [{content_type, "text/plain"}],
  Body = ["Invalid method: ", Method, "\n"],
  {StatusCode, Headers, Body}.

%%%---------------------------------------------------------------------------

%% @doc Find and call an appropriate function from dispatch table.
%%
%% @spec dispatch(xmerlrpc_xml:proc_name(), [xmerlrpc_xml:proc_arg()],
%%                [{Key :: atom(), Value :: term()}],
%%                [{binary(), proc_spec()}]) ->
%%     {ok, xmerlrpc_xml:xmlrpc_result()}
%%   | {exception, Exception}
%%   | {error, Reason}

dispatch(_ProcName, _ProcArgs, _Environment, _DispatchTable) ->
  % TODO: try..catch -> {error, Reason}
  {error, not_implemented}.

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
