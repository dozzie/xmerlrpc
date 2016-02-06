%%%---------------------------------------------------------------------------
%%% @doc
%%%   XML-RPC client and server interface.
%%%
%%% @todo HTTP request/response creation (`http_request()', `http_response()')
%%% @end
%%%---------------------------------------------------------------------------

-module(xmerlrpc).

%% client API
-export([call/3]).
%% XML messages API
-export([request/3, result/2, exception/3]).
-export([parse/2, parse_request/2, parse_response/2]).

-export_type([http_body/0, http_header/0]).

%%%---------------------------------------------------------------------------
%%% types {{{

-type optlist() :: [{atom(), term()} | atom()].

-type http_body() :: binary().

-type http_header() :: {Name :: binary(), Value :: binary()}.

-type http_client() :: atom() | fun().

%%% }}}
%%%---------------------------------------------------------------------------
%%% client {{{

%% @doc Call remote procedure.
%%   Function returns `{ok,Value}' on success, `{error,Reason}' on protocol
%%   level failure and `{exception,E}' when remote procedure raised an
%%   exception.

-spec call(xmerlrpc_xml:proc_name(), [xmerlrpc_xml:proc_arg()], optlist()) ->
    {ok, xmerlrpc_xml:proc_arg()}
  | {exception, xmerlrpc_xml:xmlrpc_exception()}
  | {error, term()}.

call(Proc, Args, Opts) ->
  try
    % extract URL (somewhat obvious, given the function name)
    {ok, URLSpec} = extract_url_spec(Opts),
    % build a request XML
    {ok, RequestBody} = request(Proc, Args, Opts),
    % send HTTP POST request
    {ok, {_Headers, ResponseBody}} = do_post(URLSpec, RequestBody, Opts),
    % parse response (obvious from function name)
    {ok, result, Result} = parse_response(ResponseBody, Opts),
    {ok, Result}
  catch
    error:{badmatch,{error,Reason}} ->
      {error, Reason};
    error:{badmatch,{ok,exception,Message}} ->
      {exception, Message}
  end.

%% @doc Send HTTP POST and retrieve response.
%%   Headers returned are list of 2-tuples. Unused by the caller, so no spec
%%   here.

-spec do_post(xmerlrpc_url:url_spec(), iolist(), optlist()) ->
  {ok, {[http_header()], http_body()}} | {error, term()}.

do_post(URLSpec, RequestBody, Opts) ->
  HTTPClient = proplists:get_value(http_client, Opts, xmerlrpc_http_client),
  do_post(URLSpec, RequestBody, HTTPClient, Opts).

%% @doc Send HTTP POST and retrieve response.

-spec do_post(xmerlrpc_url:url_spec(), iolist(), http_client(), optlist()) ->
  {ok, {[http_header()], http_body()}} | {error, term()}.

do_post(URLSpec, RequestBody, HTTPClient, Opts) when is_atom(HTTPClient) ->
  Headers = [{<<"Content-Type">>, <<"text/xml">>}],
  HTTPClient:post(URLSpec, Headers, RequestBody, Opts);
do_post(URLSpec, RequestBody, HTTPClient, _Opts) when is_function(HTTPClient) ->
  Headers = [{<<"Content-Type">>, <<"text/xml">>}],
  HTTPClient(URLSpec, Headers, RequestBody).

%% @doc Construct {@type xmerlrpc_url:url_spec()} based on options proplist.

-spec extract_url_spec(optlist()) ->
  {ok, xmerlrpc_url:url_spec()} | {error, term()}.

extract_url_spec(Opts) ->
  case proplists:get_value(url, Opts) of
    undefined ->
      % build one from host+port options (port defaults to default for
      % HTTP(s))
      case proplists:get_value(host, Opts) of
        undefined ->
          {error, no_host};
        Host ->
          Port = proplists:get_value(port, Opts, default),
          % HTTP vs. HTTPs is differentiated with presence of SSL options
          Proto = case proplists:is_defined(ssl_verify, Opts) orelse
            proplists:is_defined(ssl_ca, Opts) of
            true  -> https;
            false -> http
          end,
          Spec = xmerlrpc_url:url_spec(Proto, Host, Port, "/"),
          {ok, Spec}
      end;
    URL ->
      Spec = xmerlrpc_url:parse(URL),
      {ok, Spec}
  end.

%%% }}}
%%%---------------------------------------------------------------------------
%%% XML message construction and parsing {{{

%%%---------------------------------------------------------
%%% message construction {{{

%% @doc Create XML-RPC request (function call) document.
%%
%% @see xmerlrpc_xml:request/3

-spec request(xmerlrpc_xml:proc_name(), [xmerlrpc_xml:proc_arg()], optlist()) ->
  {ok, iolist()} | {error, term()}.

request(Proc, Args, Opts) ->
  xmerlrpc_xml:request(Proc, Args, Opts).

%% @doc Create XML-RPC response document carrying result data returned by the
%%   function.
%%
%% @see xmerlrpc_xml:result/2

-spec result(xmerlrpc_xml:proc_arg(), optlist()) ->
  {ok, iolist()} | {error, term()}.

result(Result, Opts) ->
  xmerlrpc_xml:result(Result, Opts).

%% @doc Create XML-RPC exception document.
%%
%% @see xmerlrpc_xml:exception/3

-spec exception(integer(), iolist(), optlist()) ->
  {ok, iolist()} | {error, term()}.

exception(Code, Message, Opts) ->
  xmerlrpc_xml:exception(Code, Message, Opts).

%%% }}}
%%%---------------------------------------------------------
%%% message parsing {{{

%% @doc Parse XML message to request, result or exception.
%%
%% @see xmerlrpc_xml:parse/2

-spec parse(binary() | string(), optlist()) ->
    {ok, request,   xmerlrpc_xml:xmlrpc_request()}
  | {ok, result,    xmerlrpc_xml:xmlrpc_result()}
  | {ok, exception, xmerlrpc_xml:xmlrpc_exception()}
  | {error, term()}.

parse(XMLDocument, Opts) ->
  xmerlrpc_xml:parse(XMLDocument, Opts).

%% @doc Parse XML message to request.
%%
%% @see xmerlrpc_xml:parse_request/2

-spec parse_request(binary() | string(), optlist()) ->
  {ok, request, xmerlrpc_xml:xmlrpc_request()} | {error, term()}.

parse_request(XMLDocument, Opts) ->
  xmerlrpc_xml:parse_request(XMLDocument, Opts).

%% @doc Parse XML message to result or exception.
%%
%% @see xmerlrpc_xml:parse_response/2

-spec parse_response(binary() | string(), optlist()) ->
    {ok, result,    xmerlrpc_xml:xmlrpc_result()}
  | {ok, exception, xmerlrpc_xml:xmlrpc_exception()}
  | {error, term()}.

parse_response(XMLDocument, Opts) ->
  xmerlrpc_xml:parse_response(XMLDocument, Opts).

%%% }}}
%%%---------------------------------------------------------

%%% }}}
%%%---------------------------------------------------------------------------
%%% HTTP request/response creation (TODO) {{{

%http_request() ->
%  'TODO'.

%http_response() ->
%  'TODO'.

%%% }}}
%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
