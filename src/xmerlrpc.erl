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
-export([request/2, result/1, exception/2]).
-export([parse/1, parse_request/1, parse_response/1]).

%%%---------------------------------------------------------------------------
%%% types {{{

-type option() ::
    xmerlrpc_http:option()
  | {url, xmerlrpc_url:url()}
  | {host, xmerlrpc_url:hostname()}
  | {port, xmerlrpc_url:portnum()}
  | {http_client, http_client()}.
%% Call configuration option.
%%
%% Option `{url,_}' has precedence over `{host,_}' and `{port,_}'.

-type http_client() ::
    module()
    | fun((xmerlrpc_url:url_spec(),
            [xmerlrpc_http:header()], xmerlrpc_http:body()) ->
          {ok, xmerlrpc_http:body()} | {error, term()}).
%% HTTP client, either a module implementing {@link xmerlrpc_http} behaviour
%% or a function that sends HTTP POST request.

%%% }}}
%%%---------------------------------------------------------------------------
%%% client {{{

%% @doc Call remote procedure.
%%   Function returns `{ok,Value}' on success, `{error,Reason}' on protocol
%%   level failure and `{exception,E}' when remote procedure raised an
%%   exception.

-spec call(xmerlrpc_xml:proc_name(), [xmerlrpc_xml:proc_arg()], [option()]) ->
    {ok, xmerlrpc_xml:proc_arg()}
  | {exception, xmerlrpc_xml:xmlrpc_exception()}
  | {error, term()}.

call(Proc, Args, Opts) ->
  try
    % extract URL (somewhat obvious, given the function name)
    {ok, URLSpec} = extract_url_spec(Opts),
    % build a request XML
    {ok, RequestBody} = request(Proc, Args),
    % send HTTP POST request
    {ok, {_Headers, ResponseBody}} = do_post(URLSpec, RequestBody, Opts),
    % parse response (obvious from function name)
    {ok, result, Result} = parse_response(ResponseBody),
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

-spec do_post(xmerlrpc_url:url_spec(), xmerlrpc_http:body(),
              [xmerlrpc_http:option()]) ->
  {ok, xmerlrpc_http:response()} | {error, term()}.

do_post(URLSpec, RequestBody, Opts) ->
  HTTPClient = proplists:get_value(http_client, Opts, xmerlrpc_http_client),
  do_post(URLSpec, RequestBody, HTTPClient, Opts).

%% @doc Send HTTP POST and retrieve response.

-spec do_post(xmerlrpc_url:url_spec(), xmerlrpc_http:body(),
              http_client(), [xmerlrpc_http:option()]) ->
  {ok, xmerlrpc_http:response()} | {error, term()}.

do_post(URLSpec, RequestBody, HTTPClient, Opts) when is_atom(HTTPClient) ->
  Headers = [{<<"Content-Type">>, <<"text/xml">>}],
  HTTPClient:post(URLSpec, Headers, RequestBody, Opts);
do_post(URLSpec, RequestBody, HTTPClient, _Opts) when is_function(HTTPClient) ->
  Headers = [{<<"Content-Type">>, <<"text/xml">>}],
  HTTPClient(URLSpec, Headers, RequestBody).

%% @doc Construct {@type xmerlrpc_url:url_spec()} based on options proplist.

-spec extract_url_spec([option()]) ->
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
%% @see xmerlrpc_xml:request/2

-spec request(xmerlrpc_xml:proc_name(), [xmerlrpc_xml:proc_arg()]) ->
  {ok, iolist()} | {error, term()}.

request(Proc, Args) ->
  xmerlrpc_xml:request(Proc, Args).

%% @doc Create XML-RPC response document carrying result data returned by the
%%   function.
%%
%% @see xmerlrpc_xml:result/1

-spec result(xmerlrpc_xml:proc_arg()) ->
  {ok, iolist()} | {error, term()}.

result(Result) ->
  xmerlrpc_xml:result(Result).

%% @doc Create XML-RPC exception document.
%%
%% @see xmerlrpc_xml:exception/2

-spec exception(integer(), iolist()) ->
  {ok, iolist()} | {error, term()}.

exception(Code, Message) ->
  xmerlrpc_xml:exception(Code, Message).

%%% }}}
%%%---------------------------------------------------------
%%% message parsing {{{

%% @doc Parse XML message to request, result or exception.
%%
%% @see xmerlrpc_xml:parse/1

-spec parse(binary() | string()) ->
    {ok, request,   xmerlrpc_xml:xmlrpc_request()}
  | {ok, result,    xmerlrpc_xml:xmlrpc_result()}
  | {ok, exception, xmerlrpc_xml:xmlrpc_exception()}
  | {error, term()}.

parse(XMLDocument) ->
  xmerlrpc_xml:parse(XMLDocument).

%% @doc Parse XML message to request.
%%
%% @see xmerlrpc_xml:parse_request/1

-spec parse_request(binary() | string()) ->
  {ok, request, xmerlrpc_xml:xmlrpc_request()} | {error, term()}.

parse_request(XMLDocument) ->
  xmerlrpc_xml:parse_request(XMLDocument).

%% @doc Parse XML message to result or exception.
%%
%% @see xmerlrpc_xml:parse_response/1

-spec parse_response(binary() | string()) ->
    {ok, result,    xmerlrpc_xml:xmlrpc_result()}
  | {ok, exception, xmerlrpc_xml:xmlrpc_exception()}
  | {error, term()}.

parse_response(XMLDocument) ->
  xmerlrpc_xml:parse_response(XMLDocument).

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
