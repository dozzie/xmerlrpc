%%%---------------------------------------------------------------------------
%%% @doc
%%%   XML-RPC client and server.
%%% @end
%%%---------------------------------------------------------------------------

-module(xmerlrpc).

%% client API
-export([call/3]).
%% XML messages API
-export([request/3, result/2, exception/3]).
-export([parse/2, parse_request/2, parse_response/2]).
%% TODO: HTTP request/response creation
%-export([http_request/1, http_response/1]).
%% TODO: request dispatching and routing
%-export([dispatch/0, route/0, apply/0]).

%-export_type([foo/0]).

%%%---------------------------------------------------------------------------
%%% types {{{

%%% }}}
%%%---------------------------------------------------------------------------
%%% client {{{

%% @doc Call remote procedure.
%%
%% @spec call(xmerlrpc_xml:proc_name(), [xmerlrpc_xml:proc_arg()], Opts) ->
%%   term()

call(Proc, Args, Opts) ->
  URLSpec = case proplists:get_value(url, Opts) of
    undefined ->
      get_host_port_proto(Opts);
    URL ->
      xmerlrpc_url:parse(URL)
  end,

  RequestBody = request(Proc, Args, Opts),

  {ok, {_Headers, ResponseBody}} =
    case proplists:get_value(http_client, Opts, xmerlrpc_http_client) of
      HTTPClient when is_atom(HTTPClient) ->
        HTTPClient:post(
          URLSpec, [{<<"Content-Type">>, <<"text/xml">>}], RequestBody, Opts
        );
      HTTPClient when is_function(HTTPClient) ->
        HTTPClient(
          URLSpec, [{<<"Content-Type">>, <<"text/xml">>}], RequestBody
        )
    end,

  case parse_response(ResponseBody, Opts) of
    {ok, result, Result} ->
      Result;
    {ok, exception, Message} ->
      erlang:error({remote_exception, Message});
    {error, _Reason} = Error ->
      erlang:error(Error)
  end.

%% @doc Construct {@link xmerlrpc_url:url_spec/0} out of options proplist.
%%
%%   Function to be used when no `{url,URL}' tuple was present in options.
%%
%% @spec get_host_port_proto(Opts) ->
%%   xmerlrpc_url:url_spec()

get_host_port_proto(Opts) ->
  Host = case proplists:get_value(host, Opts) of
    undefined -> erlang:error(no_host);
    Hostname  -> Hostname
  end,
  Port = proplists:get_value(port, Opts, default),
  Proto = case proplists:is_defined(ssl_verify, Opts) orelse
       proplists:is_defined(ssl_ca, Opts) of
    true  -> https;
    false -> http
  end,
  URLSpec = xmerlrpc_url:url_spec(Proto, Host, Port, "/"),
  URLSpec.

%%% }}}
%%%---------------------------------------------------------------------------
%%% XML message construction and parsing {{{

%%%---------------------------------------------------------
%%% message construction {{{

%% @doc Create XML-RPC request (function call) document.
%% @see xmerlrpc_xml:request/3
%%
%% @spec request(xmerlrpc_xml:proc_name(), [xmerlrpc_xml:proc_arg()], Opts) ->
%%   iolist()

request(Proc, Args, Opts) ->
  xmerlrpc_xml:request(Proc, Args, Opts).

%% @doc Create XML-RPC response document carrying result data returned by the
%%   function.
%% @see xmerlrpc_xml:result/2
%%
%% @spec result(term(), Opts) ->
%%   iolist()

result(Result, Opts) ->
  xmerlrpc_xml:result(Result, Opts).

%% @doc Create XML-RPC exception document.
%% @see xmerlrpc_xml:exception/3
%%
%% @spec exception(integer(), iolist(), Opts) ->
%%   iolist()

exception(Code, Message, Opts) ->
  xmerlrpc_xml:exception(Code, Message, Opts).

%%% }}}
%%%---------------------------------------------------------
%%% message parsing {{{

%% @doc Parse XML message to request, result or exception.
%% @see xmerlrpc_xml:parse/2
%%
%% @spec parse(binary() | string(), Opts) ->
%%     {ok, request,   Request :: any()}
%%   | {ok, result,    Result  :: any()}
%%   | {ok, exception, Message :: any()}
%%   | {error, Reason}

parse(XMLDocument, Opts) ->
  xmerlrpc_xml:parse(XMLDocument, Opts).

%% @doc Parse XML message to request.
%% @see xmerlrpc_xml:parse_request/2
%%
%% @spec parse_request(binary() | string(), Opts) ->
%%     {ok, request, Request :: any()}
%%   | {error, Reason}

parse_request(XMLDocument, Opts) ->
  xmerlrpc_xml:parse_request(XMLDocument, Opts).

%% @doc Parse XML message to result or exception.
%% @see xmerlrpc_xml:parse_response/2
%%
%% @spec parse_response(binary() | string(), Opts) ->
%%     {ok, result,    Result  :: any()}
%%   | {ok, exception, Message :: any()}
%%   | {error, Reason}

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
%%% request routing and dispatching (TODO) {{{

% calls route() and then apply() on a request
%dispatch() ->
%  'TODO'.

% find out what to call
%route() ->
%  'TODO'.

% call what was found by route() and return the result
%apply() ->
%  'TODO'.

%%% }}}
%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
