%%%---------------------------------------------------------------------------
%%% @doc
%%%   Simple and dumb HTTP client for xmerlrpc.
%%% @end
%%%---------------------------------------------------------------------------

-module(xmerlrpc_http_client).

%% public API
-export([request/4]).
-export([get/1, get/2, get/3]).
-export([post/1, post/2, post/3]).

%% do I actually need to export these?
%-export_type([http_header/0, http_response/0]).

%%%---------------------------------------------------------------------------
%%% types {{{

%% @type url() = xmerlrpc_url:url() | xmerlrpc_url:url_spec().

%% @type http_response() = {Headers :: [http_header()], Body :: binary()}.

-type http_response() :: {[http_header()], binary()}.

%% @type http_header() = {Name :: binary(), Value :: binary()}.

-type http_header() :: {binary(), binary()}.

%%% }}}
%%%---------------------------------------------------------------------------
%%% generic request {{{

%% @doc Connect to HTTP server, send a request and retrieve response.
%%
%% @spec request(get | post, URL :: url(), [http_header()], binary()) ->
%%   {ok, http_response()} | {error, Reason}

request(_Method, {_Proto, _Creds, _Host, _Port, _Path} = _URLSpec, _Headers, _Body) ->
  'TODO';
request(Method, URL, Headers, Body) ->
  request(Method, xmerlrpc_url:parse(URL), Headers, Body).

%%% }}}
%%%---------------------------------------------------------------------------
%%% GET {{{

%% @doc Send GET request and retrieve response.
%%
%% @spec get(url()) ->
%%   {ok, http_response()} | {error, Reason}

get(URL) ->
  get(URL, [], none).

%% @doc Send GET request and retrieve response.
%%
%% @spec get(url(), [http_header()]) ->
%%   {ok, http_response()} | {error, Reason}

get(URL, Headers) ->
  get(URL, Headers, none).

%% @doc Send GET request and retrieve response.
%%
%% @spec get(url(), [http_header()], binary()) ->
%%   {ok, http_response()} | {error, Reason}

get(URL, Headers, Body) ->
  request(get, URL, Headers, Body).

%%% }}}
%%%---------------------------------------------------------------------------
%%% POST {{{

%% @doc Send POST request and retrieve response.
%%
%% @spec post(url()) ->
%%   {ok, http_response()} | {error, Reason}

post(URL) ->
  post(URL, [], none).

%% @doc Send POST request and retrieve response.
%%
%% @spec post(url(), [http_header()]) ->
%%   {ok, http_response()} | {error, Reason}

post(URL, Headers) ->
  post(URL, Headers, none).

%% @doc Send POST request and retrieve response.
%%
%% @spec post(url(), [http_header()], binary()) ->
%%   {ok, http_response()} | {error, Reason}

post(URL, Headers, Body) ->
  request(post, URL, Headers, Body).

%%% }}}
%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
