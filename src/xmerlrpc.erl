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
-export([parse/1, parse_request/1, parse_response/1]).
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

call(_Proc, _Args, _Opts) ->
  'TODO'.

%%% }}}
%%%---------------------------------------------------------------------------
%%% XML message construction and parsing {{{

%%%---------------------------------------------------------
%%% message construction {{{

%% @doc Create XML-RPC request (function call) document.
%%
%% @spec request(xmerlrpc_xml:proc_name(), [xmerlrpc_xml:proc_arg()], Opts) ->
%%   iolist()

request(_Proc, _Args, _Opts) ->
  'TODO'.

%% @doc Create XML-RPC response document carrying result data returned by the
%%   function.
%%
%% @spec result(term(), Opts) ->
%%   iolist()

result(Result, _Opts) ->
  % TODO: pass options
  xmerlrpc_xml:result(Result, []).

%% @doc Create XML-RPC exception document.
%%
%% @TODO
%%   Make `Message' iolist() instead of binary()
%%
%% @spec exception(integer(), binary(), Opts) ->
%%   iolist()

exception(Code, Message, _Opts) ->
  % TODO: pass options
  xmerlrpc_xml:exception(Code, Message, []).

%%% }}}
%%%---------------------------------------------------------
%%% message parsing {{{

%% @doc Parse XML message to request, result or exception.
%%
%% @spec parse(binary() | string()) ->
%%     {ok, request,   Request :: any()}
%%   | {ok, result,    Result  :: any()}
%%   | {ok, exception, Message :: any()}
%%   | {error, Reason}

parse(_XMLDocument) ->
  'TODO'.

%% @doc Parse XML message to request.
%%
%% @spec parse_request(binary() | string()) ->
%%     {ok, request, Request :: any()}
%%   | {error, Reason}

parse_request(_XMLDocument) ->
  'TODO'.

%% @doc Parse XML message to result or exception.
%%
%% @spec parse_response(binary() | string()) ->
%%     {ok, result,    Result  :: any()}
%%   | {ok, exception, Message :: any()}
%%   | {error, Reason}

parse_response(_XMLDocument) ->
  'TODO'.

%%% }}}
%%%---------------------------------------------------------

%%% }}}
%%%---------------------------------------------------------------------------
%%% HTTP request/response creation {{{

%http_request() ->
%  'TODO'.

%http_response() ->
%  'TODO'.

%%% }}}
%%%---------------------------------------------------------------------------
%%% request routing and dispatching {{{

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
