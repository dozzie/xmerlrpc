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

call(_Proc, _Args, _Opts) ->
  'TODO'.

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

request(Proc, Args, _Opts) ->
  % TODO: pass options
  xmerlrpc_xml:request(Proc, Args, []).

%% @doc Create XML-RPC response document carrying result data returned by the
%%   function.
%% @see xmerlrpc_xml:result/2
%%
%% @spec result(term(), Opts) ->
%%   iolist()

result(Result, _Opts) ->
  % TODO: pass options
  xmerlrpc_xml:result(Result, []).

%% @doc Create XML-RPC exception document.
%% @see xmerlrpc_xml:exception/3
%%
%% @TODO
%%   Allow `Message' to be iolist().
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
%% @see xmerlrpc_xml:parse/2
%%
%% @spec parse(binary() | string(), Opts) ->
%%     {ok, request,   Request :: any()}
%%   | {ok, result,    Result  :: any()}
%%   | {ok, exception, Message :: any()}
%%   | {error, Reason}

parse(XMLDocument, _Opts) ->
  % TODO: pass options
  xmerlrpc_xml:parse(XMLDocument, []).

%% @doc Parse XML message to request.
%% @see xmerlrpc_xml:parse_request/2
%%
%% @spec parse_request(binary() | string(), Opts) ->
%%     {ok, request, Request :: any()}
%%   | {error, Reason}

parse_request(XMLDocument, _Opts) ->
  % TODO: pass options
  xmerlrpc_xml:parse_request(XMLDocument, []).

%% @doc Parse XML message to result or exception.
%% @see xmerlrpc_xml:parse_response/2
%%
%% @spec parse_response(binary() | string(), Opts) ->
%%     {ok, result,    Result  :: any()}
%%   | {ok, exception, Message :: any()}
%%   | {error, Reason}

parse_response(XMLDocument, _Opts) ->
  % TODO: pass options
  xmerlrpc_xml:parse_response(XMLDocument, []).

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
