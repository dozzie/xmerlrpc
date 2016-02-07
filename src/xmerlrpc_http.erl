%%%---------------------------------------------------------------------------
%%% @doc
%%%   Behaviour for HTTP client suitable for {@link xmerlrpc:call/3}.
%%%
%%%   Module implementing this behaviour needs to have following functions
%%%   exported:
%%%
%%%   <ul>
%%%     <li>`post(Address, Headers, Request, Options) ->
%%%           {ok, Response} | {error, Reason}' -- send a POST HTTP request:
%%%       <ul>
%%%         <li>`Address' ({@type xmerlrpc_url:url_spec()}) -- URL to send
%%%             request to</li>
%%%         <li>`Headers' ({@type [header()]}) -- headers of the request,
%%%             excluding <i>Content-Length</i>, which module must determine
%%%             from `Request'</li>
%%%         <li>`Request' ({@type body()}) -- request body (XML document)</li>
%%%         <li>`Options' ({@type [option()]}) -- list of options; module must
%%%             ignore unrecognized options</li>
%%%         <li>`Response' ({@type response()}) -- response to the request, if
%%%             it ended with HTTP/2xx code</li>
%%%         <li>`Reason' ({@type term()}) -- error description (this includes
%%%             network errors and HTTP/4xx and HTTP/5xx codes)</li>
%%%       </ul>
%%%     </li>
%%%   </ul>
%%%
%%% @todo More options (`tcp_keepalive', `timeout', `creds', `retry', ...)
%%% @end
%%%---------------------------------------------------------------------------

-module(xmerlrpc_http).

-export_type([response/0, header/0, body/0, option/0]).

%%%---------------------------------------------------------------------------
%%% types

-type response() :: {[header()], body()}.
%% Response (body and headers) to HTTP request. It is assumed that the
%% response had HTTP/2xx code.

-type header() :: {Name :: binary(), Value :: binary()}.
%% HTTP header.

-type body() :: iolist().
%% Request or response body.

-type option() ::
    {ssl_verify, peer | none}
  | {ssl_ca, file:filename()}
  | {atom(), term()}
  | atom().

%%%---------------------------------------------------------------------------

-callback post(Address :: xmerlrpc_url:url_spec(),
               Headers :: [header()],
               Request :: body(),
               Options :: [option()]) ->
  {ok, response()} | {error, term()}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
