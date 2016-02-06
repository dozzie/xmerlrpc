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
%%%             excluding <i>Content-Length</i>, which will be determined
%%%             automatically</li>
%%%         <li>`Request' ({@type body()}) -- request body (XML document)</li>
%%%         <li>`Options' ({@type optlist()}) -- list of options; module must
%%%             ignore unrecognized options</li>
%%%         <li>`Response' ({@type response()}) -- response to the request, if
%%%             it ended with HTTP/2xx code</li>
%%%         <li>`Reason' ({@type term()}) -- error description (this includes
%%%             network errors and HTTP/4xx and HTTP/5xx codes)</li>
%%%       </ul>
%%%     </li>
%%%   </ul>
%%%
%%% @todo Describe {@type optlist()} in more detail (what should be
%%%   supported).
%%% @end
%%%---------------------------------------------------------------------------

-module(xmerlrpc_http).

-export_type([response/0, header/0, body/0, optlist/0]).

%%%---------------------------------------------------------------------------
%%% types

-type response() :: {[header()], body()}.

-type header() :: {Name :: binary(), Value :: binary()}.

-type body() :: iolist().

-type optlist() :: [{atom(), term()} | atom()].

%%%---------------------------------------------------------------------------

-callback post(Address :: xmerlrpc_url:url_spec(),
               Headers :: [header()],
               Request :: body(),
               Options :: optlist()) ->
  {ok, response()} | {error, term()}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
