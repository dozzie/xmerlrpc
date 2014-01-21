%%%---------------------------------------------------------------------------
%%% @doc
%%%   URL parser for xmerlrpc.
%%% @end
%%%---------------------------------------------------------------------------

-module(xmerlrpc_url).

%% public API
-export([parse/1]).
-export([form/1, form/2, form/4, form/5]).
-export([url_spec/1, url_spec/2, url_spec/4, url_spec/5]).

%% raw URL and URL broken into parts
-export_type([url/0, url_spec/0]).
%% URL fragments
-export_type([protocol/0, credentials/0, hostname/0, portnum/0]).

%%%---------------------------------------------------------------------------
%%% types {{{

%% @type url() = string().
%%
%% Raw, unparsed URL.
-type url() :: string().

%% @type url_spec() =
%%   {protocol(), credentials(), hostname(), portnum() | default,
%%     Path :: string()}.
%%
%% URL broken into parts.
%%
%% URL arguments (`?foo=...') are part of `Path'.
-type url_spec() ::
  {protocol(), credentials(), hostname(), portnum() | default,
    Path :: string()}.

%% @type protocol() = http | https.
-type protocol() :: http | https.

%% @type hostname() = string().
-type hostname() :: string().

%% @type portnum() = non_neg_integer().
-type portnum() :: non_neg_integer().

%% @type credentials() = none | {Username :: string(), Password :: string()}.
-type credentials() :: none | {Username :: string(), Password :: string()}.

%%% }}}
%%%---------------------------------------------------------------------------
%%% split URL {{{

%% @doc Split URL to protocol, host, port, path, and credentials.
%%
%%   Note that URL arguments (`?foo=...') are treated as a part of path.
%%
%% @spec parse(url()) ->
%%   url_spec()

parse(_URL) ->
  'TODO'.

%%% }}}
%%%---------------------------------------------------------------------------
%%% form URL {{{

%% @doc Reconstruct HTTP URL from fragments.
%%
%% @spec form(protocol(), credentials(), hostname(), portnum() | default,
%%            string()) ->
%%   url()

form(_Protocol, _Creds, _Host, _Port, _Path) ->
  'TODO'.

%% @doc Reconstruct HTTP URL from fragments.
%%
%% @spec form(protocol(), hostname(), portnum() | default, string()) ->
%%   url()

form(Protocol, Host, Port, Path) ->
  form(Protocol, none, Host, Port, Path).

%% @doc Reconstruct HTTP URL from fragments.
%%
%% @spec form(hostname(), portnum() | default) ->
%%   url()

form(Host, Port) ->
  form(http, Host, Port, "/").

%% @doc Reconstruct HTTP URL from fragments.
%%   The tuple is compatible with the one returned by {@link parse/1}.
%%
%% @spec form(url_spec()) ->
%%   url()

form({Protocol, Creds, Host, Port, Path} = _URLSpec) ->
  form(Protocol, Creds, Host, Port, Path).

%%% }}}
%%%---------------------------------------------------------------------------
%%% defaults for url_spec() {{{

%% @doc Fill `url_spec()' tuple with defaults.
%%   The tuple is compatible with the one returned by {@link parse/1}.
%%
%% @spec url_spec(protocol(), credentials(), hostname(), portnum() | default,
%%                string()) ->
%%   url_spec()

url_spec(Protocol, Creds, Host, Port, Path) ->
  case Creds of
    {_User, _Pass} -> ok;
    none -> ok
    % else error(case_clause)
  end,
  PortNumeric = case {Protocol, Port} of
    {http,  default} -> 80;
    {https, default} -> 443;
    {_Any,  _Number} -> Port
  end,
  {Protocol, Creds, Host, PortNumeric, Path}.

%% @equiv url_spec(Protocol, none, Host, Port, Path)
%% @see   url_spec/5
%%
%% @spec url_spec(protocol(), hostname(), portnum() | default, string()) ->
%%   url_spec()

url_spec(Protocol, Host, Port, Path) ->
  url_spec(Protocol, none, Host, Port, Path).

%% @equiv url_spec(http, Host, Port, "/")
%% @see   url_spec/5
%%
%% @spec url_spec(hostname(), portnum() | default) ->
%%   url_spec()

url_spec(Host, Port) ->
  url_spec(http, Host, Port, "/").

%% @equiv url_spec(Host, default)
%% @see   url_spec/5
%%
%% @spec url_spec(hostname()) ->
%%   url_spec()

url_spec(Host) ->
  url_spec(Host, default).

%%% }}}
%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
