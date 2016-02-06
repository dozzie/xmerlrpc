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
-export([percent_encode/1]).

%% raw URL and URL broken into parts
-export_type([url/0, url_spec/0]).
%% URL parts
-export_type([protocol/0, credentials/0, hostname/0, portnum/0, path/0]).

%%%---------------------------------------------------------------------------
%%% types {{{

-type url() :: string().
%% Raw, unparsed URL.

-type url_spec() :: {protocol(), credentials(), hostname(), portnum(), path()}.
%% URL broken into parts.
%%
%% URL arguments (`?foo=...') are part of path.

-type protocol() :: http | https.

-type hostname() :: string().

-type portnum() :: inet:port_number() | default.

-type credentials() :: none | {Username :: string(), Password :: string()}.

-type path() :: string().

%%% }}}
%%%---------------------------------------------------------------------------
%%% split URL {{{

%% @doc Split URL to protocol, host, port, path, and credentials.
%%
%%   Port is always returned as a number.
%%
%%   Note that URL arguments (`?foo=...') are treated as a part of path.

-spec parse(url()) ->
  url_spec() | {error, term()}.

parse(URL) ->
  try
    parse_real(URL)
  catch
    error:_Any ->
      {error, badarg}
  end.

%% @doc Split URL to protocol, host, port, path, and credentials.
%%   This is the working function. {@link parse/1} only catches pattern
%%   matching errors.

-spec parse_real(url()) ->
  url_spec().

parse_real(URL) ->
  URLLowCase = string:to_lower(URL),

  % also, RestLowCase is now bound
  {Proto, Rest} = case URLLowCase of
    "https://" ++ RestLowCase ->
      {https, string:substr(URL, 9)};
    "http://" ++ RestLowCase ->
      {http, string:substr(URL, 8)}
  end,

  {CredsHostPort, CredsHostPortLowCase, Path} = case string:chr(Rest, $/) of
    0  -> {Rest, RestLowCase, "/"};
    P1 -> {
      string:substr(Rest, 1, P1-1),
      string:substr(RestLowCase, 1, P1-1),
      string:substr(Rest, P1)
    }
  end,

  {CredsPair, HostPort} = case string:chr(CredsHostPort, $@) of
    0  -> {"", CredsHostPortLowCase};
    P2 -> {
      string:substr(CredsHostPort, 1, P2-1),
      string:substr(CredsHostPortLowCase, P2+1)
    }
  end,

  Creds = case {CredsPair, string:chr(CredsPair, $:)} of
    {"", 0} -> none;
    {_, 0}  -> {CredsPair, ""}; % XXX: or 'none'?
    {_, P3} -> {
      string:substr(CredsPair, 1, P3-1),
      string:substr(CredsPair, P3+1)
    }
  end,

  {Host, Port} = case string:chr(HostPort, $:) of
    0  ->
      case Proto of
        http  -> {HostPort, 80};
        https -> {HostPort, 443}
      end;
    P4 -> {
      string:substr(HostPort, 1, P4-1),
      list_to_integer(string:substr(HostPort, P4+1))
    }
  end,

  {Proto, Creds, Host, Port, Path}.

%%% }}}
%%%---------------------------------------------------------------------------
%%% form URL {{{

%% @doc Reconstruct HTTP URL from fragments.

-spec form(protocol(), credentials(), hostname(), portnum(), path()) ->
  url().

form(Protocol, Creds, Host, Port, Path) ->
  case Protocol of
    http -> "http://";
    https -> "https://"
  end
  ++
  case Creds of
    none -> "";
    {User, Pass} -> percent_encode(User) ++ ":" ++ percent_encode(Pass) ++ "@"
  end
  ++
  Host
  ++
  case {Protocol, Port} of
    {_, default} -> "";
    {http,  80}  -> "";
    {https, 443} -> "";
    {_, _}       -> ":" ++ integer_to_list(Port)
  end
  ++
  case Path of
    "" -> "/";
    _  -> Path
  end.

%% @doc `%'-encode string.
%%   List of characters to encode taken from
%%   <a href="https://www.ietf.org/rfc/rfc3986.txt">RFC 3986</a>.

-spec percent_encode(string() | binary()) ->
  string().

percent_encode(String) when is_binary(String) ->
  percent_encode(binary_to_list(String));
percent_encode(":" ++ Rest) -> "%3a" ++ percent_encode(Rest);
percent_encode("/" ++ Rest) -> "%2f" ++ percent_encode(Rest);
percent_encode("?" ++ Rest) -> "%3f" ++ percent_encode(Rest);
percent_encode("#" ++ Rest) -> "%23" ++ percent_encode(Rest);
percent_encode("[" ++ Rest) -> "%5b" ++ percent_encode(Rest);
percent_encode("]" ++ Rest) -> "%5d" ++ percent_encode(Rest);
percent_encode("@" ++ Rest) -> "%40" ++ percent_encode(Rest);
percent_encode("!" ++ Rest) -> "%21" ++ percent_encode(Rest);
percent_encode("$" ++ Rest) -> "%24" ++ percent_encode(Rest);
percent_encode("&" ++ Rest) -> "%26" ++ percent_encode(Rest);
percent_encode("'" ++ Rest) -> "%27" ++ percent_encode(Rest);
percent_encode("(" ++ Rest) -> "%28" ++ percent_encode(Rest);
percent_encode(")" ++ Rest) -> "%29" ++ percent_encode(Rest);
percent_encode("*" ++ Rest) -> "%2a" ++ percent_encode(Rest);
percent_encode("+" ++ Rest) -> "%2b" ++ percent_encode(Rest);
percent_encode("," ++ Rest) -> "%2c" ++ percent_encode(Rest);
percent_encode(";" ++ Rest) -> "%3b" ++ percent_encode(Rest);
percent_encode("=" ++ Rest) -> "%3d" ++ percent_encode(Rest);
percent_encode([C | Rest])  -> [C | percent_encode(Rest)];
percent_encode("") -> "".

%% @equiv form(Protocol, none, Host, Port, Path)
%%
%% @see form/5

-spec form(protocol(), hostname(), portnum(), path()) ->
  url().

form(Protocol, Host, Port, Path) ->
  form(Protocol, none, Host, Port, Path).

%% @equiv form(Protocol, none, Host, Port, Path)
%%
%% @see form/5

-spec form(hostname(), portnum()) ->
  url().

form(Host, Port) ->
  form(http, Host, Port, "/").

%% @doc Reconstruct HTTP URL from fragments.
%%   The tuple is compatible with the one returned by {@link parse/1}.
%%
%% @see form/5

-spec form(url_spec()) ->
  url().

form({Protocol, Creds, Host, Port, Path} = _URLSpec) ->
  form(Protocol, Creds, Host, Port, Path).

%%% }}}
%%%---------------------------------------------------------------------------
%%% defaults for url_spec() {{{

%% @doc Fill {@type url_spec()} tuple with defaults.
%%   The tuple is compatible with the one returned by {@link parse/1}.

-spec url_spec(protocol(), credentials(), hostname(), portnum(), path()) ->
  url_spec().

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
%%
%% @see url_spec/5

-spec url_spec(protocol(), hostname(), portnum(), path()) ->
  url_spec().

url_spec(Protocol, Host, Port, Path) ->
  url_spec(Protocol, none, Host, Port, Path).

%% @equiv url_spec(http, Host, Port, "/")
%%
%% @see url_spec/5

-spec url_spec(hostname(), portnum()) ->
  url_spec().

url_spec(Host, Port) ->
  url_spec(http, Host, Port, "/").

%% @equiv url_spec(Host, default)
%%
%% @see url_spec/5

-spec url_spec(hostname()) ->
  url_spec().

url_spec(Host) ->
  url_spec(Host, default).

%%% }}}
%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
