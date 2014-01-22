%%%---------------------------------------------------------------------------
%%% @doc
%%%   Simple and dumb HTTP client for xmerlrpc.
%%%
%%% @TODO SSL/TLS options (e.g. certificate verification).
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(xmerlrpc_http_client).

%% public API
-export([request/5]).
-export([get/2, get/3, get/4]).
-export([post/2, post/3, post/4]).

%% do I actually need to export these?
%-export_type([http_header/0, http_response/0]).

%%%---------------------------------------------------------------------------
%%% types {{{

%% @type url() = xmerlrpc_url:url() | xmerlrpc_url:url_spec().

%% @type http_response() = {Headers :: [http_header()], Body :: binary()}.

-type http_response() :: {[http_header()], binary()}.

%% @type http_header() = {Name :: binary(), Value :: binary()}.

-type http_header() :: {binary(), binary()}.

%% @type socket().

-opaque socket() :: {Type :: atom(), ssl:sslsocket() | gen_tcp:socket()}.

%%% }}}
%%%---------------------------------------------------------------------------
%%% generic request {{{

%% @doc Connect to HTTP server, send a request and retrieve response.
%%
%% @spec request(get | post, URL :: url(), [http_header()], iolist(), Opts) ->
%%   {ok, http_response()} | {error, Reason}

request(Method, URL, Headers, Body, Opts) when is_list(URL) ->
  request(Method, xmerlrpc_url:parse(URL), Headers, Body, Opts);

request(Method, {Proto, _Creds, Host, Port, Path}, Headers, Body, _Opts) ->
  % TODO: return error if encountered instead of dying

  {ok, Sock} = case Proto of
    http  -> open_http(Host, Port);
    https -> open_https(Host, Port)
  end,

  % TODO: Connection: keep-alive + retries
  % TODO: credentials
  send_http_method(Sock, Method, Path),
  send_http_headers(Sock, [{"Host", Host}, {"Connection", "close"} | Headers]),
  send_http_body(Sock, Body),

  {Code, StatusLine, ReplyHeaders, ReplyBodyList} = read_http_response(Sock),

  ok = close_http(Sock),
  ReplyBody = iolist_to_binary(ReplyBodyList),

  case Code div 100 of
    2 -> {ok, {ReplyHeaders, ReplyBody}};
    3 -> {error, redirection_not_supported};
    4 -> {error, {client_error, Code, StatusLine, ReplyHeaders}};
    5 -> {error, {server_error, Code, StatusLine, ReplyHeaders}}
  end.

%%----------------------------------------------------------
%% open/close connection {{{

%% @doc Open HTTP (unencrypted) connection to specified host.
%%   Port defaults to 80.
%%
%% @spec open_http(string(), integer() | default) ->
%%   {ok, socket()} | {error, Reason}

open_http(Host, default) ->
  open_http(Host, 80);
open_http(Host, Port) ->
  TCPOpts = [{packet, line}, {active, false}, binary],
  {ok, Sock} = gen_tcp:connect(Host, Port, TCPOpts),
  {ok, {gen_tcp, Sock}}.

%% @doc Open HTTPs (SSL/TLS) connection to specified host.
%%   Port defaults to 443.
%%
%% @spec open_https(string(), integer() | default) ->
%%   {ok, socket()} | {error, Reason}

open_https(Host, default) ->
  open_https(Host, 443);
open_https(Host, Port) ->
  SSLOpts = [{packet, line}, {active, false}, binary],
  {ok, Sock} = ssl:connect(Host, Port, SSLOpts),
  {ok, {ssl, Sock}}.

%% @doc Close HTTP (unencrypted) connection.
%%
%% @spec close_http(socket()) ->
%%   ok

close_http({Mod, S} = _Sock) ->
  Mod:close(S).

%% @doc Send chunk of data through socket.
%%
%% @spec send(socket(), iolist()) ->
%%   ok | {error, Reason}

send({Mod, S} = _Sock, Data) ->
  Mod:send(S, Data).

%% @doc Read HTTP status line from socket.
%%
%% @spec recv_code(socket()) ->
%%   {ok, {Code :: integer(), Status :: binary()}} | {error, Reason}

recv_code({Mod, S} = _Sock) ->
  {ok, Line} = Mod:recv(S, 0),
  case erlang:decode_packet(http_bin, Line, []) of
    {ok, {http_response, _HTTPVer, Code, Status}, <<>>} ->
      {ok, {Code, Status}};
    {error, _Reason} = Error ->
      Error
  end.

%% @doc Read HTTP header from socket.
%%
%%   For headers returned as atoms, see erlang:decode_packet/3
%%
%% @spec recv_header(socket()) ->
%%     {ok, {Name :: binary() | atom(), Value :: binary()}}
%%   | {ok, http_eoh}
%%   | {error, Reason}

recv_header({Mod, S} = _Sock) ->
  {ok, Line} = Mod:recv(S, 0),
  % XXX: period after `Line' is artificial thing required at least under R14A
  % for decode_packet() not to return `{more,Length}'
  case erlang:decode_packet(httph_bin, <<Line/binary, ".">>, []) of
    {ok, {http_header, _HNum, HName, _Reserved, HValue}, <<".">>} ->
      {ok, {HName, HValue}};
    {ok, http_eoh, <<".">>} ->
      {ok, http_eoh};
    {more, _Length} ->
      {error, packet_too_short};
    {error, _Reason} = Error ->
      Error
  end.

%% @doc Read single (payload) line from socket.
%%
%% @spec recv_line(socket()) ->
%%   {ok, binary()} | {error, Reason}

recv_line({Mod, S} = _Sock) ->
  Mod:recv(S, 0).

%% }}}
%%----------------------------------------------------------
%% send various parts of HTTP request {{{

%% @doc Send method line of HTTP request.
%%
%% @spec send_http_method(socket(), get | post, string()) ->
%%   ok

send_http_method(Sock, get, Path) ->
  ok = send(Sock, ["GET ", Path, " HTTP/1.1\r\n"]);
send_http_method(Sock, post, Path) ->
  ok = send(Sock, ["POST ", Path, " HTTP/1.1\r\n"]).

%% @doc Send headers of HTTP request.
%%   Function doesn't send <i>end of headers</i> marker, so it's still
%%   possible to include further headers (like <i>Content-Length</i>).
%%
%% @spec send_http_headers(socket(), [http_header()]) ->
%%   ok

send_http_headers(_Sock, [] = _Headers) ->
  ok;
send_http_headers(Sock, [{Name, Value} | Rest] = _Headers) ->
  ok = send(Sock, [Name, ": ", Value, "\r\n"]),
  send_http_headers(Sock, Rest).

%% @doc Send body of HTTP request.
%%   Function sends also <i>Content-Length</i> and <i>end of headers</i>
%%   marker.
%%
%%   When `Body = none', no <i>Content-Length</i> is sent, of course.
%%
%% @spec send_http_body(socket(), iolist() | none) ->
%%   ok

send_http_body(Sock, none = _Body) ->
  ok = send(Sock, "\r\n");
send_http_body(Sock, <<>> = _Body) ->
  ok = send(Sock, "\r\n");
send_http_body(Sock, Body) ->
  Length = integer_to_list(iolist_size(Body)),
  ok = send(Sock, ["Content-Length: ", Length, "\r\n"]),
  ok = send(Sock, "\r\n"),
  ok = send(Sock, Body).

%% }}}
%%----------------------------------------------------------
%% read response (whole) {{{

%% @doc Read HTTP response (status line, headers and body).
%%
%% @see read_http_headers/1
%% @see read_http_body_size/2
%% @see read_http_body_chunked/1
%% @see read_http_body_till_eof/1
%%
%% @spec read_http_response(socket()) ->
%%   {Code :: integer(), Status :: binary(),
%%     Headers :: [http_header()], Body :: iolist()}

read_http_response(Sock) ->
  {ok, {Code, Status}} = recv_code(Sock),
  Headers = read_http_headers(Sock),
  Body = case proplists:get_value(<<"Content-Length">>, Headers) of
    undefined ->
      case proplists:get_value(<<"Transfer-Encoding">>, Headers) of
        undefined ->
          read_http_body_till_eof(Sock);
        <<"chunked", _Rest/binary>> ->
          read_http_body_chunked(Sock)
      end;
    LengthStr ->
      Length = list_to_integer(binary_to_list(LengthStr)),
      read_http_body_size(Sock, Length)
  end,
  {Code, Status, Headers, Body}.

%% }}}
%%----------------------------------------------------------
%% read response headers {{{

%% @doc Read headers of HTTP response.
%%
%%   Headers' values don't include trailing CRLF.
%%
%% @spec read_http_headers(socket()) ->
%%   [http_header()]

read_http_headers(Sock) ->
  case recv_header(Sock) of
    {ok, {Name, Value}} when is_atom(Name) ->
      % NOTE: `Name' has normalized case
      BinName = atom_to_binary(Name, utf8),
      [{BinName, Value} | read_http_headers(Sock)];
    {ok, {Name, Value}} ->
      % NOTE: `Name' has normalized case
      [{Name, Value} | read_http_headers(Sock)];
    {ok, http_eoh} ->
      []
  end.

%% }}}
%%----------------------------------------------------------
%% read response body {{{

%% @doc Read body of HTTP response.
%%
%%   For cases when server sent <i>Content-Length</i> header.
%%
%% @spec read_http_body_size(socket(), Size :: integer()) ->
%%   iolist()

read_http_body_size(_Sock, 0) ->
  [];
read_http_body_size(Sock, Size) when Size > 0 ->
  % NOTE: if `{error,closed}' was encountered, throw an error (not all was
  % read, as there's still `Size > 0')
  {ok, Line} = recv_line(Sock),
  [Line | read_http_body_size(Sock, Size - byte_size(Line))].

%% @doc Read body of HTTP response.
%%
%%   For cases when server sent <i>Transfer-Encoding: chunked</i> header.
%%
%% @spec read_http_body_chunked(socket()) ->
%%   iolist()

read_http_body_chunked(Sock) ->
  {ok, Line} = recv_line(Sock),
  LineLen = byte_size(Line) - 2,
  <<ChunkSizeBin:LineLen/binary, "\r\n">> = Line, % make sure CRLF ending
  case ChunkSizeBin of
    <<"0", _Tail/binary>> ->
      % end-of-chunks
      % NOTE: it's not totally accurate pattern; just hope nobody sends size
      % starting with zero
      % TODO: read trailer and additional CRLF
      [];
    _Any ->
      % Erlang R14A doesn't have binary_to_integer()
      ChunkSize = erlang:list_to_integer(binary_to_list(ChunkSizeBin), 16),
      Chunk = read_http_body_size(Sock, ChunkSize),
      % chunk ends with CRLF
      {ok, <<"\r\n">>} = recv_line(Sock),
      [Chunk | read_http_body_chunked(Sock)]
  end.

%% @doc Read body of HTTP response.
%%
%%   For cases when no content length is known in advance. In such cases the
%%   socket will be in EOF condition after reading the body (user still needs
%%   to call {@link close_http/1}).
%%
%% @spec read_http_body_till_eof(socket()) ->
%%   iolist()

read_http_body_till_eof(Sock) ->
  case recv_line(Sock) of
    {ok, Line} ->
      [Line | read_http_body_till_eof(Sock)];
    {error, closed} ->
      []
  end.

%% }}}
%%----------------------------------------------------------

%%% }}}
%%%---------------------------------------------------------------------------
%%% GET {{{

%% @doc Send GET request and retrieve response.
%%
%% @spec get(url(), Opts) ->
%%   {ok, http_response()} | {error, Reason}

get(URL, Opts) ->
  get(URL, [], none, Opts).

%% @doc Send GET request and retrieve response.
%%
%% @spec get(url(), [http_header()], Opts) ->
%%   {ok, http_response()} | {error, Reason}

get(URL, Headers, Opts) ->
  get(URL, Headers, none, Opts).

%% @doc Send GET request and retrieve response.
%%
%% @spec get(url(), [http_header()], iolist(), Opts) ->
%%   {ok, http_response()} | {error, Reason}

get(URL, Headers, Body, Opts) ->
  request(get, URL, Headers, Body, Opts).

%%% }}}
%%%---------------------------------------------------------------------------
%%% POST {{{

%% @doc Send POST request and retrieve response.
%%
%% @spec post(url(), Options) ->
%%   {ok, http_response()} | {error, Reason}

post(URL, Opts) ->
  post(URL, [], none, Opts).

%% @doc Send POST request and retrieve response.
%%
%% @spec post(url(), [http_header()], Options) ->
%%   {ok, http_response()} | {error, Reason}

post(URL, Headers, Opts) ->
  post(URL, Headers, none, Opts).

%% @doc Send POST request and retrieve response.
%%
%% @spec post(url(), [http_header()], iolist(), Options) ->
%%   {ok, http_response()} | {error, Reason}

post(URL, Headers, Body, Opts) ->
  request(post, URL, Headers, Body, Opts).

%%% }}}
%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
