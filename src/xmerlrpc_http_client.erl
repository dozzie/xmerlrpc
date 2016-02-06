%%%---------------------------------------------------------------------------
%%% @doc
%%%   Simple and dumb HTTP client for xmerlrpc.
%%%
%%% @todo SSL/TLS options (e.g. certificate verification).
%%% @todo Error when the server returns content without newlines (and closes
%%%   the connection)
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(xmerlrpc_http_client).

%% public API
-export([request/5]).
-export([get/2, get/3, get/4]).
-export([post/2, post/3, post/4]).

%%%---------------------------------------------------------------------------
%%% types {{{

-type url() :: xmerlrpc_url:url() | xmerlrpc_url:url_spec().

-type http_request_body() :: none | iolist().

-type http_response() :: {[xmerlrpc:http_header()], Body :: binary()}.

-type http_status_code() :: pos_integer().

-type http_status_reason() :: binary().

-type socket() :: {Type :: atom(), Socket :: term()}.

-type optlist() :: [{atom(), term()} | atom()].

%%% }}}
%%%---------------------------------------------------------------------------
%%% generic request {{{

%% @doc Connect to HTTP server, send a request and retrieve response.

-spec request(get | post, url(), [xmerlrpc:http_header()], http_request_body(),
              optlist()) ->
  {ok, http_response()} | {error, term()}.

request(Method, URL, Headers, Body, Opts) when is_list(URL) ->
  request(Method, xmerlrpc_url:parse(URL), Headers, Body, Opts);

request(Method, {Proto, _Creds, Host, Port, Path}, Headers, Body, _Opts) ->
  try
    Sock = case Proto of
      http  -> open_http(Host, Port);
      https -> open_https(Host, Port)
    end,

    % TODO: Connection: keep-alive + retries
    % TODO: credentials
    send_http_method(Sock, Method, Path),
    send_http_headers(Sock, [{"Host",Host}, {"Connection","close"} | Headers]),
    send_http_body(Sock, Body),

    {Code, StatusLine, ReplyHeaders, ReplyBodyList} = read_http_response(Sock),

    close_http(Sock),

    case Code div 100 of
      2 -> {ok, {ReplyHeaders, iolist_to_binary(ReplyBodyList)}};
      3 -> {error, redirection_not_supported};
      4 -> {error, {client_error, Code, StatusLine, ReplyHeaders}};
      5 -> {error, {server_error, Code, StatusLine, ReplyHeaders}}
    end
  catch
    throw:Error ->
      Error
  end.

%%----------------------------------------------------------
%% open/close/send/recv {{{

%% @doc Open HTTP (unencrypted) connection to specified host.
%%   Port defaults to 80.

-spec open_http(xmerlrpc_url:hostname(), xmerlrpc_url:portnum()) ->
  socket().

open_http(Host, default) ->
  open_http(Host, 80);
open_http(Host, Port) ->
  TCPOpts = [{packet, line}, {active, false}, binary],
  case gen_tcp:connect(Host, Port, TCPOpts) of
    {ok, Sock} ->
      {gen_tcp, Sock};
    {error, _Reason} = Error ->
      throw(Error)
  end.

%% @doc Open HTTPs (SSL/TLS) connection to specified host.
%%   Port defaults to 443.

-spec open_https(xmerlrpc_url:hostname(), xmerlrpc_url:portnum()) ->
  socket().

open_https(Host, default) ->
  open_https(Host, 443);
open_https(Host, Port) ->
  SSLOpts = [{packet, line}, {active, false}, binary],
  case ssl:connect(Host, Port, SSLOpts) of
    {ok, Sock} ->
      {ssl, Sock};
    {error, _Reason} = Error ->
      throw(Error)
  end.

%% @doc Close HTTP (unencrypted) connection.

-spec close_http(socket()) ->
  ok.

close_http({Mod, S} = _Sock) ->
  Mod:close(S).

%% @doc Send chunk of data through socket.

-spec send(socket(), iolist()) ->
  ok.

send({Mod, S} = _Sock, Data) ->
  case Mod:send(S, Data) of
    ok ->
      ok;
    {error, _Reason} = Error ->
      throw(Error)
  end.

%% @doc Read HTTP status line from socket.

-spec recv_code(socket()) ->
  {http_status_code(), http_status_reason()}.

recv_code({Mod, S} = _Sock) ->
  Line = case Mod:recv(S, 0) of
    {ok, L} ->
      L;
    {error, _RecvReason} = RecvError ->
      throw(RecvError)
  end,
  case erlang:decode_packet(http, Line, []) of
    {ok, {http_response, _HTTPVer, Code, Status}, <<>>} ->
      {Code, ensure_binary(Status)};
    {error, _DecodeReason} = DecodeError ->
      throw(DecodeError)
  end.

%% @doc Read HTTP header from socket.
%%
%%   For headers returned as atoms, see erlang:decode_packet/3

-spec recv_header(socket()) ->
  xmerlrpc:http_header() | http_eoh.

recv_header({Mod, S} = _Sock) ->
  Line = case Mod:recv(S, 0) of
    {ok, L} ->
      L;
    {error, _RecvReason} = RecvError ->
      throw(RecvError)
  end,
  % XXX: period after `Line' is artificial thing required at least under R14A
  % for decode_packet() not to return `{more,Length}'
  case erlang:decode_packet(httph, <<Line/binary, ".">>, []) of
    {ok, {http_header, _HNum, HName, _Reserved, HValue}, <<".">>} ->
      {ensure_binary(HName), ensure_binary(HValue)};
    {ok, http_eoh, <<".">>} ->
      http_eoh;
    {more, _Length} ->
      throw({error, packet_too_short});
    {error, _Reason} = Error ->
      throw(Error)
  end.

%% @doc Read single (payload) line from socket.

-spec recv_line(socket()) ->
  binary() | eof.

recv_line({Mod, S} = _Sock) ->
  case Mod:recv(S, 0) of
    {ok, L} ->
      L;
    {error, closed} ->
      eof;
    {error, _RecvReason} = RecvError ->
      throw(RecvError)
  end.

%% @doc Convert atom or string to a binary.

-spec ensure_binary(atom() | string()) ->
  binary().

ensure_binary(Name) when is_atom(Name) ->
  atom_to_binary(Name, utf8);
ensure_binary(Name) when is_list(Name) ->
  list_to_binary(Name).

%% }}}
%%----------------------------------------------------------
%% send various parts of HTTP request {{{

%% @doc Send method line of HTTP request.

-spec send_http_method(socket(), get | post, xmerlrpc_url:path()) ->
  ok.

send_http_method(Sock, get, Path) ->
  send(Sock, ["GET ", Path, " HTTP/1.1\r\n"]);
send_http_method(Sock, post, Path) ->
  send(Sock, ["POST ", Path, " HTTP/1.1\r\n"]).

%% @doc Send headers of HTTP request.
%%   Function doesn't send <i>end of headers</i> marker, so it's still
%%   possible to include further headers (like <i>Content-Length</i>).

-spec send_http_headers(socket(), [xmerlrpc:http_header()]) ->
  ok.

send_http_headers(_Sock, [] = _Headers) ->
  ok;
send_http_headers(Sock, [{Name, Value} | Rest] = _Headers) ->
  send(Sock, [Name, ": ", Value, "\r\n"]),
  send_http_headers(Sock, Rest).

%% @doc Send body of HTTP request.
%%   Function sends also <i>Content-Length</i> and <i>end of headers</i>
%%   marker.
%%
%%   When `Body = none', no <i>Content-Length</i> is sent, of course.

-spec send_http_body(socket(), http_request_body()) ->
  ok.

send_http_body(Sock, none = _Body) ->
  send(Sock, "\r\n");
send_http_body(Sock, <<>> = _Body) ->
  send(Sock, "\r\n");
send_http_body(Sock, Body) ->
  Length = integer_to_list(iolist_size(Body)),
  send(Sock, ["Content-Length: ", Length, "\r\n"]),
  send(Sock, "\r\n"),
  send(Sock, Body).

%% }}}
%%----------------------------------------------------------
%% read response (whole) {{{

%% @doc Read HTTP response (status line, headers and body).
%%
%% @see read_http_headers/1
%% @see read_http_body_size/2
%% @see read_http_body_chunked/1
%% @see read_http_body_till_eof/1

-spec read_http_response(socket()) ->
  {http_status_code(), http_status_reason(),
    [xmerlrpc:http_header()], Body :: iolist()}.

read_http_response(Sock) ->
  {Code, Status} = recv_code(Sock),
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

-spec read_http_headers(socket()) ->
  [xmerlrpc:http_header()].

read_http_headers(Sock) ->
  case recv_header(Sock) of
    {Name, Value} ->
      % NOTE: `Name' has normalized case
      [{Name, Value} | read_http_headers(Sock)];
    http_eoh ->
      []
  end.

%% }}}
%%----------------------------------------------------------
%% read response body {{{

%% @doc Read body of HTTP response.
%%
%%   For cases when server sent <i>Content-Length</i> header.

-spec read_http_body_size(socket(), integer()) ->
  iolist().

read_http_body_size(_Sock, 0 = _Size) ->
  [];
read_http_body_size(Sock, Size) when Size > 0 ->
  % NOTE: if EOF was encountered, throw an error (not all was read, as there's
  % still `Size > 0')
  Line = case recv_line(Sock) of
    L when is_binary(L) ->
      L;
    eof ->
      throw({error,eof})
  end,
  [Line | read_http_body_size(Sock, Size - byte_size(Line))].

%% @doc Read body of HTTP response.
%%
%%   For cases when server sent <i>Transfer-Encoding: chunked</i> header.

-spec read_http_body_chunked(socket()) ->
  iolist().

read_http_body_chunked(Sock) ->
  Line = case recv_line(Sock) of
    L when is_binary(L) ->
      L;
    eof ->
      throw({error,eof})
  end,
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
      case recv_line(Sock) of
        <<"\r\n">> -> ok;
        eof  -> throw({error,eof});
        _Any -> throw({error,protocol_error})
      end,
      [Chunk | read_http_body_chunked(Sock)]
  end.

%% @doc Read body of HTTP response.
%%
%%   For cases when no content length is known in advance. In such cases the
%%   socket will be in EOF condition after reading the body (user still needs
%%   to call {@link close_http/1}).

-spec read_http_body_till_eof(socket()) ->
  iolist().

read_http_body_till_eof(Sock) ->
  case recv_line(Sock) of
    Line when is_binary(Line) ->
      [Line | read_http_body_till_eof(Sock)];
    eof ->
      []
  end.

%% }}}
%%----------------------------------------------------------

%%% }}}
%%%---------------------------------------------------------------------------
%%% GET {{{

%% @doc Send GET request and retrieve response.

-spec get(url(), optlist()) ->
  {ok, http_response()} | {error, term()}.

get(URL, Opts) ->
  get(URL, [], none, Opts).

%% @doc Send GET request and retrieve response.

-spec get(url(), [xmerlrpc:http_header()], optlist()) ->
  {ok, http_response()} | {error, term()}.

get(URL, Headers, Opts) ->
  get(URL, Headers, none, Opts).

%% @doc Send GET request and retrieve response.

-spec get(url(), [xmerlrpc:http_header()], http_request_body(), optlist()) ->
  {ok, http_response()} | {error, term()}.

get(URL, Headers, Body, Opts) ->
  request(get, URL, Headers, Body, Opts).

%%% }}}
%%%---------------------------------------------------------------------------
%%% POST {{{

%% @doc Send POST request and retrieve response.

-spec post(url(), optlist()) ->
  {ok, http_response()} | {error, term()}.

post(URL, Opts) ->
  post(URL, [], none, Opts).

%% @doc Send POST request and retrieve response.

-spec post(url(), [xmerlrpc:http_header()], optlist()) ->
  {ok, http_response()} | {error, term()}.

post(URL, Headers, Opts) ->
  post(URL, Headers, none, Opts).

%% @doc Send POST request and retrieve response.

-spec post(url(), [xmerlrpc:http_header()], http_request_body(), optlist()) ->
  {ok, http_response()} | {error, term()}.

post(URL, Headers, Body, Opts) ->
  request(post, URL, Headers, Body, Opts).

%%% }}}
%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
