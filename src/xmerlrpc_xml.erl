%%%---------------------------------------------------------------------------
%%% @doc
%%%   Parsing and forming XML-RPC messages for xmerlrpc.
%%%
%%%   Module uses data structures after `jsx' application, which in turn uses
%%%   <a href="http://www.erlang.org/eeps/eep-0018.html">EEP-18</a>
%%%   structures.
%%%
%%% @TODO Jiffy-like structures.
%%% @TODO Split forming and parsing XML to separate modules.
%%% @TODO Cleanup types documentation.
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(xmerlrpc_xml).

-include_lib("xmerl/include/xmerl.hrl").

-export([request/3, result/2, exception/3]).
-export([parse/2, parse_request/2, parse_response/2]).

-export_type([proc_name/0, proc_arg/0]).
-export_type([xmlrpc_request/0, xmlrpc_result/0, xmlrpc_exception/0]).

%%%---------------------------------------------------------------------------
%%% types {{{

%% @type proc_name() = string() | binary() | atom().

-type proc_name() :: string() | binary() | atom().

%% @type proc_arg() = jsx_value().

-type proc_arg() :: jsx_value().

%% @type optlist() = [{atom(), term()} | atom()].

-type optlist() :: [{atom(), term()} | atom()].

%%----------------------------------------------------------
%% parsed XML-RPC documents {{{

%% @type xmlrpc_request() = {proc_name(), [proc_arg()]}.

-type xmlrpc_request() :: {proc_name(), [proc_arg()]}.

%% @type xmlrpc_result() = jsx_value().

-type xmlrpc_result() :: jsx_value().

%% @type xmlrpc_exception() = {Code :: integer(), Message :: string()}.

-type xmlrpc_exception() :: {Code :: integer(), Message :: string()}.

%% }}}
%%----------------------------------------------------------
%% jsx-style types {{{

%% @type jsx_value() = jsx_hash() | jsx_array() | jsx_value().

-type jsx_value() :: jsx_hash() | jsx_array() | jsx_scalar().

%% @type jsx_hash() = [{}] | [{jsx_hash_key(), jsx_value()}].

-type jsx_hash() :: [{}] | [{jsx_hash_key(), jsx_value()}, ...].

%% @type jsx_array() = [jsx_value()].

-type jsx_array() :: [jsx_value()].

%% @type jsx_scalar() = binary() | number() | null | true | false.

-type jsx_scalar() :: binary() | number() | null | true | false.

%% @type jsx_hash_key() = string() | binary() | atom().

-type jsx_hash_key() :: string() | binary() | atom().

%% }}}
%%----------------------------------------------------------
%% Jiffy-style types (TODO) {{{

%% }}}
%%----------------------------------------------------------

%% @type xml_node() = #xmlElement{} | #xmlText{}.
%%
%% Internal data type. XML node suitable for xmerl.

-type xml_node() :: #xmlElement{} | #xmlText{}.

%%% }}}
%%%---------------------------------------------------------------------------
%%% XML forming {{{

%% @doc Form XML document containing XML-RPC request (function call).
%%
%% @spec request(proc_name(), [proc_arg()], optlist()) ->
%%   iolist()

-spec request(proc_name(), [proc_arg()], optlist()) ->
  iolist().

request(ProcName, ProcParams, _Opts) ->
  XMLCall = e(methodCall, [
    e(methodName, [text(name_to_string(ProcName))]),
    e(params, [ e(param, e(value, encode_value(P))) || P <- ProcParams ])
  ]),
  xmerl:export([XMLCall], xmerl_xml).

%% @doc Form XML document carrying call result (function reply).
%%
%% @spec result(proc_arg(), optlist()) ->
%%   iolist()

-spec result(proc_arg(), optlist()) ->
  iolist().

result(Result, _Opts) ->
  XMLResponse = e(methodResponse, [
    e(params, [
      e(param, [
        e(value, [
          encode_value(Result)
        ])
      ])
    ])
  ]),
  xmerl:export([XMLResponse], xmerl_xml).

%% @doc Form XML document carrying exception information (function reply).
%%
%% @spec exception(integer(), iolist(), optlist()) ->
%%   iolist()

-spec exception(integer(), iolist(), optlist()) ->
  iolist().

exception(Code, MessageIOL, _Opts) ->
  Message = iolist_to_binary(MessageIOL),
  XMLResponse = e(methodResponse, [
    e(fault, [
      e(value, [
        encode_value([{"faultCode", Code}, {"faultString", Message}])
      ])
    ])
  ]),
  xmerl:export([XMLResponse], xmerl_xml).

%%% }}}
%%%---------------------------------------------------------------------------
%%% XML parsing {{{

%% @doc Parse XML message to request, result or exception.
%%
%% @TODO Rewrite this to SAX-style parser.
%%
%% @spec parse(binary() | string(), optlist()) ->
%%     {ok, request,   xmlrpc_request()}
%%   | {ok, result,    xmlrpc_result()}
%%   | {ok, exception, xmlrpc_exception()}
%%   | {error, Reason}

-spec parse(binary() | string(), optlist()) ->
    {ok, request,   xmlrpc_request()}
  | {ok, result,    xmlrpc_result()}
  | {ok, exception, xmlrpc_exception()}
  | {error, term()}.

parse(XML, Opts) when is_binary(XML) ->
  parse(binary_to_list(XML), Opts);
parse(XML, _Opts) when is_list(XML) ->
  {Document, _Rest} = xmerl_scan:string(XML),
  decode_document(Document).

%% @doc Parse XML message to request.
%%
%% @spec parse_request(binary() | string(), optlist()) ->
%%   {ok, request, xmlrpc_request()} | {error, Reason}

-spec parse_request(binary() | string(), optlist()) ->
  {ok, request, xmlrpc_request()} | {error, term()}.

parse_request(XML, Opts) ->
  case parse(XML, Opts) of
    {ok, request, _Data} = Result ->
      Result;
    {ok, result, _Data} ->
      {error, not_request};
    {ok, exception, _Data} ->
      {error, not_request};
    {error, _Reason} = Error ->
      Error
  end.

%% @doc Parse XML message to result or exception.
%%
%% @spec parse_response(binary() | string(), optlist()) ->
%%     {ok, result,    Result  :: xmlrpc_result()}
%%   | {ok, exception, Message :: xmlrpc_exception()}
%%   | {error, Reason}

-spec parse_response(binary() | string(), optlist()) ->
    {ok, result,    xmlrpc_result()}
  | {ok, exception, xmlrpc_exception()}
  | {error, term()}.

parse_response(XML, Opts) ->
  case parse(XML, Opts) of
    {ok, request, _Data} ->
      {error, not_response};
    {ok, result, _Data} = Result ->
      Result;
    {ok, exception, _Data} = Result ->
      Result;
    {error, _Reason} = Error ->
      Error
  end.

%%% }}}
%%%---------------------------------------------------------------------------
%%% support functions {{{

%%----------------------------------------------------------
%% encode_value(Value) {{{

%% @doc Recursively convert value to XML node for exporting through xmerl.
%%
%% @TODO base64 support
%%
%% @spec encode_value(Value :: jsx_value()) ->
%%   #xmlElement{}

-spec encode_value(jsx_value()) ->
  #xmlElement{}.

encode_value([{_,_} | _Rest] = Value) ->
  Members = [
    e(member, [
      e(name, text(name_to_string(N))),
      e(value, encode_value(V))
    ]) || {N,V} <- Value
  ],
  e(struct, Members);
encode_value([{}] = _Value) ->
  e(struct, []);
encode_value(Value) when is_list(Value) ->
  EncodedValues = [e(value, encode_value(E)) || E <- Value],
  ArrayData = e(data, EncodedValues),
  e(array, ArrayData);
encode_value(Value) when is_binary(Value) ->
  e(string, text(Value));
encode_value(Value) when is_integer(Value) ->
  e(i4, text(integer_to_list(Value)));
encode_value(Value) when is_float(Value) ->
  e(double, text(float_to_list(Value)));
encode_value(true) ->
  e(boolean, text("1"));
encode_value(false) ->
  e(boolean, text("0"));
encode_value(null) ->
  e(nil, []);
encode_value(Value) when is_atom(Value) ->
  e(string, text(atom_to_list(Value))).

%% }}}
%%----------------------------------------------------------
%% rest of user data -> xmerl structures {{{

%% @doc Create XML element for exporting through xmerl.
%%
%% @spec e(atom(), [xml_node()] | xml_node()) ->
%%   #xmlElement{}

-spec e(atom(), [xml_node()] | xml_node()) ->
  #xmlElement{}.

e(Tag, Content) when is_list(Content) ->
  #xmlElement{ name = Tag, content = Content};
e(Tag, Content) ->
  #xmlElement{ name = Tag, content = [Content]}.

%% @doc Create text content node for XML element (for {@link e/2}, for
%%   example).
%%
%% @spec text(iolist() | binary()) ->
%%   #xmlText{}

-spec text(iolist() | binary()) ->
  #xmlText{}.

text(Text) ->
  #xmlText{value = [Text]}.

%% @doc Convert name (atom, list or binary) to string.
%%
%% @spec name_to_string(atom() | binary() | string()) ->
%%   binary() | string()

-spec name_to_string(atom() | binary() | string()) ->
  binary() | string().

name_to_string(Name) when is_atom(Name) ->
  atom_to_list(Name);
name_to_string(Name) when is_binary(Name) orelse is_list(Name) ->
  Name.

%% }}}
%%----------------------------------------------------------
%% decode_document(D) {{{

%% @doc Decode XML-RPC request or response document.
%%
%%   Note that exception raised on server side is returned as a well-formed
%%   XML response. This is `{ok, exception, ...}' case.
%%
%%   `{error, Reason}' is only returned when error in XML was found, not when
%%   remote procedure failed, and indicates protocol error instead of
%%   application error.
%%
%% @spec decode_document(XMLElement :: #xmlElement{}) ->
%%     {ok, request,   xmlrpc_request()}
%%   | {ok, result,    xmlrpc_result()}
%%   | {ok, exception, xmlrpc_exception()}
%%   | {error, Reason}

-spec decode_document(#xmlElement{}) ->
    {ok, request,   xmlrpc_request()}
  | {ok, result,    xmlrpc_result()}
  | {ok, exception, xmlrpc_exception()}
  | {error, term()}.

decode_document(#xmlElement{ name = methodCall, content = Children }) ->
  % for method call, two children to expect: `<methodName/>' and `<params/>',
  % in any order(?)
  case [E || #xmlElement{} = E <- Children] of
    [#xmlElement{name = methodName, content = [#xmlText{value = Name}]},
      #xmlElement{name = params, content = Params}] ->
      try
        ParamsDecoded = [
          % decode_value_rec() doesn't catch "badly formed XML" tuples
          decode_value_rec(extract_child(ValE)) ||
          #xmlElement{name = param, content = ParamE} = _P <- Params,
          #xmlElement{name = value, content = ValE}   = _V <- ParamE
        ],
        {ok, request, {list_to_binary(Name), ParamsDecoded}}
      catch
        throw:{error,  E} ->
          {error, E}
      end;
    [#xmlElement{name = params, content = Params},
      #xmlElement{name = methodName, content = [#xmlText{value = Name}]}] ->
      try
        ParamsDecoded = [
          % decode_value_rec() doesn't catch "badly formed XML" tuples
          decode_value_rec(extract_child(ValE)) ||
          #xmlElement{name = param, content = ParamE} = _P <- Params,
          #xmlElement{name = value, content = ValE}   = _V <- ParamE
        ],
        {ok, request, {list_to_binary(Name), ParamsDecoded}}
      catch
        throw:{error,  E} ->
          {error, E}
      end;
    _Any ->
      {error, bad_xml_structure}
  end;

decode_document(#xmlElement{name = methodResponse, content = TopChildren}) ->
  % either `<params/>' (successful call) or `<fault/>' (exception)
  case [E || #xmlElement{} = E <- TopChildren] of
    [#xmlElement{name = params, content = Children}] ->
      % decoding requires stripping non-tag children
      case decode_results([E || #xmlElement{} = E <- Children]) of
        {error, _Reason} = Error ->
          % decode_results() normally doesn't return tuple
          Error;
        Result ->
          {ok, result, Result}
      end;
    [#xmlElement{name = fault, content = Children}] ->
      % decoding requires stripping non-tag children
      case decode_faults([E || #xmlElement{} = E <- Children]) of
        {error, _Reason} = Error ->
          Error;
        {_Code, _Message} = Exception ->
          {ok, exception, Exception}
      end;
    _Any ->
      {error, bad_xml_structure}
  end.


%% @doc Decode XML-RPC (successful call) reply.
%%
%%   Expect content of `<params/>' tag (i.e. single `<param/>').
%%
%% @spec decode_results(XMLElement :: #xmlElement{}) ->
%%   jsx_value() | {error, Reason}

-spec decode_results([#xmlElement{}]) ->
  jsx_value() | {error, term()}.

decode_results([#xmlElement{name = param, content = Children}] = _Elements) ->
  case [E || #xmlElement{name = value} = E <- Children] of
    [#xmlElement{name = value, content = Values}] ->
      Vals = [V || #xmlElement{} = V <- Values],
      case Vals of
        [Value] ->
          decode_value(Value);
        _Any ->
          {error, bad_xml_structure}
      end;
    _Any ->
      {error, bad_xml_structure}
  end;
decode_results(_Any) ->
  {error, bad_xml_structure}.

%% @doc Decode XML-RPC exception.
%%
%%   Expect content of `<fault/>' tag (i.e. single `<struct/>' with two
%%   members: `faultString' and `faultCode', in any order).
%%
%% @spec decode_faults([#xmlElement{}]) ->
%%   {Code :: integer(), Message :: binary()} | {error, Reason}

-spec decode_faults([#xmlElement{}]) ->
  {integer(), binary()} | {error, term()}.

decode_faults([#xmlElement{name = value, content = Children}] = _Elements) ->
  case [E || #xmlElement{} = E <- Children] of
    [#xmlElement{name = struct} = FaultStruct] ->
      Struct = decode_value(FaultStruct),
      case Struct of
        [{<<"faultString">>, Message}, {<<"faultCode">>, Code}] ->
          {Code, Message};
        [{<<"faultCode">>, Code}, {<<"faultString">>, Message}] ->
          {Code, Message};
        _Any ->
          {error, bad_xml_structure}
      end;
    _Any ->
      {error, bad_xml_structure}
  end;
decode_faults(_Any) ->
  {error, bad_xml_structure}.

%% @doc Decode single value (top-level function).
%%
%%   This function calls {@link decode_value_rec/1} and catches any error, to
%%   make error messages more readable.
%%
%% @spec decode_value(XMLElement :: #xmlElement{}) ->
%%   jsx_value() | {error, Reason}

-spec decode_value(#xmlElement{}) ->
  jsx_value().

decode_value(V) ->
  try
    decode_value_rec(V)
  catch
    throw:{error,  E} ->
      {error, E};
    error:_Any ->
      {error, bad_xml}
  end.

%% @doc Decode single value (scalar, array or struct) recursively.
%%
%% @spec decode_value_rec(XMLElement :: #xmlElement{}) ->
%%   jsx_value()

-spec decode_value_rec(#xmlElement{}) ->
  jsx_value().

decode_value_rec(#xmlElement{name = i4} = E) ->
  decode_int(E);
decode_value_rec(#xmlElement{name = int} = E) ->
  decode_int(E);
decode_value_rec(#xmlElement{name = boolean} = E) ->
  decode_boolean(E);
decode_value_rec(#xmlElement{name = string} = E) ->
  decode_string(E);
decode_value_rec(#xmlElement{name = double} = E) ->
  decode_double(E);
%decode_value_rec(#xmlElement{name = dateTime.iso8601} = E) ->
%  decode_dateTime_iso8601(E);
%decode_value_rec(#xmlElement{name = base64} = E) ->
%  decode_base64(E);
decode_value_rec(#xmlElement{name = nil} = E) ->
  decode_nil(E);
decode_value_rec(#xmlElement{name = struct} = E) ->
  decode_struct(E);
decode_value_rec(#xmlElement{name = array} = E) ->
  decode_array(E).

%% @doc Decode integer value.
%%
%% @spec decode_int(XMLElement :: #xmlElement{}) ->
%%   integer()

-spec decode_int(#xmlElement{}) ->
  integer().

decode_int(#xmlElement{content = [#xmlText{value = Value}]} = _V) ->
  list_to_integer(Value).

%% @doc Decode boolean value.
%%
%% @spec decode_boolean(XMLElement :: #xmlElement{}) ->
%%   boolean()

-spec decode_boolean(#xmlElement{}) ->
  boolean().

decode_boolean(#xmlElement{content = [#xmlText{value = "0"}]} = _V) ->
  false;
decode_boolean(#xmlElement{content = [#xmlText{value = "1"}]} = _V) ->
  true.

%% @doc Decode string value.
%%
%% @spec decode_string(XMLElement :: #xmlElement{}) ->
%%   binary()

-spec decode_string(#xmlElement{}) ->
  binary().

decode_string(#xmlElement{content = []} = _V) ->
  <<>>;
decode_string(#xmlElement{content = [#xmlText{value = Value}]} = _V) ->
  list_to_binary(Value).

%% @doc Decode double value.
%%
%% @spec decode_double(XMLElement :: #xmlElement{}) ->
%%   float()

-spec decode_double(#xmlElement{}) ->
  float().

decode_double(#xmlElement{content = [#xmlText{value = Value}]} = _V) ->
  list_to_float(Value).

%decode_dateTime_iso8601(V) -> V.
%decode_base64(V) -> V.

%% @doc Decode null value.
%%
%% @spec decode_nil(XMLElement :: #xmlElement{}) ->
%%   null

-spec decode_nil(#xmlElement{}) ->
  null.

decode_nil(_V) ->
  null.

%% XXX: unused, left as a comment for completeness
%% ```
%%   <value> ... </value>
%% '''
%decode_scalar(V) -> V.

%% @doc Decode struct (sequence of key-value pairs).
%%
%%   Function recursively decodes struct content.
%%
%% @spec decode_struct(XMLElement :: #xmlElement{}) ->
%%   jsx_hash()
%%
%% @end

-spec decode_struct(#xmlElement{}) ->
  jsx_hash().

%% ```
%%   <struct>
%%     <member>
%%       <name>String</name>
%%       <value> ... </value>
%%     </member>
%%     ...
%%   </struct>
%% '''
decode_struct(#xmlElement{content = MembersText} = _V) ->
  Members = [M || #xmlElement{name = member} = M <- MembersText],
  case Members of
    [] -> [{}];
    _  -> [extract_struct_member(M) || M <- Members]
  end.

%% @doc Extract and decode single key-value pair in struct.
%%
%%   Function recursively decodes value.
%%
%%   Function throws `{error, bad_xml_structure}' when XML document is not
%%   well-formed.
%%
%% @spec extract_struct_member(XMLElement :: #xmlElement{}) ->
%%   {jsx_hash_key(), jsx_value()}

-spec extract_struct_member(#xmlElement{}) ->
  {jsx_hash_key(), jsx_value()}.

extract_struct_member(#xmlElement{content = NameValueText} = _Member) ->
  NameValue = [E || #xmlElement{} = E <- NameValueText],
  case NameValue of
    [NameE, #xmlElement{name = value, content = Children} = _ValueE] ->
      Name  = decode_string(NameE),
      Value = decode_value_rec(extract_child(Children)),
      {Name, Value};
    _Any -> throw({error, bad_xml_structure})
  end.

%% @doc Decode array (sequence of values).
%%
%%   Function recursively decodes array content.
%%
%% @spec decode_array(XMLElement :: #xmlElement{}) ->
%%   [jsx_value()]
%%
%% @end

-spec decode_array(#xmlElement{}) ->
  [jsx_value()].

%% ```
%%   <array>
%%     <data>
%%       <value> ... </value>
%%       ...
%%     </data>
%%   </array>
%% '''
decode_array(#xmlElement{content = ArrayData} = _V) ->
  #xmlElement{name = data, content = ValueList} = extract_child(ArrayData),
  Values = [
    decode_value_rec(extract_child(Vals)) ||
    #xmlElement{name = value, content = Vals} = _ValE <- ValueList
  ],
  Values.

%% @doc Extract the single child from list of children (or throw an error).
%%
%%   Function clears any `#xmlText{}', leaving `#xmlElement{}' elements.
%%
%%   If result contains just one element, that element is returned. Otherwise,
%%   function throws `{error, bad_xml_structure}' tuple.
%%
%% @spec extract_child([xml_node()]) ->
%%   #xmlElement{}

-spec extract_child([xml_node()]) ->
  #xmlElement{}.

extract_child(Children) ->
  case [E || #xmlElement{} = E <- Children] of
    [Child] -> Child;
    _Any -> throw({error, bad_xml_structure})
  end.

%% }}}
%%----------------------------------------------------------

%%% }}}
%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
