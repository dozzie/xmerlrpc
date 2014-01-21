%%%---------------------------------------------------------------------------
%%% @doc
%%%   Parsing and forming XML-RPC messages for xmerlrpc.
%%%
%%%   Module uses data structures after `jsx' application, which in turn uses
%%%   <a href="http://www.erlang.org/eeps/eep-0018.html">EEP-18</a>
%%%   structures.
%%%
%%% @TODO
%%%   Jiffy-like structures.
%%% @TODO
%%%   Cleanup types documentation.
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(xmerlrpc_xml).

-include_lib("xmerl/include/xmerl.hrl").

-export([request/3, result/2, exception/3]).
-export([parse/2, parse_request/2, parse_response/2]).

-export([decode_document/1]).

%%%---------------------------------------------------------------------------
%%% types {{{

%% @type proc_name() = string() | binary() | atom().

-type proc_name() :: string() | binary() | atom().

%% @type proc_arg() = jsx_array() | jsx_hash() | jsx_scalar().

-type proc_arg() :: jsx_array() | jsx_hash() | jsx_scalar().

%%----------------------------------------------------------
%% jsx-style types {{{

%% @type jsx_hash() = [{}] | [{proc_name(), proc_arg()}].

-type jsx_hash() :: [{}] | [{proc_name(), proc_arg()}, ...].

%% @type jsx_array() = [proc_arg()].

-type jsx_array() :: [proc_arg()].

%% @type jsx_scalar() = binary() | number() | null | true | false.

-type jsx_scalar() :: binary() | number() | null | true | false.

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
%% @spec request(proc_name(), [proc_arg()], Opts) ->
%%   iolist()

request(ProcName, ProcParams, _Opts) ->
  XMLCall = e(methodCall, [
    e(methodName, [text(name_to_string(ProcName))]),
    e(params, [ e(param, e(value, encode_value(P))) || P <- ProcParams ])
  ]),
  xmerl:export([XMLCall], xmerl_xml).

%% @doc Form XML document carrying call result (function reply).
%%
%% @spec result(proc_arg(), Opts) ->
%%   iolist()

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
%% @TODO
%%   Make `Message' iolist() instead of binary().
%%
%% @spec exception(integer(), binary(), Opts) ->
%%   iolist()

exception(Code, Message, _Opts) ->
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
%%% XML parsing (TODO) {{{

%% @doc Parse XML message to request, result or exception.
%%
%% @spec parse(binary() | string(), Opts) ->
%%     {ok, request,   Request :: any()}
%%   | {ok, result,    Result  :: any()}
%%   | {ok, exception, Message :: any()}
%%   | {error, Reason}

parse(XML, Opts) when is_binary(XML) ->
  parse(binary_to_list(XML), Opts);
parse(XML, _Opts) when is_list(XML) ->
  {Document, _Rest} = xmerl_scan:string(XML),
  decode_document(Document).

%% @doc Parse XML message to request.
%%
%% @spec parse_request(binary() | string(), Opts) ->
%%     {ok, request, Request :: any()}
%%   | {error, Reason}

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
%% @spec parse_response(binary() | string(), Opts) ->
%%     {ok, result,    Result  :: any()}
%%   | {ok, exception, Message :: any()}
%%   | {error, Reason}

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
%% @TODO
%%   base64 support
%%
%% @spec encode_value(Value :: proc_arg()) ->
%%   #xmlElement{}

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

e(Tag, Content) when is_list(Content) ->
  #xmlElement{ name = Tag, content = Content};
e(Tag, Content) ->
  #xmlElement{ name = Tag, content = [Content]}.

%% @doc Create text content node for XML element (for {@link e/2}, for
%%   example).
%%
%% @spec text(iolist()) ->
%%   #xmlText{}

text(Text) ->
  #xmlText{value = [Text]}.

%% @doc Convert name (atom, list or binary) to string.
%%
%% @spec name_to_string(atom() | binary() | string()) ->
%%   binary() | string()

name_to_string(Name) when is_atom(Name) ->
  atom_to_list(Name);
name_to_string(Name) when is_binary(Name) orelse is_list(Name) ->
  Name.

%% }}}
%%----------------------------------------------------------
%% decode_document(D) {{{

%% TODO: rewrite this to SAX-style parser

decode_document(#xmlElement{ name = methodCall, content = Children }) ->
  case Children of
    [#xmlElement{name = methodName, content = [#xmlText{value = Name}]},
      #xmlElement{name = params, content = Params}] ->
      {ok, request, {Name, Params}};
    [#xmlElement{name = params, content = Params},
      #xmlElement{name = methodName, content = [#xmlText{value = Name}]} ] ->
      {ok, request, {Name, Params}};
    _Any ->
      {error, bad_xml_structure}
  end;

decode_document(#xmlElement{name = methodResponse, content = TopChildren}) ->
  % filter out #xmlText{}, leave just #xmlElement{} children
  case [E || #xmlElement{} = E <- TopChildren] of
    [#xmlElement{name = params, content = Children}] ->
      case decode_results([E || #xmlElement{} = E <- Children]) of
        {error, _Reason} = Error ->
          Error;
        Result ->
          {ok, result, Result}
      end;
    [#xmlElement{name = fault, content = Children}] ->
      case decode_faults([E || #xmlElement{} = E <- Children]) of
        {error, _Reason} = Error ->
          Error;
        {_Code, _Message} = Exception ->
          {ok, exception, Exception}
      end;
    _Any ->
      {error, bad_xml_structure}
  end.

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

decode_value(V) ->
  try
    decode_value_rec(V)
  catch
    throw:{result, R} -> R;
    throw:{error,  E} -> {error, E};
    error:_Any ->
      io:fwrite("!! ~p~n", [_Any]),
      {error, bad_xml}
  end.

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

decode_int(#xmlElement{content = [#xmlText{value = Value}]} = _V) ->
  list_to_integer(Value).
decode_boolean(#xmlElement{content = [#xmlText{value = "0"}]} = _V) ->
  false;
decode_boolean(#xmlElement{content = [#xmlText{value = "1"}]} = _V) ->
  true.
decode_string(#xmlElement{content = []} = _V) ->
  <<>>;
decode_string(#xmlElement{content = [#xmlText{value = Value}]} = _V) ->
  list_to_binary(Value).
decode_double(#xmlElement{content = [#xmlText{value = Value}]} = _V) ->
  list_to_float(Value).
%decode_dateTime_iso8601(V) -> V.
%decode_base64(V) -> V.
decode_nil(_V) ->
  null.

%% ```
%%   <value> ... </value>
%% '''
%decode_scalar(V) -> V.

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
    _  -> [extract_member(M) || M <- Members]
  end.

extract_member(#xmlElement{content = NameValueText} = _Member) ->
  NameValue = [E || #xmlElement{} = E <- NameValueText],
  case NameValue of
    [NameE, #xmlElement{name = value, content = Children} = _ValueE] ->
      Name  = decode_string(NameE),
      Value = decode_value_rec(extract_child(Children)),
      {Name, Value};
    _Any -> throw({error, bad_xml_structure})
  end.

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


extract_child(Children) ->
  case [E || #xmlElement{} = E <- Children] of
    [Child] -> Child;
    _Any -> throw({error, bad_xml_structure})
  end.

%% }}}
%%----------------------------------------------------------
%% rest of XML -> data {{{

%% }}}
%%----------------------------------------------------------

%%% }}}
%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
