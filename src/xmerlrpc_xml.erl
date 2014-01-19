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

%%% }}}
%%%---------------------------------------------------------------------------
%%% support functions {{{

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

%%% }}}
%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
