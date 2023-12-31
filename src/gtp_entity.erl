-module(gtp_entity).
-include("gtp.hrl").

-export([
    encode/2,
    decode/2,
    decode_line/2,
    decode_multiline/2
]).

%%%
%%% Encoding
%%%

-spec encode(entity_type(), entity_value()) -> iodata().

encode(int, Int) ->
    integer_to_binary(Int);
encode(float, Float) ->
    float_to_binary(Float);
encode(boolean, true) ->
    <<"true">>;
encode(boolean, false) ->
    <<"false">>;
encode(color, black) ->
    <<"black">>;
encode(color, white) ->
    <<"white">>;
encode(vertex, {Letter, Number}) ->
    [atom_to_binary(Letter), integer_to_binary(Number)];
encode(vertex, pass) ->
    <<"pass">>;
encode(move, #move{color = C, vertex = V}) ->
    encode([color, vertex], [C, V]);
encode(string, String) ->
    String;
encode(Types, Values) when is_list(Types) ->
    [lists:join(<<" ">>, lists:zipwith(fun encode/2, Types, Values))];
encode({alternative, Type1, Type2}, Value) ->
    try encode(Type1, Value) of
        EncodedValue -> EncodedValue
    catch
        error:_Error -> encode(Type2, Value)
    end;
encode({list, Type}, List) ->
    lists:join(<<" ">>, [encode(Type, E) || E <- List]);
encode({multiline, Type}, List) ->
    lists:join(<<"\n">>, [encode(Type, E) || E <- List]).

%%%
%%% Decoding
%%%

-spec decode(singleline_entity_type(), binary()) -> {entity_value(), [binary()]}.

decode(int, Binary) ->
    [IntBin | Rest] = binary:split(Binary, <<" ">>, [trim]),
    {binary_to_integer(IntBin), Rest};
decode(float, Binary) ->
    [FloatBin | Rest] = binary:split(Binary, <<" ">>, [trim]),
    {binary_to_float(FloatBin), Rest};
decode(boolean, Binary) ->
    case binary:split(Binary, <<" ">>, [trim]) of
        [<<"true">> | Rest] -> {true, Rest};
        [<<"false">> | Rest] -> {false, Rest}
    end;
decode(color, Binary) ->
    case binary:split(Binary, <<" ">>, [trim]) of
        [<<"black">> | Rest] -> {black, Rest};
        [<<"white">> | Rest] -> {white, Rest}
    end;
decode(vertex, <<"pass", _/binary>> = Bin) ->
    {<<"pass">>, Rest} = decode(string, Bin),
    {pass, Rest};
decode(vertex, <<Letter:1/binary, Number/binary>>) ->
    L = binary_to_atom(string:lowercase(Letter)),
    {N, Rest} = decode(int, Number),
    {{L, N}, Rest};
decode(string, Binary) ->
    [String | Rest] = binary:split(Binary, <<" ">>, []),
    {String, Rest};
decode(Types, Binary) when is_list(Types) ->
    FoldingFun = fun(Type, {Decoded, [Bin]}) ->
        {Value, Rest} = decode(Type, Bin),
        {[Value | Decoded], Rest}
    end,
    {Values, Rest} = lists:foldl(FoldingFun, {[], [Binary]}, Types),
    {lists:reverse(Values), Rest};
decode({alternative, Type1, Type2}, Binary) ->
    try decode(Type1, Binary) of
        DecodedValue -> DecodedValue
    catch
        error:_Error -> decode(Type2, Binary)
    end;
decode({list, Type}, Binary) ->
    DecodedList = decode_list_of(Type, Binary),
    {DecodedList, []}.

-spec decode_line(singleline_entity_type(), binary()) -> entity_value().

decode_line(Type, Binary) ->
    {Value, []} = decode(Type, Binary),
    Value.

-spec decode_multiline(singleline_entity_type(), [binary()]) -> [entity_value()].

decode_multiline(Type, Lines) ->
    lists:map(fun(Binary) -> decode_line(Type, Binary) end, Lines).

%%%
%%% Private functions
%%%

decode_list_of(Type, Binary) ->
    case decode(Type, Binary) of
        {Value, []} -> [Value];
        {Value, [Rest]} -> [Value | decode_list_of(Type, Rest)]
    end.
