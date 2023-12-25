-module(gtp_types).

-include("gtp.hrl").

-export([
    encode/1,
    encode_int/1,
    decode_int/1,
    encode_float/1,
    decode_float/1,
    encode_boolean/1,
    decode_boolean/1
]).

encode(Int) when is_integer(Int) ->
    integer_to_binary(Int);
encode(Float) when is_float(Float) ->
    float_to_binary(Float);
encode(true) ->
    <<"true">>;
encode(false) ->
    <<"false">>;
encode(white) ->
    <<"white">>;
encode(black) ->
    <<"black">>;
encode({Letter, Number}) ->
    %% Vertex
    [atom_to_binary(Letter), integer_to_binary(Number)];
encode(#move{color = C, vertex = V}) ->
    [encode(C), " ", encode(V)];
encode(Tuple) when is_tuple(Tuple) ->
    encode(tuple_to_list(Tuple));
encode(List) when is_list(List) ->
    lists:join(" ", [encode(E) || E <- List]);
encode(String) when is_binary(String) ->
    String.

%%%
%%%
%%%

encode_int(Integer) ->
    integer_to_binary(Integer).

decode_int(Binary) ->
    binary_to_integer(Binary).

encode_float(Float) ->
    float_to_binary(Float).

decode_float(Binary) ->
    binary_to_float(Binary).

encode_boolean(true) -> <<"true">>;
encode_boolean(false) -> <<"false">>.

decode_boolean(<<"true">>) -> true;
decode_boolean(<<"false">>) -> false.
