-module(gtp_entity_tests).
-include_lib("eunit/include/eunit.hrl").

encode_int_test() ->
    <<"1337">> = gtp_entity:encode_int(1337).

decode_int_test() ->
    {3, [<<"black">>]} = gtp_entity:decode_int(<<"3 black">>).

encode_vertex_test() ->
    EncodedVertex = gtp_entity:encode_vertex({j, 11}),
    <<"j11">> = iolist_to_binary(EncodedVertex).

decode_vertex_test() ->
    {{j, 11}, []} = gtp_entity:decode_vertex(<<"j11">>).

encode_list_of_ints_test() ->
    EncodedList = gtp_entity:encode_list(fun gtp_entity:encode_int/1, [9, 8, 7, 6, 5, 4, 3]),
    <<"9 8 7 6 5 4 3">> = iolist_to_binary(EncodedList).

decode_list_of_ints_test() ->
    [9, 8, 7, 6, 5, 4, 3] =
        gtp_entity:decode_list(fun gtp_entity:decode_int/1, <<"9 8 7 6 5 4 3">>).