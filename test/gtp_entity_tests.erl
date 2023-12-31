-module(gtp_entity_tests).
-include_lib("eunit/include/eunit.hrl").

encode_int_test() ->
    <<"1337">> = gtp_entity:encode(int, 1337).

decode_int_test() ->
    {3, [<<"black">>]} = gtp_entity:decode(int, <<"3 black">>).

encode_vertex_test() ->
    EncodedVertex = gtp_entity:encode(vertex, {j, 11}),
    <<"j11">> = iolist_to_binary(EncodedVertex).

decode_vertex_test() ->
    {{j, 11}, []} = gtp_entity:decode(vertex, <<"j11">>).

decode_uppercase_vertex_test() ->
    {{g, 10}, []} = gtp_entity:decode(vertex, <<"G10">>).

encode_pass_test() ->
    EncodedVertex = gtp_entity:encode(vertex, pass),
    <<"pass">> = iolist_to_binary(EncodedVertex).

decode_pass_test() ->
    {pass, []} = gtp_entity:decode(vertex, <<"pass">>).

encode_string_test() ->
    EncodedString = gtp_entity:encode(string, "Deadmau5"),
    <<"Deadmau5">> = iolist_to_binary(EncodedString).

decode_string_test() ->
    {<<"Hello">>, [<<"world">>]} = gtp_entity:decode(string, <<"Hello world">>).

encode_collection_test() ->
    EncodedValues = gtp_entity:encode([color, int, int], [black, 30, 0]),
    <<"black 30 0">> = iolist_to_binary(EncodedValues).

decode_collection_test() ->
    {[black, 30, 0], []} = gtp_entity:decode([color, int, int], <<"black 30 0">>).

encode_list_of_ints_test() ->
    EncodedList = gtp_entity:encode({list, int}, [9, 8, 7, 6, 5, 4, 3]),
    <<"9 8 7 6 5 4 3">> = iolist_to_binary(EncodedList).

decode_list_of_ints_test() ->
    {[9, 8, 7, 6, 5, 4, 3], []} = gtp_entity:decode({list, int}, <<"9 8 7 6 5 4 3">>).

encode_alternative_1_test() ->
    EncodedValue = gtp_entity:encode({alternative, vertex, string}, {k, 3}),
    <<"k3">> = iolist_to_binary(EncodedValue).

encode_alternative_2_test() ->
    EncodedValue = gtp_entity:encode({alternative, vertex, string}, "resign"),
    <<"resign">> = iolist_to_binary(EncodedValue).

decode_alternative_1_test() ->
    {{k, 3}, []} = gtp_entity:decode({alternative, vertex, string}, <<"k3">>).

decode_alternative_2_test() ->
    {<<"resign">>, []} = gtp_entity:decode({alternative, vertex, string}, <<"resign">>).

encode_multiline_string_test() ->
    EncodedString = gtp_entity:encode({multiline, string}, ["Ein", "Zwei", "Drei"]),
    <<"Ein\nZwei\nDrei">> = iolist_to_binary(EncodedString).

encode_multiline_list_of_strings_test() ->
    EncodedString = gtp_entity:encode({multiline, {list, string}}, [
        ["One", "Ring", "to", "rule", "them", "all"],
        ["One", "Ring", "to", "find", "them"],
        ["One", "Ring", "to", "bring", "them", "all"],
        ["and", "in", "the", "darkness", "bind", "them"]
    ]),
    <<"One Ring to rule them all\nOne Ring to find them\nOne Ring to bring them all\nand in the darkness bind them">> =
        iolist_to_binary(EncodedString).

decode_multiline_string_test() ->
    [<<"Ein">>, <<"Zwei">>, <<"Drei">>] =
        gtp_entity:decode({multiline, string}, [<<"Ein">>, <<"Zwei">>, <<"Drei">>]).

decode_multiline_list_of_strings_test() ->
    [
        [<<"One">>, <<"Ring">>, <<"to">>, <<"rule">>, <<"them">>, <<"all">>],
        [<<"One">>, <<"Ring">>, <<"to">>, <<"find">>, <<"them">>],
        [<<"One">>, <<"Ring">>, <<"to">>, <<"bring">>, <<"them">>, <<"all">>],
        [<<"and">>, <<"in">>, <<"the">>, <<"darkness">>, <<"bind">>, <<"them">>]
    ] = gtp_entity:decode({multiline, {list, string}}, [
        <<"One Ring to rule them all">>,
        <<"One Ring to find them">>,
        <<"One Ring to bring them all">>,
        <<"and in the darkness bind them">>
    ]).
