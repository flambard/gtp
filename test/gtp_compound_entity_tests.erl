-module(gtp_compound_entity_tests).
-include_lib("eunit/include/eunit.hrl").

encode_list_of_ints_test() ->
    EncodedList = gtp_types:encode_list(fun gtp_types:encode_int/1, [9, 8, 7, 6, 5, 4, 3]),
    <<"9 8 7 6 5 4 3">> = iolist_to_binary(EncodedList).

decode_list_of_ints_test() ->
    [9, 8, 7, 6, 5, 4, 3] = gtp_types:decode_list(fun gtp_types:decode_int/1, <<"9 8 7 6 5 4 3">>).
