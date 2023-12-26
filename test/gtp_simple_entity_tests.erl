-module(gtp_simple_entity_tests).
-include_lib("eunit/include/eunit.hrl").

encode_int_test() ->
    <<"1337">> = gtp_types:encode_int(1337).

decode_int_test() ->
    {3, [<<"black">>]} = gtp_types:decode_int(<<"3 black">>).
