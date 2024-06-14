-module(gtp_controller_io_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gtp.hrl").

io_test() ->
    IoServer =
        spawn(fun() ->
            receive
                {io_request, From1, ReplyAs1, {put_chars, latin1, <<"protocol_version\n">>}} ->
                    From1 ! {io_reply, ReplyAs1, ok}
            end,
            receive
                {io_request, From2, ReplyAs2, {get_line, latin1, _Prompt1}} ->
                    From2 ! {io_reply, ReplyAs2, <<"= 2\n">>}
            end,
            receive
                {io_request, From3, ReplyAs3, {get_line, latin1, _Prompt2}} ->
                    From3 ! {io_reply, ReplyAs3, <<"\n">>}
            end
        end),

    {ok, CC} = gtp_controller_channel:start_link(IoServer, []),

    {ok, #success{values = #{version_number := 2}}} =
        gtp_controller_channel:send_command(CC, #protocol_version{}).
