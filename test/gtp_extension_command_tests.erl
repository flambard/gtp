-module(gtp_extension_command_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("gtp.hrl").

roundtrip_test() ->
    {ok, ChannelA} = gtp_erlang_channel:start_link(),
    {ok, ChannelB} = gtp_erlang_channel:start_link(ChannelA),

    {ok, Engine} = gtp_engine_channel:start_link(
        gtp_bogus_engine, make_ref(), gtp_erlang_channel, ChannelB, []
    ),

    ok = gtp_engine_channel:register_extension_commands(Engine, #{
        <<"test-echo">> => gtp_echo_command
    }),

    {ok, Controller} = gtp_controller_channel:start_link(gtp_erlang_channel, ChannelA, []),

    Message = <<"PING">>,
    Command = gtp_echo_command:new(Message),

    {ok, #success{values = #{value := Message}}} =
        gtp_controller_channel:send_command(Controller, Command, [
            {id, 1},
            {command_module, gtp_echo_command}
        ]).
