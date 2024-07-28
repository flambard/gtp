-module(gtp_extension_command_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gtp.hrl").

roundtrip_test() ->
    {ok, CrossoverIO} = crossover_io_server:start_link([]),

    {ok, Engine} = gtp_bogus_engine:start_link(),

    {ok, EngineChannel} =
        gtp_engine_channel:start_link(gtp_bogus_engine, Engine, CrossoverIO, []),

    ok =
        gtp_engine_channel:register_extension_commands(
            EngineChannel,
            #{<<"test-echo">> => gtp_echo_command}
        ),

    {ok, ControllerChannel} = gtp_controller_channel:start_link(CrossoverIO, []),

    Message = <<"PING">>,
    Command = gtp_echo_command:new(Message),

    {ok, #success{values = #{value := Message}}} =
        gtp_controller_channel:send_command(
            ControllerChannel,
            Command,
            [{id, 1}, {command_module, gtp_echo_command}]
        ).
