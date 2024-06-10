-module(gtp_command_response_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gtp.hrl").

roundtrip_test() ->
    {ok, CrossoverIO} = gtp_crossover_io_server:start_link([]),

    {ok, Engine} = gtp_bogus_engine:start_link(),

    {ok, _EngineChannel} =
        gtp_engine_channel:start_link(gtp_bogus_engine, Engine, CrossoverIO, []),

    {ok, ControllerChannel} = gtp_controller_channel:start_link(CrossoverIO, []),

    {ok, #success{values = #{version_number := 2}}} =
        gtp_controller_channel:send_command(ControllerChannel, #protocol_version{}, [{id, 1}]).

shutdown_test() ->
    {ok, CrossoverIO} = gtp_crossover_io_server:start_link([]),

    {ok, Engine} = gtp_bogus_engine:start_link(),

    {ok, EngineChannel} =
        gtp_engine_channel:start_link(gtp_bogus_engine, Engine, CrossoverIO, []),

    {ok, ControllerChannel} = gtp_controller_channel:start_link(CrossoverIO, []),

    {ok, #success{values = #{}}} =
        gtp_controller_channel:send_command(ControllerChannel, #quit{}),

    false = is_process_alive(EngineChannel),
    false = is_process_alive(Engine),
    false = is_process_alive(ControllerChannel).

command_with_single_argument_test() ->
    {ok, CrossoverIO} = gtp_crossover_io_server:start_link([]),

    {ok, Engine} = gtp_bogus_engine:start_link(),

    {ok, _EngineChannel} =
        gtp_engine_channel:start_link(gtp_bogus_engine, Engine, CrossoverIO, []),

    {ok, ControllerChannel} = gtp_controller_channel:start_link(CrossoverIO, []),

    {ok, #success{values = #{known := true}}} =
        gtp_controller_channel:send_command(ControllerChannel,
                                            #known_command{command_name = <<"known_command">>}).

command_with_multiple_arguments_test() ->
    {ok, CrossoverIO} = gtp_crossover_io_server:start_link([]),

    {ok, Engine} = gtp_bogus_engine:start_link(),

    {ok, _EngineChannel} =
        gtp_engine_channel:start_link(gtp_bogus_engine, Engine, CrossoverIO, []),

    {ok, ControllerChannel} = gtp_controller_channel:start_link(CrossoverIO, []),

    {ok, #failure{error_message = <<"unknown command">>}} =
        gtp_controller_channel:send_command(ControllerChannel,
                                            #time_left{color = black,
                                                       time = 30,
                                                       stones = 0}).

command_with_multiple_strings_response_test() ->
    {ok, CrossoverIO} = gtp_crossover_io_server:start_link([]),

    {ok, Engine} = gtp_bogus_engine:start_link(),

    {ok, _EngineChannel} =
        gtp_engine_channel:start_link(gtp_bogus_engine, Engine, CrossoverIO, []),

    {ok, ControllerChannel} = gtp_controller_channel:start_link(CrossoverIO, []),

    {ok, #success{values = #{name := [<<"Bogus">>, <<"Engine">>]}}} =
        gtp_controller_channel:send_command(ControllerChannel, #name{}),

    {ok, #success{values = #{version := [<<"0.0.0">>]}}} =
        gtp_controller_channel:send_command(ControllerChannel, #version{}).

command_with_multiline_response_test() ->
    {ok, CrossoverIO} = gtp_crossover_io_server:start_link([]),

    {ok, Engine} = gtp_bogus_engine:start_link(),

    {ok, _EngineChannel} =
        gtp_engine_channel:start_link(gtp_bogus_engine, Engine, CrossoverIO, []),

    {ok, ControllerChannel} = gtp_controller_channel:start_link(CrossoverIO, []),

    {ok, #success{values = #{commands := Commands}}} =
        gtp_controller_channel:send_command(ControllerChannel, #list_commands{}, [{id, 19}]),

    [<<"protocol_version">>,
     <<"name">>,
     <<"known_command">>,
     <<"list_commands">>,
     <<"quit">>] =
        Commands.
