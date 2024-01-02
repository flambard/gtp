-module(gtp_command_response_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("gtp.hrl").

roundtrip_test() ->
    {ok, ChannelA} = gtp_erlang_channel:start_link(),
    {ok, ChannelB} = gtp_erlang_channel:start_link(ChannelA),

    {ok, _Engine} = gtp_engine_channel:start_link(
        gtp_bogus_engine, make_ref(), gtp_erlang_channel, ChannelB, []
    ),

    {ok, Controller} = gtp_controller_channel:start_link(gtp_erlang_channel, ChannelA, []),

    {ok, #success{values = #{version_number := 2}}} =
        gtp_controller_channel:send_command(Controller, #protocol_version{}, [{id, 1}]).

shutdown_test() ->
    {ok, ChannelA} = gtp_erlang_channel:start_link(),
    {ok, ChannelB} = gtp_erlang_channel:start_link(ChannelA),

    {ok, Engine} = gtp_engine_channel:start_link(
        gtp_bogus_engine, make_ref(), gtp_erlang_channel, ChannelB, []
    ),

    {ok, Controller} = gtp_controller_channel:start_link(gtp_erlang_channel, ChannelA, []),

    {ok, #success{values = #{}}} = gtp_controller_channel:send_command(Controller, #quit{}),

    false = is_process_alive(ChannelA),
    false = is_process_alive(ChannelB),
    false = is_process_alive(Engine),
    false = is_process_alive(Controller).

command_with_single_argument_test() ->
    {ok, ChannelA} = gtp_erlang_channel:start_link(),
    {ok, ChannelB} = gtp_erlang_channel:start_link(ChannelA),

    {ok, _Engine} = gtp_engine_channel:start_link(
        gtp_bogus_engine, make_ref(), gtp_erlang_channel, ChannelB, []
    ),

    {ok, Controller} = gtp_controller_channel:start_link(gtp_erlang_channel, ChannelA, []),

    {ok, #success{values = #{known := true}}} =
        gtp_controller_channel:send_command(Controller, #known_command{
            command_name = <<"known_command">>
        }).

command_with_multiple_arguments_test() ->
    {ok, ChannelA} = gtp_erlang_channel:start_link(),
    {ok, ChannelB} = gtp_erlang_channel:start_link(ChannelA),

    {ok, _Engine} = gtp_engine_channel:start_link(
        gtp_bogus_engine, make_ref(), gtp_erlang_channel, ChannelB, []
    ),

    {ok, Controller} = gtp_controller_channel:start_link(gtp_erlang_channel, ChannelA, []),

    {ok, #failure{error_message = <<"unknown command">>}} =
        gtp_controller_channel:send_command(Controller, #time_left{
            color = black,
            time = 30,
            stones = 0
        }).

command_with_multiple_strings_response_test() ->
    {ok, ChannelA} = gtp_erlang_channel:start_link(),
    {ok, ChannelB} = gtp_erlang_channel:start_link(ChannelA),

    {ok, _Engine} = gtp_engine_channel:start_link(
        gtp_bogus_engine, make_ref(), gtp_erlang_channel, ChannelB, []
    ),

    {ok, Controller} = gtp_controller_channel:start_link(gtp_erlang_channel, ChannelA, []),

    {ok, #success{values = #{name := [<<"Bogus">>, <<"Engine">>]}}} =
        gtp_controller_channel:send_command(Controller, #name{}).

command_with_multiline_response_test() ->
    {ok, ChannelA} = gtp_erlang_channel:start_link(),
    {ok, ChannelB} = gtp_erlang_channel:start_link(ChannelA),

    {ok, _Engine} = gtp_engine_channel:start_link(
        gtp_bogus_engine, make_ref(), gtp_erlang_channel, ChannelB, []
    ),

    {ok, Controller} = gtp_controller_channel:start_link(gtp_erlang_channel, ChannelA, []),

    {ok, #success{values = #{commands := Commands}}} =
        gtp_controller_channel:send_command(Controller, #list_commands{}, [{id, 19}]),

    [
        <<"protocol_version">>,
        <<"name">>,
        <<"known_command">>,
        <<"list_commands">>,
        <<"quit">>
    ] = Commands.
