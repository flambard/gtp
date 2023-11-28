-module(gtp_command_response_tests).
-include_lib("eunit/include/eunit.hrl").

roundtrip_test() ->
    {ok, ChannelA} = gtp_erlang_channel:start_link(),
    {ok, ChannelB} = gtp_erlang_channel:start_link(),
    ok = gtp_erlang_channel:connect(ChannelA, ChannelB),

    {ok, _Engine} = gtp_engine:start_link(
        gtp_bogus_engine, make_ref(), gtp_erlang_channel, ChannelB, []
    ),

    {ok, Controller} = gtp_controller:start_link(gtp_erlang_channel, ChannelA, []),

    {ok, _} = gtp_controller:command_protocol_version(Controller, [{id, 1}]).

shutdown_test() ->
    {ok, ChannelA} = gtp_erlang_channel:start_link(),
    {ok, ChannelB} = gtp_erlang_channel:start_link(),
    ok = gtp_erlang_channel:connect(ChannelA, ChannelB),

    {ok, Engine} = gtp_engine:start_link(
        gtp_bogus_engine, make_ref(), gtp_erlang_channel, ChannelB, []
    ),

    {ok, Controller} = gtp_controller:start_link(gtp_erlang_channel, ChannelA, []),

    {ok, {success_response, []}} = gtp_controller:command_quit(Controller, []),

    false = is_process_alive(ChannelA),
    false = is_process_alive(ChannelB),
    false = is_process_alive(Engine),
    false = is_process_alive(Controller).
