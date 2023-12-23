-module(gtp_command_response_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("gtp.hrl").

roundtrip_test() ->
    {ok, ChannelA} = gtp_erlang_channel:start_link(),
    {ok, ChannelB} = gtp_erlang_channel:start_link(),
    ok = gtp_erlang_channel:connect(ChannelA, ChannelB),

    {ok, _Engine} = gtp_engine:start_link(
        gtp_bogus_engine, make_ref(), gtp_erlang_channel, ChannelB, []
    ),

    {ok, Controller} = gtp_controller:start_link(gtp_erlang_channel, ChannelA, []),

    {ok, #success{values = #{version_number := 2}}} =
        gtp_controller:send_command(Controller, #protocol_version{}, [{id, 1}]).

shutdown_test() ->
    {ok, ChannelA} = gtp_erlang_channel:start_link(),
    {ok, ChannelB} = gtp_erlang_channel:start_link(),
    ok = gtp_erlang_channel:connect(ChannelA, ChannelB),

    {ok, Engine} = gtp_engine:start_link(
        gtp_bogus_engine, make_ref(), gtp_erlang_channel, ChannelB, []
    ),

    {ok, Controller} = gtp_controller:start_link(gtp_erlang_channel, ChannelA, []),

    {ok, #success{values = #{}}} = gtp_controller:send_command(Controller, #quit{}),

    false = is_process_alive(ChannelA),
    false = is_process_alive(ChannelB),
    false = is_process_alive(Engine),
    false = is_process_alive(Controller).
