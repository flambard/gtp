-module(gtp_gnugo_port_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("gtp.hrl").

connect_and_quit_test() ->
    {ok, Channel} = gtp_port_channel:start_link(),
    ok = gtp_port_channel:open_port(Channel, {spawn, "gnugo --mode gtp"}, [binary, {line, 1024}]),

    {ok, Controller} = gtp_controller:start_link(gtp_port_channel, Channel, []),

    {ok, #success{values = #{version_number := 2}}} =
        gtp_controller:send_command(Controller, #protocol_version{}),

    {ok, #success{values = #{}}} = gtp_controller:send_command(Controller, #quit{}),

    false = is_process_alive(Channel),
    false = is_process_alive(Controller).

showboard_started_game_test() ->
    {ok, Channel} = gtp_port_channel:start_link(),
    ok = gtp_port_channel:open_port(Channel, {spawn, "gnugo --mode gtp"}, [binary, {line, 1024}]),

    {ok, Controller} = gtp_controller:start_link(gtp_port_channel, Channel, []),

    {ok, #success{values = #{board := Board}}} =
        gtp_controller:send_command(Controller, #showboard{}),

    debug_display_board(Board).

debug_display_board(WordLines) ->
    Lines = lists:map(fun(Words) -> lists:join(" ", Words) end, WordLines),
    Board = lists:join("\n", Lines),
    ?debugMsg(Board).
