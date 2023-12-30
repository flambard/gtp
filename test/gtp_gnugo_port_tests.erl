-module(gtp_gnugo_port_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("gtp.hrl").

%%
%% These tests require that GnuGo is installed
%%

connect_and_quit_test() ->
    {ok, Channel} = gtp_port_channel:start_link(),
    ok = gtp_port_channel:open_port(Channel, {spawn, "gnugo --mode gtp"}),

    {ok, Controller} = gtp_controller:start_link(gtp_port_channel, Channel, []),

    {ok, #success{values = #{version_number := 2}}} =
        gtp_controller:send_command(Controller, #protocol_version{}),

    {ok, #success{}} = gtp_controller:send_command(Controller, #quit{}),

    false = is_process_alive(Channel),
    false = is_process_alive(Controller).

showboard_started_game_test() ->
    {ok, Channel} = gtp_port_channel:start_link(),
    ok = gtp_port_channel:open_port(Channel, {spawn, "gnugo --mode gtp"}),

    {ok, Controller} = gtp_controller:start_link(gtp_port_channel, Channel, []),

    {ok, #success{}} = gtp_controller:send_command(Controller, #boardsize{size = 9}),
    {ok, #success{}} = gtp_controller:send_command(Controller, #clear_board{}),

    {ok, #success{values = #{vertices := [_V1, _V2, _V3]}}} =
        gtp_controller:send_command(Controller, #fixed_handicap{number_of_stones = 3}),

    {ok, #success{}} =
        gtp_controller:send_command(Controller, #play{move = #move{color = white, vertex = {g, 3}}}),

    {ok, #success{values = #{vertex := {_, _}}}} =
        gtp_controller:send_command(Controller, #genmove{color = black}),

    {ok, #success{values = #{board := Board}}} =
        gtp_controller:send_command(Controller, #showboard{}),

    debug_display_board(Board).

debug_display_board(WordLines) ->
    Lines = lists:map(fun(Words) -> lists:join(" ", Words) end, WordLines),
    Board = lists:join("\n", Lines),
    ?debugMsg(Board).
