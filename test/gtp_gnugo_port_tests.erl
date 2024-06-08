-module(gtp_gnugo_port_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gtp.hrl").

%%
%% These tests require that GnuGo is installed
%%

connect_and_quit_test() ->
    {ok, Connection} = gtp_port_io_server:start_link(),
    ok = gtp_port_transport:open_port(Connection, {spawn, "gnugo --mode gtp"}),
    {ok, ControllerChannel} = gtp_controller_channel:start_link(Connection, []),

    {ok, #success{values = #{version_number := 2}}} =
        gtp_controller_channel:send_command(ControllerChannel, #protocol_version{}),

    {ok, #success{}} = gtp_controller_channel:send_command(ControllerChannel, #quit{}),

    false = is_process_alive(ControllerChannel).

showboard_started_game_test() ->
    {ok, Connection} = gtp_port_io_server:start_link(),
    ok = gtp_port_transport:open_port(Connection, {spawn, "gnugo --mode gtp"}),
    {ok, ControllerChannel} = gtp_controller_channel:start_link(Connection, []),

    {ok, #success{}} =
        gtp_controller_channel:send_command(ControllerChannel, #komi{new_komi = 6.5}),
    {ok, #success{}} =
        gtp_controller_channel:send_command(ControllerChannel, #boardsize{size = 9}),
    {ok, #success{}} = gtp_controller_channel:send_command(ControllerChannel, #clear_board{}),

    {ok, #success{values = #{vertices := [_V1, _V2, _V3]}}} =
        gtp_controller_channel:send_command(ControllerChannel,
                                            #fixed_handicap{number_of_stones = 3}),

    {ok, #success{}} =
        gtp_controller_channel:send_command(ControllerChannel,
                                            #play{move = #move{color = white, vertex = {g, 3}}}),

    {ok, #success{values = #{vertex := {_, _}}}} =
        gtp_controller_channel:send_command(ControllerChannel, #genmove{color = black}),

    {ok, #success{values = #{board := Board}}} =
        gtp_controller_channel:send_command(ControllerChannel, #showboard{}),

    debug_display_board(Board).

debug_display_board(WordLines) ->
    Lines = lists:map(fun(Words) -> lists:join(" ", Words) end, WordLines),
    Board = lists:join("\n", Lines),
    ?debugMsg(Board).
