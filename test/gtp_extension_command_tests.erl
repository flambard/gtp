-module(gtp_extension_command_tests).

-include_lib("eunit/include/eunit.hrl").

%% -include_lib("gtp.hrl").

% roundtrip_test() ->
%     {ok, ConnectionA} = gtp_erlang_transport:start_link(),
%     {ok, ConnectionB} = gtp_erlang_transport:start_link(ConnectionA),

%     {ok, Engine} = gtp_bogus_engine:start_link(),

%     {ok, EngineChannel} = gtp_engine_channel:start_link(
%         gtp_bogus_engine, Engine, gtp_erlang_transport, ConnectionB, []
%     ),

%     ok = gtp_engine_channel:register_extension_commands(EngineChannel, #{
%         <<"test-echo">> => gtp_echo_command
%     }),

%     {ok, ControllerChannel} = gtp_controller_channel:start_link(
%         gtp_erlang_transport, ConnectionA, []
%     ),

%     Message = <<"PING">>,
%     Command = gtp_echo_command:new(Message),

%     {ok, #success{values = #{value := Message}}} =
%         gtp_controller_channel:send_command(ControllerChannel, Command, [
%             {id, 1},
%             {command_module, gtp_echo_command}
%         ]).
