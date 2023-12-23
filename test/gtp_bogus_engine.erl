-module(gtp_bogus_engine).
-behaviour(gtp_engine).
-include_lib("gtp.hrl").

-export([
    handle_command/2
]).

handle_command(_Engine, Command) ->
    handle_command(Command).

handle_command(#protocol_version{}) ->
    {ok, #{version_number => 2}};
handle_command(#quit{}) ->
    {ok, #{}}.
