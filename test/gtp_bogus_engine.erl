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
handle_command(#name{}) ->
    {ok, #{name => ["Bogus", "Engine"]}};
handle_command(#version{}) ->
    {ok, #{version => ["0.0.0"]}};
handle_command(#known_command{command_name = Name}) ->
    {ok, #{known => is_known_command(Name)}};
handle_command(#list_commands{}) ->
    {ok, #{commands => command_list()}};
handle_command(#quit{}) ->
    {ok, #{}};
handle_command(#komi{new_komi = _Komi}) ->
    {ok, #{}};
handle_command({echo, Value}) ->
    {ok, #{value => Value}};
handle_command(_Unknown) ->
    {error, "unknown command"}.

%%%
%%% Private function
%%%

is_known_command(Name) ->
    lists:member(Name, command_list()).

command_list() ->
    [
        <<"protocol_version">>,
        <<"name">>,
        <<"known_command">>,
        <<"list_commands">>,
        <<"quit">>
    ].
