-module(gtp_bogus_engine).
-behaviour(gen_server).
-behaviour(gtp_engine).
-include_lib("gtp.hrl").

%% API
-export([
    start_link/0
]).

%% gtp_engine callbacks
-export([
    handle_command/2
]).
%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

%%%
%%% API
%%%

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%
%%% gtp_engine
%%%

handle_command(Engine, Command) ->
    gen_server:call(Engine, {command, Command}).

%%%
%%% gen_server callbacks
%%%

init([]) ->
    {ok, #{}}.

handle_call({command, #quit{}}, _From, State) ->
    {stop, normal, handle_command(#quit{}), State};
handle_call({command, Command}, _From, State) ->
    {reply, handle_command(Command), State}.

handle_cast(_Message, State) ->
    {noreply, State}.

%%%
%%% Private function
%%%

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
