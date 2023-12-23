-module(gtp_controller).
-behaviour(gen_server).
-include("gtp.hrl").

-type option() :: {id, pos_integer()}.

-type options() :: [option()].

%% API
-export([
    start_link/3,
    command_protocol_version/2,
    command_quit/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%%%
%%% API
%%%

-spec start_link(ChannelMod :: atom(), Channel :: pid(), Options :: proplists:proplist()) ->
    {ok, pid()} | {error, Reason :: term()}.

start_link(ChannelMod, Channel, Options) ->
    case gen_server:start_link(?MODULE, [ChannelMod, Channel], Options) of
        {ok, Pid} ->
            case ChannelMod:controlling_process(Channel, Pid) of
                {error, Reason} -> {error, {channel, Reason}};
                ok -> {ok, Pid}
            end;
        Other ->
            Other
    end.

-spec command_protocol_version(Controller :: pid(), options()) ->
    {ok, ProtocolVersion :: pos_integer()} | {error, Reason :: term()}.

command_protocol_version(Controller, Options) ->
    gen_server:call(Controller, {send_command, #protocol_version{}, Options}).

-spec command_quit(Controller :: pid(), options()) -> ok | {error, Reason :: term()}.

command_quit(Controller, Options) ->
    case gen_server:call(Controller, {send_command, #quit{}, Options}) of
        {error, Reason} ->
            {error, Reason};
        {ok, #success_response{}} ->
            gen_server:stop(Controller),
            {ok, #success_response{}}
    end.

%%%
%%% gen_server callbacks
%%%

init([ChannelMod, Channel]) ->
    State = #{
        reply_to => undefined,
        message_id => undefined,
        command_module => undefined,
        channel_module => ChannelMod,
        channel => Channel
    },
    {ok, State}.

handle_call({send_command, Command, Options}, From, State) ->
    #{
        channel_module := ChannelMod,
        channel := Channel
    } = State,

    ID = proplists:get_value(id, Options),
    CommandMod = gtp_command:command_module(Command),
    Message = [gtp_command:encode_optional_id(ID), CommandMod:encode_command(Command)],

    case ChannelMod:send_message(Channel, Message) of
        {error, Reason} -> {reply, {error, {channel_send, Reason}}, State};
        ok -> {noreply, State#{reply_to := From, message_id := ID, command_module := CommandMod}}
    end.

handle_cast(_Ignored, State) ->
    {noreply, State}.

handle_info({gtp, Message}, State) ->
    #{
        reply_to := From,
        message_id := ID,
        command_module := CommandMod
    } = State,

    {ID, Response} = gtp_response:decode(CommandMod, Message),
    ok = gen_server:reply(From, {ok, Response}),
    {noreply, State#{reply_to := undefined, message_id := undefined}}.

terminate(_Reason, State) ->
    #{
        channel := Channel,
        channel_module := ChannelMod
    } = State,
    ChannelMod:stop(Channel).
