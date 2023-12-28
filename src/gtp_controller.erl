-module(gtp_controller).
-behaviour(gen_server).
-include("gtp.hrl").

-type option() :: {id, pos_integer()}.

-type options() :: [option()].

%% API
-export([
    start_link/3,
    send_command/2,
    send_command/3
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

-spec send_command(Controller :: pid(), Command :: command()) ->
    {ok, Response :: response()} | {error, Reason :: term()}.

send_command(Controller, Command) ->
    send_command(Controller, Command, []).

-spec send_command(Controller :: pid(), Command :: command(), options()) ->
    {ok, Response :: response()} | {error, Reason :: term()}.

send_command(Controller, #quit{}, Options) ->
    case gen_server:call(Controller, {send_command, #quit{}, Options}) of
        {error, Reason} ->
            {error, Reason};
        {ok, #success{} = SuccessResponse} ->
            gen_server:stop(Controller),
            {ok, SuccessResponse}
    end;
send_command(Controller, Command, Options) ->
    gen_server:call(Controller, {send_command, Command, Options}).

%%%
%%% gen_server callbacks
%%%

init([ChannelMod, Channel]) ->
    State = #{
        response_buffer => [],
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
    {Message, CommandMod} = gtp_command:encode(ID, Command),

    case ChannelMod:send_message(Channel, Message) of
        {error, Reason} -> {reply, {error, {channel_send, Reason}}, State};
        ok -> {noreply, State#{reply_to := From, message_id := ID, command_module := CommandMod}}
    end.

handle_cast(_Ignored, State) ->
    {noreply, State}.

handle_info({gtp, <<>>}, State) ->
    #{
        response_buffer := Buffer,
        reply_to := From,
        message_id := ID,
        command_module := CommandMod
    } = State,
    Message = lists:reverse(Buffer),
    {ID, Response} = gtp_response:decode(CommandMod, Message),
    ok = gen_server:reply(From, {ok, Response}),
    {noreply, State#{response_buffer := [], reply_to := undefined, message_id := undefined}};
handle_info({gtp, Line}, State) ->
    #{response_buffer := Buffer} = State,
    %% TODO: preprocess each line as they arrive
    {noreply, State#{response_buffer := [Line | Buffer]}}.

terminate(_Reason, State) ->
    #{
        channel := Channel,
        channel_module := ChannelMod
    } = State,
    ChannelMod:stop(Channel).
