-module(gtp_engine).
-behaviour(gen_server).
-include("gtp.hrl").

-type engine() :: term().

-callback handle_command(engine(), command()) ->
    {ok, #{atom() := term()}} | {error, Error :: iodata()}.

%% API
-export([
    start_link/5
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

start_link(EngineMod, Engine, ChannelMod, Channel, Options) ->
    Args = [EngineMod, Engine, ChannelMod, Channel],
    case gen_server:start_link(?MODULE, Args, Options) of
        {ok, Pid} ->
            case ChannelMod:controlling_process(Channel, Pid) of
                {error, Reason} -> {error, {channel, Reason}};
                ok -> {ok, Pid}
            end;
        Other ->
            Other
    end.

%%%
%%% gen_server callbacks
%%%

init([EngineMod, Engine, ChannelMod, Channel]) ->
    State = #{
        channel_module => ChannelMod,
        channel => Channel,
        engine_module => EngineMod,
        engine => Engine
    },
    {ok, State}.

handle_call(_Ignored, _From, State) ->
    {reply, ok, State}.

handle_cast(_Ignored, State) ->
    {noreply, State}.

handle_info({gtp, CommandMessage}, State) ->
    #{
        channel := Channel,
        channel_module := ChannelMod,
        engine_module := EngineMod,
        engine := Engine
    } = State,

    {ID, Command, CommandMod} = gtp_command:decode(CommandMessage),

    Response =
        case EngineMod:handle_command(Engine, Command) of
            {error, Error} -> #failure{error_message = Error};
            {ok, ResponseValues} -> #success{values = ResponseValues}
        end,

    ResponseMessage = gtp_response:encode(ID, Response, CommandMod),
    ok = ChannelMod:send_message(Channel, ResponseMessage),

    case Command of
        #quit{} -> {stop, normal, State};
        _Other -> {noreply, State}
    end.

terminate(_Reason, State) ->
    #{
        channel := Channel,
        channel_module := ChannelMod
    } = State,
    ChannelMod:stop(Channel).
