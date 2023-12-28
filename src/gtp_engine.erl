-module(gtp_engine).
-behaviour(gen_server).
-include("gtp.hrl").

-type engine() :: term().

-callback handle_command(engine(), command()) ->
    {ok, #{atom() := term()}} | {error, Error :: iodata()}.

%% API
-export([
    start_link/5,
    register_command_module/2
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

register_command_module(Server, ExtensionCommandModule) ->
    gen_server:call(Server, {register_command_module, ExtensionCommandModule}).

%%%
%%% gen_server callbacks
%%%

init([EngineMod, Engine, ChannelMod, Channel]) ->
    State = #{
        channel_module => ChannelMod,
        channel => Channel,
        engine_module => EngineMod,
        engine => Engine,
        extension_commands => #{}
    },
    {ok, State}.

handle_call({register_command_module, CommandMod}, _From, State) ->
    #{extension_commands := ExtensionCommands} = State,
    Name = CommandMod:command_name(),
    NewState = State#{extension_commands := ExtensionCommands#{Name => CommandMod}},
    {reply, ok, NewState}.

handle_cast(_Ignored, State) ->
    {noreply, State}.

handle_info({gtp, CommandMessage}, State) ->
    #{
        channel := Channel,
        channel_module := ChannelMod,
        engine_module := EngineMod,
        engine := Engine,
        extension_commands := ExtensionCommands
    } = State,

    case preprocess(CommandMessage) of
        discard ->
            %% Discarding empty or whitespace-only line
            {noreply, State};
        {ok, ProcessedMessage} ->
            {ID, Command, CommandMod} = gtp_command:decode(ProcessedMessage, ExtensionCommands),

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
            end
    end.

terminate(_Reason, State) ->
    #{
        channel := Channel,
        channel_module := ChannelMod
    } = State,
    ChannelMod:stop(Channel).

%%%
%%% Private functions
%%%

preprocess(Binary) ->
    B1 = gtp_protocol:remove_control_characters(Binary),
    B2 = remove_comment(B1),
    B3 = gtp_protocol:convert_tabs_to_spaces(B2),
    discard_empty_line(B3).

remove_comment(Binary) ->
    [WithoutComment | _Ignored] = binary:split(Binary, <<"#">>),
    WithoutComment.

discard_empty_line(Binary) ->
    case binary:replace(Binary, <<" ">>, <<>>, [global]) of
        <<>> -> discard;
        _NotEmpty -> {ok, Binary}
    end.
