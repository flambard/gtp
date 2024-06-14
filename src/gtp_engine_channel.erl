-module(gtp_engine_channel).

-behaviour(gen_server).

-include("gtp.hrl").

%% API
-export([start_link/4, register_extension_commands/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%%%
%%% API
%%%

-spec start_link(
    EngineMod :: atom(),
    Engine :: term(),
    Transport :: pid(),
    Options :: proplists:proplist()
) ->
    {ok, EngineServer :: pid()} | {error, Reason :: term()}.
start_link(EngineMod, Engine, Transport, Options) ->
    Args = [EngineMod, Engine, Transport],
    gen_server:start_link(?MODULE, Args, Options).

-spec register_extension_commands(
    EngineServer :: pid(),
    ExtensionCommands :: #{Name :: binary() => Module :: atom()}
) ->
    ok.
register_extension_commands(Server, ExtensionCommands) ->
    gen_server:call(Server, {register_extension_commands, ExtensionCommands}).

%%%
%%% gen_server callbacks
%%%

init([EngineMod, Engine, Transport]) ->
    _Ref = request_line(Transport),
    State =
        #{
            transport => Transport,
            engine_module => EngineMod,
            engine => Engine,
            extension_commands => #{}
        },
    {ok, State}.

handle_call({register_extension_commands, NewCommands}, _From, State) ->
    #{extension_commands := ExtensionCommands} = State,
    NewState = State#{extension_commands := maps:merge(ExtensionCommands, NewCommands)},
    {reply, ok, NewState}.

handle_cast(_Ignored, State) ->
    {noreply, State}.

handle_info({io_reply, _Ref, CommandMessage}, State) ->
    handle_command_message(CommandMessage, State).

%%%
%%% Private functions
%%%

request_line(Transport) ->
    gtp_io:request_line(Transport, "GTP command> ").

handle_command_message(CommandMessage, State) ->
    #{
        transport := Transport,
        engine_module := EngineMod,
        engine := Engine,
        extension_commands := ExtensionCommands
    } =
        State,

    case preprocess(CommandMessage) of
        discard ->
            %% Discarding empty or whitespace-only line
            {noreply, State};
        {ok, ProcessedMessage} ->
            {ID, Command, CommandMod} = gtp_command:decode(ProcessedMessage, ExtensionCommands),

            Response =
                case EngineMod:handle_command(Engine, Command) of
                    {error, Error} ->
                        #failure{error_message = Error};
                    {ok, ResponseValues} ->
                        #success{values = ResponseValues}
                end,

            ResponseMessage = gtp_response:encode(ID, Response, CommandMod),
            %%
            %% TODO: send multi-line responses line by line
            %%
            ok = file:write(Transport, ResponseMessage),

            case Command of
                #quit{} ->
                    {stop, normal, State};
                _Other ->
                    _Ref = request_line(Transport),
                    {noreply, State}
            end
    end.

preprocess(Binary) ->
    B1 = gtp_protocol:remove_control_characters(Binary),
    B2 = remove_comment(B1),
    B3 = gtp_protocol:convert_tabs_to_spaces(B2),
    case discard_empty_line(B3) of
        discard ->
            discard;
        {ok, B4} ->
            {ok, hd(binary:split(B4, <<"\n">>, [trim]))}
    end.

remove_comment(Binary) ->
    [WithoutComment | _Ignored] = binary:split(Binary, <<"#">>),
    WithoutComment.

discard_empty_line(Binary) ->
    case binary:replace(Binary, <<" ">>, <<>>, [global]) of
        <<>> ->
            discard;
        _NotEmpty ->
            {ok, Binary}
    end.
