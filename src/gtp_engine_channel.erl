-module(gtp_engine_channel).

-behaviour(gen_server).

-include("gtp.hrl").

%% API
-export([start_link/5, register_extension_commands/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%%
%%% API
%%%

-spec start_link(EngineMod :: atom(),
                 Engine :: term(),
                 TransportMod :: atom(),
                 TransportInitArgs :: [term()],
                 Options :: proplists:proplist()) ->
                    {ok, EngineServer :: pid()} | {error, Reason :: term()}.
start_link(EngineMod, Engine, TransportMod, TransportInitArgs, Options) ->
    Args = [EngineMod, Engine, TransportMod, TransportInitArgs],
    gen_server:start_link(?MODULE, Args, Options).

-spec register_extension_commands(EngineServer :: pid(),
                                  ExtensionCommands :: #{Name :: binary() => Module :: atom()}) ->
                                     ok.
register_extension_commands(Server, ExtensionCommands) ->
    gen_server:call(Server, {register_extension_commands, ExtensionCommands}).

%%%
%%% gen_server callbacks
%%%

init([EngineMod, Engine, TransportMod, TransportInitArgs]) ->
    {ok, TransportState} = TransportMod:init(TransportInitArgs),
    State =
        #{transport_module => TransportMod,
          transport_state => TransportState,
          engine_module => EngineMod,
          engine => Engine,
          extension_commands => #{}},
    {ok, State}.

handle_call({register_extension_commands, NewCommands}, _From, State) ->
    #{extension_commands := ExtensionCommands} = State,
    NewState = State#{extension_commands := maps:merge(ExtensionCommands, NewCommands)},
    {reply, ok, NewState}.

handle_cast({transport_recv, CommandMessage}, State) ->
    #{transport_module := TransportMod,
      transport_state := TransportState,
      engine_module := EngineMod,
      engine := Engine,
      extension_commands := ExtensionCommands} =
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
            {ok, NewTransportState} = TransportMod:send_message(ResponseMessage, TransportState),
            NewState = State#{transport_state := NewTransportState},

            case Command of
                #quit{} ->
                    {stop, normal, NewState};
                _Other ->
                    {noreply, NewState}
            end
    end.

handle_info(Message, State) ->
    #{transport_module := TransportMod, transport_state := TransportState} = State,
    {noreply, NewTransportState} = TransportMod:handle_info(Message, TransportState),
    {noreply, State#{transport_state := NewTransportState}}.

terminate(Reason, State) ->
    #{transport_module := TransportMod, transport_state := TransportState} = State,
    TransportMod:terminate(Reason, TransportState).

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
        <<>> ->
            discard;
        _NotEmpty ->
            {ok, Binary}
    end.
