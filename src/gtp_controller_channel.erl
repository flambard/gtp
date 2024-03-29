-module(gtp_controller_channel).

-behaviour(gen_server).

-include("gtp.hrl").

-type option() :: {id, pos_integer()}.
-type options() :: [option()].

%% API
-export([start_link/3, send_command/2, send_command/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%%
%%% API
%%%

-spec start_link(TransportMod :: atom(),
                 Transport :: pid(),
                 Options :: proplists:proplist()) ->
                    {ok, pid()} | {error, Reason :: term()}.
start_link(TransportMod, Transport, Options) ->
    case gen_server:start_link(?MODULE, [TransportMod, Transport], Options) of
        {ok, Pid} ->
            case TransportMod:controlling_process(Transport, Pid) of
                {error, Reason} ->
                    {error, {transport, Reason}};
                ok ->
                    {ok, Pid}
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

init([TransportMod, Transport]) ->
    State =
        #{response_buffer => [],
          reply_to => undefined,
          message_id => undefined,
          command_module => undefined,
          transport_module => TransportMod,
          transport => Transport},
    {ok, State}.

handle_call({send_command, Command, Options}, From, State) ->
    #{transport_module := TransportMod, transport := Transport} = State,

    ID = proplists:get_value(id, Options),
    ExtensionCommandMod = proplists:get_value(command_module, Options),
    {Message, CommandMod} = gtp_command:encode(ID, Command, ExtensionCommandMod),

    case TransportMod:send_message(Transport, Message) of
        {error, Reason} ->
            {reply, {error, {transport_send, Reason}}, State};
        ok ->
            {noreply,
             State#{reply_to := From,
                    message_id := ID,
                    command_module := CommandMod}}
    end.

handle_cast(_Ignored, State) ->
    {noreply, State}.

handle_info({gtp, <<>>}, State) ->
    #{response_buffer := Buffer,
      reply_to := From,
      message_id := ID,
      command_module := CommandMod} =
        State,
    Message = lists:reverse(Buffer),
    {ID, Response} = gtp_response:decode(CommandMod, Message),
    ok = gen_server:reply(From, {ok, Response}),
    NewState =
        State#{response_buffer := [],
               reply_to := undefined,
               message_id := undefined,
               command_module := undefined},
    {noreply, NewState};
handle_info({gtp, Line}, State) ->
    #{response_buffer := Buffer} = State,
    ProcessedLine = preprocess(Line),
    {noreply, State#{response_buffer := [ProcessedLine | Buffer]}}.

terminate(_Reason, State) ->
    #{transport := Transport, transport_module := TransportMod} = State,
    TransportMod:stop(Transport).

preprocess(Binary) ->
    B1 = gtp_protocol:remove_control_characters(Binary),
    gtp_protocol:convert_tabs_to_spaces(B1).
