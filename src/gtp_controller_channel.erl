-module(gtp_controller_channel).

-behaviour(gen_server).

-include("gtp.hrl").

-type option() :: {id, pos_integer()}.
-type options() :: [option()].

%% API
-export([start_link/2, send_command/2, send_command/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%%%
%%% API
%%%

-spec start_link(Transport :: pid(), Options :: proplists:proplist()) ->
    {ok, pid()} | {error, Reason :: term()}.
start_link(Transport, Options) ->
    gen_server:start_link(?MODULE, [Transport], Options).

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

init([Transport]) ->
    State =
        #{
            response_buffer => [],
            reply_to => undefined,
            message_id => undefined,
            command_module => undefined,
            transport => Transport
        },
    {ok, State}.

handle_call({send_command, Command, Options}, From, State) ->
    #{transport := Transport} = State,

    ID = proplists:get_value(id, Options),
    ExtensionCommandMod = proplists:get_value(command_module, Options),
    {Message, CommandMod} = gtp_command:encode(ID, Command, ExtensionCommandMod),

    case file:write(Transport, Message) of
        {error, Reason} ->
            {reply, {error, {transport_send, Reason}}, State};
        ok ->
            _Ref = request_line(Transport),
            NewState =
                State#{
                    reply_to := From,
                    message_id := ID,
                    command_module := CommandMod
                },
            {noreply, NewState}
    end.

handle_cast(_Ignored, State) ->
    {noreply, State}.

handle_info({io_reply, _Ref, ""}, State) ->
    NewState = finish_response(State),
    {noreply, NewState};
handle_info({io_reply, _Ref, "\n"}, State) ->
    NewState = finish_response(State),
    {noreply, NewState};
handle_info({io_reply, _Ref, <<>>}, State) ->
    NewState = finish_response(State),
    {noreply, NewState};
handle_info({io_reply, _Ref, <<"\n">>}, State) ->
    NewState = finish_response(State),
    {noreply, NewState};
handle_info({io_reply, _Ref, Line}, State) ->
    #{transport := Transport, response_buffer := Buffer} = State,
    _NewRef = request_line(Transport),
    Bin = iolist_to_binary(Line),
    ProcessedLine = preprocess(Bin),
    {noreply, State#{response_buffer := [ProcessedLine | Buffer]}}.

%%%
%%% Private functions
%%%

finish_response(State) ->
    #{
        response_buffer := Buffer,
        reply_to := From,
        message_id := ID,
        command_module := CommandMod
    } =
        State,
    Message = lists:reverse(Buffer),
    {ID, Response} = gtp_response:decode(CommandMod, Message),
    ok = gen_server:reply(From, {ok, Response}),
    State#{
        response_buffer := [],
        reply_to := undefined,
        message_id := undefined,
        command_module := undefined
    }.

request_line(Transport) ->
    gtp_io:request_line(Transport, "GTP response> ").

preprocess(Binary) ->
    B1 = gtp_protocol:remove_control_characters(Binary),
    B2 = gtp_protocol:convert_tabs_to_spaces(B1),
    hd(binary:split(B2, <<"\n">>, [trim])).
