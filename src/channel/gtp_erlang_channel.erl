-module(gtp_erlang_channel).
-behaviour(gtp_channel).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    connect/2
]).

%% gtp_channel callbacks
-export([
    controlling_process/2,
    send_message/2,
    stop/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

%%%
%%% API
%%%

start_link() ->
    gen_server:start_link(?MODULE, [], []).

connect(Channel, Peer) ->
    gen_server:call(Channel, {connect, Peer}).

%%%
%%% gtp_channel callbacks
%%%

controlling_process(Channel, Pid) ->
    gen_server:call(Channel, {controlling_process, Pid}).

send_message(Channel, Message) ->
    gen_server:call(Channel, {send_message, Message}).

stop(Channel) ->
    gen_server:stop(Channel).

%%%
%%% gen_server callbacks
%%%

init([]) ->
    State = #{
        peer => undefined,
        controlling_process => undefined
    },
    {ok, State}.

handle_call({connect, Peer}, _From, State) ->
    Peer ! {connect, self()},
    {reply, ok, State#{peer := Peer}};
handle_call({controlling_process, Pid}, _From, State) ->
    {reply, ok, State#{controlling_process := Pid}};
handle_call({send_message, Message}, _From, State) ->
    #{peer := Peer} = State,
    Peer ! {transport, iolist_to_binary(Message)},
    {reply, ok, State}.

handle_cast(_Ignored, State) ->
    {noreply, State}.

handle_info({connect, Peer}, State) ->
    {noreply, State#{peer := Peer}};
handle_info({transport, Message}, State) ->
    #{controlling_process := Pid} = State,
    ok = recv_message_lines(Pid, Message),
    {noreply, State}.

recv_message_lines(Pid, Message) ->
    [Line, Rest] = binary:split(Message, <<"\n">>),
    ok = gtp_channel:recv_message(Pid, Line),
    case Rest of
        <<>> -> ok;
        <<"\n">> -> gtp_channel:recv_message(Pid, <<>>);
        MoreData -> recv_message_lines(Pid, MoreData)
    end.
