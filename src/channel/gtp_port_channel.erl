-module(gtp_port_channel).
-behaviour(gtp_channel).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    open_port/3
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

open_port(Channel, PortName, PortSettings) ->
    gen_server:call(Channel, {open_port, PortName, PortSettings}).

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
        port => undefined,
        controlling_process => undefined
    },
    {ok, State}.

handle_call({open_port, PortName, PortSettings}, _From, State) ->
    Port = open_port(PortName, PortSettings),
    {reply, ok, State#{port := Port}};
handle_call({controlling_process, Pid}, _From, State) ->
    {reply, ok, State#{controlling_process := Pid}};
handle_call({send_message, Message}, _From, State) ->
    #{port := Port} = State,
    Port ! {self(), {command, Message}},
    {reply, ok, State}.

handle_cast(_Ignored, State) ->
    {noreply, State}.

handle_info({Port, {data, {eol, Line}}}, State) ->
    #{
        port := Port,
        controlling_process := Pid
    } = State,
    ok = gtp_channel:recv_message(Pid, Line),
    {noreply, State}.
