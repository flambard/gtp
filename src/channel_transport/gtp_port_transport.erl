-module(gtp_port_transport).
-behaviour(gtp_channel_transport).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    open_port/2,
    open_port/3
]).

%% gtp_channel_transport callbacks
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

open_port(Server, PortName) ->
    open_port(Server, PortName, []).

open_port(Server, PortName, PortSettings) ->
    gen_server:call(Server, {open_port, PortName, PortSettings}).

%%%
%%% gtp_channel_transport callbacks
%%%

controlling_process(Server, Pid) ->
    gen_server:call(Server, {controlling_process, Pid}).

send_message(Server, Message) ->
    gen_server:call(Server, {send_message, Message}).

stop(Server) ->
    gen_server:stop(Server).

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
    Port = erlang:open_port(PortName, [binary, {line, 1024} | PortSettings]),
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
    ok = gtp_channel_transport:recv_message(Pid, Line),
    {noreply, State}.
