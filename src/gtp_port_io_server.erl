-module(gtp_port_io_server).

-behaviour(gen_server).

%% API
-export([start_link/0, open_port/2, open_port/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

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
%%% gen_server callbacks
%%%

init([]) ->
    State =
        #{
            port => undefined,
            reply_to => undefined,
            reply_as => undefined,
            buffer => queue:new()
        },
    {ok, State}.

handle_call({open_port, PortName, PortSettings}, _From, State) ->
    Port = erlang:open_port(PortName, [binary, {line, 1024} | PortSettings]),
    {reply, ok, State#{port := Port}}.

handle_cast(_Ignored, State) ->
    {noreply, State}.

handle_info({io_request, From, ReplyAs, {put_chars, latin1, Message}}, State) ->
    #{port := Port} = State,
    Port ! {self(), {command, Message}},
    From ! {io_reply, ReplyAs, ok},
    {noreply, State};
handle_info({io_request, From, ReplyAs, {get_line, latin1, _Prompt}}, State) ->
    #{buffer := Buffer} = State,
    NewState =
        case queue:out(Buffer) of
            {empty, Buffer} ->
                %% No data buffered - store the request
                State#{reply_to := From, reply_as := ReplyAs};
            {{value, Line}, NewBuffer} ->
                %% Buffered data ready - send immediately
                From ! {io_reply, ReplyAs, Line},
                State#{buffer := NewBuffer}
        end,
    {noreply, NewState};
handle_info({Port, {data, {eol, Line}}}, State) ->
    #{
        port := Port,
        buffer := Buffer,
        reply_to := ReplyTo,
        reply_as := ReplyAs
    } =
        State,
    NewState =
        case ReplyTo of
            undefined ->
                %% No get_line request waiting - buffer the data
                State#{buffer := queue:in(Line, Buffer)};
            ReplyTo ->
                %% get_line request waiting for reply - send immediately
                ReplyTo ! {io_reply, ReplyAs, Line},
                State#{reply_to := undefined, reply_as := undefined}
        end,
    {noreply, NewState}.
