-module(gtp_erlang_transport).

-behaviour(gtp_channel_transport).

%% -behaviour(gen_server).

%% API
% -export([start_link/0, start_link/1]).
%% gtp_channel_transport callbacks
% -export([controlling_process/2, send_message/2, stop/1]).
% %% gen_server callbacks
-export([init/1, send_message/2, handle_info/2]).

%%%
%%% API

% start_link() ->
%     gen_server:start_link(?MODULE, [], []).

% start_link(Peer) ->
%     gen_server:start_link(?MODULE, [Peer], []).

%%%
%%% gtp_channel_transport callbacks
%%%

% controlling_process(Server, Pid) ->
%     gen_server:call(Server, {controlling_process, Pid}).

send_message(Message, State) ->
    %% gen_server:call(Server, {send_message, Message}).
    #{peer := Peer} = State,
    Peer ! {transport, iolist_to_binary(Message)},
    {ok, State}.

% stop(Server) ->
%     gen_server:stop(Server).

%%%
%%% gen_server callbacks
%%%

init([]) ->
    State = #{peer => undefined},
    {ok, State};
init([Peer]) ->
    Peer ! {connect, self()},
    State = #{peer => Peer},
    {ok, State}.

% handle_call({controlling_process, Pid}, _From, State) ->
%     {reply, ok, State#{controlling_process := Pid}};
% handle_call({send_message, Message}, _From, State) ->
%     #{peer := Peer} = State,
%     Peer ! {transport, iolist_to_binary(Message)},
%     {reply, ok, State}.

% handle_cast(_Ignored, State) ->
%     {noreply, State}.

handle_info({connect, Peer}, State) ->
    {noreply, State#{peer := Peer}};
handle_info({transport, Message}, State) ->
    ok = recv_message_lines(Message),
    {noreply, State}.

recv_message_lines(Message) ->
    [Line, Rest] = binary:split(Message, <<"\n">>),
    ok = gtp_channel_transport:recv_message(Line),
    case Rest of
        <<>> ->
            ok;
        <<"\n">> ->
            ok = gtp_channel_transport:recv_message(<<>>);
        MoreData ->
            recv_message_lines(MoreData)
    end.
