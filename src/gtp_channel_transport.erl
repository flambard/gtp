-module(gtp_channel_transport).

%% -type connection() :: term().

%% -callback controlling_process(connection(), pid()) -> ok | {error, Reason :: term()}.

-callback init(Args :: term()) -> {ok, State :: term()} | {error, Reason :: term()}.
-callback send_message(Message :: iodata(), State :: term()) ->
                          {ok, NewState :: term()} | {error, Reason :: term()}.
-callback handle_info(Info :: term(), State :: term()) -> {noreply, NewState :: term()}.
-callback terminate(Reason :: atom(), State :: term()) -> {ok, NewState :: term()}.

%% -callback stop(connection()) -> ok.

-optional_callbacks([terminate/2]).

%% API
-export([recv_message/1]).

%%%
%%% API
%%%

-spec recv_message(Message :: binary()) -> ok.
recv_message(Message) ->
    gen_server:cast(self(), {transport_recv, Message}).
