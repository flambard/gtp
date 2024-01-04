-module(gtp_channel_transport).

-type connection() :: term().

-callback controlling_process(connection(), pid()) -> ok | {error, Reason :: term()}.

-callback send_message(connection(), Message :: iodata()) ->
    ok | {error, Reason :: term()}.

-callback stop(connection()) -> ok.

%% API
-export([recv_message/2]).

%%%
%%% API
%%%

-spec recv_message(ControllingProcess :: pid(), Message :: binary()) -> ok.

recv_message(ControllingProcess, Message) ->
    ControllingProcess ! {gtp, Message},
    ok.
