-module(gtp_channel).

-type channel() :: term().

-callback controlling_process(channel(), pid()) -> ok | {error, Reason :: term()}.

-callback send_message(channel(), Message :: iodata()) ->
    ok | {error, Reason :: term()}.

-callback stop(channel()) -> ok.

%% API
-export([recv_message/2]).

%%%
%%% API
%%%

-spec recv_message(ControllingProcess :: pid(), Message :: binary()) -> ok.

recv_message(ControllingProcess, Message) ->
    ControllingProcess ! {gtp, Message},
    ok.
