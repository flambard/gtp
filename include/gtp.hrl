-type id() :: undefined | pos_integer().

-record(move, {color, vertex}).

%%%
%%% Commands
%%%

-record(protocol_version, {}).

-record(quit, {}).

-type command() :: #protocol_version{} | #quit{}.

%%%
%%% Responses
%%%

-record(success_response, {values = []}).
-record(failure_response, {error_message}).

-type response() :: #success_response{} | #failure_response{}.
