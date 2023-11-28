-type id() :: undefined | pos_integer().

-record(move, {color, vertex}).

%%%
%%% Responses
%%%

-record(success_response, {values = []}).
-record(failure_response, {error_message}).

-type response() :: #success_response{} | #failure_response{}.
