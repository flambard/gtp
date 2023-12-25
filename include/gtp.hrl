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

-type response_values() :: #{atom() => term()}.

-record(success, {values = #{} :: response_values()}).
-record(failure, {error_message :: binary()}).

-type response() :: #success{} | #failure{}.
