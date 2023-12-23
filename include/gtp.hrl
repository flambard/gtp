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

-record(success, {values = #{} :: #{atom() => term()}}).
-record(failure, {error_message}).

-type response() :: #success{} | #failure{}.
