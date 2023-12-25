-type id() :: undefined | pos_integer().

-record(move, {color, vertex}).

%%%
%%% Commands
%%%

-record(protocol_version, {}).

-record(quit, {}).

-record(known_command, {command_name :: binary()}).

-type command() :: #protocol_version{} | #quit{}.

%%%
%%% Responses
%%%

-type response_values() :: #{atom() => term()}.

-record(success, {values = #{} :: response_values()}).
-record(failure, {error_message :: binary()}).

-type response() :: #success{} | #failure{}.
