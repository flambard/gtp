-type id() :: undefined | pos_integer().

-type color() :: black | white.

-record(move, {color, vertex}).

%%%
%%% Commands
%%%

-record(protocol_version, {}).

-record(quit, {}).

-record(known_command, {command_name :: binary()}).

-record(time_left, {
    color :: color(),
    time :: non_neg_integer(),
    stones = 0 :: non_neg_integer()
}).

-type command() ::
    #protocol_version{}
    | #quit{}
    | #known_command{}
    | #time_left{}.

%%%
%%% Responses
%%%

-type response_values() :: #{atom() => term()}.

-record(success, {values = #{} :: response_values()}).
-record(failure, {error_message :: binary()}).

-type response() :: #success{} | #failure{}.
