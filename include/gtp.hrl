-type id() :: undefined | pos_integer().

-type color() :: black | white.

-type vertex() :: {atom(), pos_integer()} | pass.

-record(move, {
    color :: color(),
    vertex :: vertex()
}).

%%%
%%% Commands
%%%

-record(protocol_version, {}).

-record(name, {}).

-record(known_command, {command_name :: binary()}).

-record(list_commands, {}).

-record(quit, {}).

-record(time_left, {
    color :: color(),
    time :: non_neg_integer(),
    stones = 0 :: non_neg_integer()
}).

-type command() ::
    #protocol_version{}
    | #name{}
    | #known_command{}
    | #list_commands{}
    | #quit{}
    | #time_left{}.

%%%
%%% Responses
%%%

-type response_values() :: #{atom() => term()}.

-record(success, {values = #{} :: response_values()}).
-record(failure, {error_message :: binary()}).

-type response() :: #success{} | #failure{}.
