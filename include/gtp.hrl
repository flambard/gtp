-type id() :: undefined | pos_integer().

-type color() :: black | white.

-type vertex() :: {atom(), pos_integer()} | pass.

-record(move, {
    color :: color(),
    vertex :: vertex()
}).

-type simple_entity_type() ::
    int
    | float
    | boolean
    | color
    | vertex
    | move
    | string.

-type compound_entity_type() ::
    [singleline_entity_type()]
    | {alternative, singleline_entity_type(), singleline_entity_type()}
    | {list, singleline_entity_type()}.

-type singleline_entity_type() :: simple_entity_type() | compound_entity_type().

-type entity_type() :: singleline_entity_type() | {multiline, singleline_entity_type()}.

-type entity_value() ::
    non_neg_integer()
    | float()
    | boolean()
    | color()
    | vertex()
    | #move{}
    | binary()
    | [entity_value()].

%%%
%%% Commands
%%%

-record(protocol_version, {}).

-record(name, {}).

-record(version, {}).

-record(known_command, {
    command_name :: binary()
}).

-record(list_commands, {}).

-record(quit, {}).

-record(boardsize, {
    size :: pos_integer()
}).

-record(clear_board, {}).

-record(komi, {
    new_komi :: float()
}).

-record(fixed_handicap, {
    number_of_stones :: non_neg_integer()
}).

-record(place_free_handicap, {
    number_of_stones :: non_neg_integer()
}).

-record(set_free_handicap, {
    vertices :: [vertex()]
}).

-record(play, {
    move :: #move{}
}).

-record(genmove, {
    color :: color()
}).

-record(undo, {}).

-record(time_settings, {
    main_time :: non_neg_integer(),
    byo_yomi_time :: non_neg_integer(),
    byo_yomi_stones = 0 :: non_neg_integer()
}).

-record(time_left, {
    color :: color(),
    time :: non_neg_integer(),
    stones = 0 :: non_neg_integer()
}).

-record(final_score, {}).

-record(final_status_list, {
    status :: iodata()
}).

-record(loadsgf, {
    filename :: iodata(),
    move_number :: non_neg_integer() | undefined
}).

-record(reg_genmove, {
    color :: color()
}).

-record(showboard, {}).

-type command() ::
    #protocol_version{}
    | #name{}
    | #version{}
    | #known_command{}
    | #list_commands{}
    | #quit{}
    | #boardsize{}
    | #clear_board{}
    | #komi{}
    | #fixed_handicap{}
    | #place_free_handicap{}
    | #set_free_handicap{}
    | #play{}
    | #genmove{}
    | #undo{}
    | #time_settings{}
    | #time_left{}
    | #final_score{}
    | #final_status_list{}
    | #loadsgf{}
    | #reg_genmove{}
    | #showboard{}.

-define(COMMAND_MODULES, #{
    <<"protocol_version">> => gtp_command_protocol_version,
    <<"name">> => gtp_command_name,
    <<"version">> => gtp_command_version,
    <<"known_command">> => gtp_command_known_command,
    <<"list_commands">> => gtp_command_list_commands,
    <<"quit">> => gtp_command_quit,
    <<"boardsize">> => gtp_command_boardsize,
    <<"clear_board">> => gtp_command_clear_board,
    <<"komi">> => gtp_command_komi,
    <<"fixed_handicap">> => gtp_command_fixed_handicap,
    <<"place_free_handicap">> => gtp_command_place_free_handicap,
    <<"set_free_handicap">> => gtp_command_set_free_handicap,
    <<"play">> => gtp_command_play,
    <<"genmove">> => gtp_command_genmove,
    <<"undo">> => gtp_command_undo,
    <<"time_settings">> => gtp_command_time_settings,
    <<"time_left">> => gtp_command_time_left,
    <<"final_score">> => gtp_command_final_score,
    <<"final_status_list">> => gtp_command_final_status_list,
    <<"loadsgf">> => gtp_command_loadsgf,
    <<"reg_genmove">> => gtp_command_reg_genmove,
    <<"showboard">> => gtp_command_showboard
}).

%%%
%%% Responses
%%%

-type response_values() :: #{atom() => term()}.

-record(success, {values = #{} :: response_values()}).
-record(failure, {error_message :: binary()}).

-type response() :: #success{} | #failure{}.
