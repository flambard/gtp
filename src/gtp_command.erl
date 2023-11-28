-module(gtp_command).
-include("gtp.hrl").
-include("gtp_commands.hrl").

-callback encode_command(Command :: command()) -> EncodedCommand :: iodata().

-callback decode_command_arguments(EncodedArgs :: binary()) -> command().

-callback encode_response_values(ResponseValues :: #{atom() => term()}) ->
    EncodedResponseValues :: [iodata()].

-callback decode_response_values(EncodedResponseValues :: [binary()]) -> Response :: response().

-export([
    encode_optional_id/1,
    decode_command_message/1,
    command_module/1
]).

encode_optional_id(undefined) -> <<"">>;
encode_optional_id(ID) -> [integer_to_binary(ID), <<" ">>].

decode_command_message(Binary) ->
    {IdOrCommand, Rest} = split_binary_with_rest(Binary, <<" ">>),
    case string:to_integer(binary_to_list(IdOrCommand)) of
        {error, no_integer} ->
            #{
                id => undefined,
                command => IdOrCommand,
                module => command_module(IdOrCommand),
                arguments => Rest
            };
        {ID, []} ->
            {Command, Args} = split_binary_with_rest(Rest, <<" ">>),
            #{
                id => ID,
                command => Command,
                module => command_module(Command),
                arguments => Args
            }
    end.

split_binary_with_rest(Bin, Pattern) ->
    case binary:split(Bin, Pattern) of
        [C] -> {C, <<>>};
        [C, A] -> {C, A}
    end.

command_module(CommandName) when is_binary(CommandName) ->
    case CommandName of
        <<"protocol_version">> -> gtp_command_protocol_version;
        <<"quit">> -> gtp_command_quit
    end;
command_module(Command) ->
    case Command of
        #protocol_version{} -> gtp_command_protocol_version;
        #quit{} -> gtp_command_quit
    end.
