-module(gtp_command).
-include("gtp.hrl").

-export([
    encode/2,
    decode/1
]).

%%%
%%% Encoding
%%%

encode(ID, Command) ->
    CommandMod = command_module(Command),
    Name = CommandMod:command_name(),
    Args = [[<<" ">>, A] || A <- CommandMod:encode_command_arguments(Command)],
    {[encode_optional_id(ID), Name, Args, <<"\n">>], CommandMod}.

encode_optional_id(undefined) -> <<"">>;
encode_optional_id(ID) -> [integer_to_binary(ID), <<" ">>].

%%%
%%% Decoding
%%%

decode(Binary) ->
    {IdOrCommand, Rest} = split_binary_with_rest(Binary, <<" ">>),
    case string:to_integer(binary_to_list(IdOrCommand)) of
        {error, no_integer} ->
            CommandMod = command_module(IdOrCommand),
            {undefined, CommandMod:decode_command_arguments(Rest), CommandMod};
        {ID, []} ->
            {Command, Args} = split_binary_with_rest(Rest, <<" ">>),
            CommandMod = command_module(Command),
            {ID, CommandMod:decode_command_arguments(Args), CommandMod}
    end.

split_binary_with_rest(Bin, Pattern) ->
    case binary:split(Bin, Pattern) of
        [C] -> {C, <<>>};
        [C, A] -> {C, A}
    end.

command_module(CommandName) when is_binary(CommandName) ->
    maps:get(CommandName, ?COMMAND_MODULES);
command_module(Command) when is_tuple(Command) ->
    CommandName = atom_to_binary(element(1, Command)),
    maps:get(CommandName, ?COMMAND_MODULES).
