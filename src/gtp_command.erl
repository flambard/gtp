-module(gtp_command).
-include("gtp.hrl").

-export([
    encode/3,
    decode/2
]).

%%%
%%% Encoding
%%%

encode(ID, Command, undefined) ->
    encode(ID, Command, command_module(Command));
encode(ID, Command, CommandMod) ->
    Name = CommandMod:command_name(),
    Args = [[<<" ">>, A] || A <- CommandMod:encode_command_arguments(Command)],
    {[encode_optional_id(ID), Name, Args, <<"\n">>], CommandMod}.

encode_optional_id(undefined) -> <<"">>;
encode_optional_id(ID) -> [integer_to_binary(ID), <<" ">>].

%%%
%%% Decoding
%%%

decode(Binary, ExtensionCommands) ->
    {IdOrCommand, Rest} = split_binary_with_rest(Binary, <<" ">>),
    case string:to_integer(binary_to_list(IdOrCommand)) of
        {error, no_integer} ->
            CommandMod = command_module(IdOrCommand, ExtensionCommands),
            {undefined, CommandMod:decode_command_arguments(Rest), CommandMod};
        {ID, []} ->
            {Command, Args} = split_binary_with_rest(Rest, <<" ">>),
            CommandMod = command_module(Command, ExtensionCommands),
            {ID, CommandMod:decode_command_arguments(Args), CommandMod}
    end.

split_binary_with_rest(Bin, Pattern) ->
    case binary:split(Bin, Pattern) of
        [C] -> {C, <<>>};
        [C, A] -> {C, A}
    end.

command_module(Command) when is_tuple(Command) ->
    CommandName = atom_to_binary(element(1, Command)),
    maps:get(CommandName, ?COMMAND_MODULES).

command_module(CommandName, ExtensionCommands) ->
    case ExtensionCommands of
        #{CommandName := CommandMod} -> CommandMod;
        #{} -> maps:get(CommandName, ?COMMAND_MODULES)
    end.
