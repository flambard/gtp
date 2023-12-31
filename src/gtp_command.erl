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
    {OptionalID, CommandName, Arguments} =
        case gtp_entity:decode({alternative, [int, string], string}, Binary) of
            {[ID, Command], ArgsBin} -> {ID, Command, iolist_to_binary(ArgsBin)};
            {Command, ArgsBin} -> {undefined, Command, iolist_to_binary(ArgsBin)}
        end,
    CommandMod = command_module(CommandName, ExtensionCommands),
    {OptionalID, CommandMod:decode_command_arguments(Arguments), CommandMod}.

%%%
%%% Private functions
%%%

command_module(Command) when is_tuple(Command) ->
    CommandName = atom_to_binary(element(1, Command)),
    maps:get(CommandName, ?COMMAND_MODULES).

command_module(CommandName, ExtensionCommands) ->
    case ExtensionCommands of
        #{CommandName := CommandMod} -> CommandMod;
        #{} -> maps:get(CommandName, ?COMMAND_MODULES)
    end.
