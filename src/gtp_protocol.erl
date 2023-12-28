-module(gtp_protocol).

-export([
    convert_tabs_to_spaces/1,
    remove_control_characters/1
]).

convert_tabs_to_spaces(Binary) ->
    binary:replace(Binary, <<"\t">>, <<"\n">>, [global]).

remove_control_characters(Binary) ->
    %% Control characters: 0-31 and 127
    %% Remove all of them except tab (9) and newline (10)
    binary:replace(Binary, control_characters_to_remove(), <<>>, [global]).

control_characters_to_remove() ->
    [
        <<1>>,
        <<2>>,
        <<3>>,
        <<4>>,
        <<5>>,
        <<6>>,
        <<7>>,
        <<8>>,
        <<11>>,
        <<12>>,
        <<13>>,
        <<14>>,
        <<15>>,
        <<16>>,
        <<17>>,
        <<18>>,
        <<19>>,
        <<20>>,
        <<21>>,
        <<22>>,
        <<23>>,
        <<24>>,
        <<25>>,
        <<26>>,
        <<27>>,
        <<28>>,
        <<29>>,
        <<30>>,
        <<31>>,
        <<127>>
    ].
