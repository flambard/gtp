-module(gtp_io).

-export([request_line/2]).

request_line(IoDevice, Prompt) ->
    Request = {get_line, latin1, Prompt},
    Ref = make_ref(),
    IoDevice ! {io_request, self(), Ref, Request},
    Ref.
