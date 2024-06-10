-module(gtp_engine_io_tests).

-include_lib("eunit/include/eunit.hrl").

io_test() ->
    Self = self(),
    IoServer =
        spawn(fun() ->
                 receive
                     {io_request, From1, ReplyAs1, {get_line, latin1, _Prompt}} ->
                         From1 ! {io_reply, ReplyAs1, <<"protocol_version\n">>}
                 end,
                 receive
                     %% The two lines are sent in one message currently
                     {io_request, From2, ReplyAs2, {put_chars, latin1, <<"= 2\n\n">>}} ->
                         From2 ! {io_reply, ReplyAs2, ok}
                 end,
                 Self ! done
              end),

    {ok, Engine} = gtp_bogus_engine:start_link(),

    {ok, _EC} = gtp_engine_channel:start_link(gtp_bogus_engine, Engine, IoServer, []),

    receive
        done ->
            ok
    end.
