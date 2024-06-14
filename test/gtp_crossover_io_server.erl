-module(gtp_crossover_io_server).

-behaviour(gen_server).

%%
%%                      Crossover I/O server
%%   I/O client A         +-------------+         I/O client B
%% +--------------+       |             |       +--------------+
%% |        out ->|------>|----\   /----|<------|<- out        |
%% |              |       |     \ /     |       |              |
%% |              |       |      X      |       |              |
%% |              |       |     / \     |       |              |
%% |         in <-|<------|<---/   \--->|------>|-> in         |
%% +--------------+       |             |       +--------------+
%%                        +-------------+
%%

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%%%
%%% API
%%%

start_link(Options) ->
    gen_server:start_link(?MODULE, [], Options).

%%%
%%% gen_server callbacks
%%%

-record(client, {pid = undefined, reply_as = undefined, buffer = <<>>}).

init([]) ->
    State = #{client_a => #client{}, client_b => #client{}},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({io_request, Client, ReplyAs, Request}, State) ->
    {Side, State1} = register_client(State, Client),
    State2 = handle_io_request(Request, Side, Client, ReplyAs, State1),
    {noreply, State2};
handle_info(Message, State) ->
    erlang:display({unexpected_info, Message}),
    {noreply, State}.

%%%
%%% Private functions
%%%

register_client(State, Client) ->
    case State of
        #{client_a := #client{pid = Client}} ->
            {client_a, State};
        #{client_b := #client{pid = Client}} ->
            {client_b, State};
        #{client_a := #client{pid = undefined} = C} ->
            {client_a, State#{client_a := C#client{pid = Client}}};
        #{client_b := #client{pid = undefined} = C} ->
            {client_b, State#{client_b := C#client{pid = Client}}}
    end.

handle_io_request({get_line, latin1, _Prompt}, Side, From, ReplyAs, State) ->
    %%
    %% IF there is a complete line in the buffer
    %% THEN pop a line from the buffer and send it
    %% ELSE store the reply_as reference
    %%
    #{Side := #client{pid = From, buffer = Buffer} = C} = State,
    NewClient =
        case binary:split(Buffer, <<"\n">>) of
            [Buffer] ->
                C#client{reply_as = ReplyAs};
            [Line, Rest] ->
                From ! {io_reply, ReplyAs, <<Line/binary, "\n">>},
                C#client{buffer = Rest}
        end,
    State#{Side := NewClient};
handle_io_request({put_chars, latin1, Chars}, Side, From, ReplyAs, State) ->
    %%
    %% 1. Put the chars into the other clients buffer.
    %%
    %% 2. IF the other client has a waiting get_line request
    %%    AND there is a complete line in the buffer
    %%    THEN pop a line from the buffer and send it to the other client.
    %%
    %% 3. Reply 'ok' immediately to the sender
    %%
    OtherSide = other(Side),
    #{
        OtherSide :=
            #client{
                pid = Pid,
                reply_as = Ref,
                buffer = Buffer
            } =
            C
    } =
        State,
    Buffer1 = <<Buffer/binary, Chars/binary>>,
    NewClient =
        case Ref of
            undefined ->
                C#client{buffer = Buffer1};
            Ref ->
                case binary:split(Buffer1, <<"\n">>) of
                    [Buffer1] ->
                        C#client{buffer = Buffer1};
                    [Line, Rest] ->
                        Pid ! {io_reply, Ref, <<Line/binary, "\n">>},
                        C#client{reply_as = undefined, buffer = Rest}
                end
        end,
    From ! {io_reply, ReplyAs, ok},
    State#{OtherSide := NewClient}.

other(client_a) ->
    client_b;
other(client_b) ->
    client_a.
