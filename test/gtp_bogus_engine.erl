-module(gtp_bogus_engine).
-behaviour(gtp_engine).
-include_lib("gtp.hrl").

-export([
    handle_protocol_version/1,
    handle_quit/1
]).

handle_protocol_version(_Engine) ->
    {ok, #{version_number => 2}}.

handle_quit(_Engine) ->
    {ok, #{}}.
