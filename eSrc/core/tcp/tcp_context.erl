-module(tcp_context, [CONTEXT]).
-export([fetch/0, store/1]).

-include("pmod.hrl").

fetch() -> erlang:get(CONTEXT).

store(SESSION) -> erlang:put(CONTEXT, SESSION).