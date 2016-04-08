-module(tcp).
-export([open/2, push/3]).

-include("tcp.hrl").

-define(ELIB, "../elib").
-define(ELOG, ?ELIB ++ "/log4erl").
-define(EJSN, ?ELIB ++ "/rfc4627").

open(AGRT, PORT) when erlang:is_atom(AGRT) andalso erlang:is_integer(PORT) -> 
	case code:add_pathz("./core/tcp") andalso code:add_pathz("./spec/tcp") of
		false -> 
			exit({error, init_tcp_exception});
		 true -> 
			case log:init(?ELOG) of ok -> ok; _ -> exit({error, init_log_exception}) end,
			case jsn:init(?EJSN) of ok -> ok; _ -> exit({error, init_jsn_exception}) end,
			(
				fun(ETID) when erlang:is_atom(ETID) -> 
					(
						fun({ok, LISTEN}) -> 
							% Concurrent connect process.
							spawn(fun() -> dhcp(AGRT, PORT, LISTEN) end)
						end
					)(
						gen_tcp:listen(PORT, [binary, {packet, 0}, {active, true}, {reuseaddr, true}])
					)
				end
			)(
				ets:new(tcpRoute, [public, set, named_table, {keypos, #tcpChain.peer}])
			)
	end.

push(AGRT, IMEI, STRUCT) when erlang:is_atom(AGRT) -> todo.

dhcp(AGRT, PORT, LISTEN) -> 
	(
		fun({ok, SOCKET}) -> 
			% Concurrent connect process.
			spawn(fun() -> dhcp(AGRT, PORT, LISTEN) end),
			% Concurrent receive ready.
			loop(AGRT, PORT, SOCKET, [], (
				fun({ok, {{H1, H2, H3, H4}, HP}}) -> 
					(
						fun(PEER) -> 
							(
								fun({CONTEXT, CHANNEL}) -> 
									(
										fun(true) -> 
											CONTEXT:store({}),
											(
												fun(_) -> {CONTEXT, CHANNEL} end
											)(
												log4erl:info("Tcp Listening Bind Port[~p] From[~s] Join.", [PORT, PEER])
											)
										end
									)(
										ets:insert(tcpRoute, #tcpChain{agrt=AGRT, peer=PEER, pipe=CHANNEL})
									)
								end
							)({
								tcp_context:new(
									PEER
								),
								tcp_channel:new(
									PORT, PEER, SOCKET
								)
							})
						end
					)(
						lists:concat([H1, '.', H2, '.', H3, '.', H4, ':', HP])
					)
				end
			)(
				inet:peername(SOCKET)
			))
		end
	)(
		gen_tcp:accept(LISTEN)
	).

loop(AGRT, PORT, SOCKET, STREAM, {CONTEXT, CHANNEL}) -> 
	receive
		{tcp, SOCKET, SOURCE} -> 
			case erlang:is_binary(SOURCE) of
				false -> 
					% Concurrent receive ready.
					loop(AGRT, PORT, SOCKET, STREAM, {CONTEXT, CHANNEL});
				 true -> 
					(
						fun(BINARY) -> 
							(
								fun
									({bin, MEMORY}) when erlang:is_list(MEMORY) -> 
										% Concurrent receive ready. 
										loop(AGRT, PORT, SOCKET, MEMORY, {CONTEXT, CHANNEL});
									({err, REASON}) -> 
										(
											fun(ok) -> 
												(
													fun(true) -> 
														(
															fun(_) -> 
																log4erl:info("Tcp Listening Bind Port[~p] From[~s] Shut:~p", [PORT, CHANNEL:peer(), REASON])
															end
														)(
															erlang:erase(CHANNEL:peer())
														)
													end
												)(
													ets:delete(tcpRoute, CHANNEL:peer())
												)
											end
										)(
											gen_tcp:close(SOCKET)
										)
								end
							)(
								try
									pack(AGRT, PORT, SOCKET, BINARY, {CONTEXT, CHANNEL})
								catch
									_:Ex -> {err, Ex}
								end
							)
						end
					)(
						binary:list_to_bin(STREAM ++ binary:bin_to_list(SOURCE))
					)
			end;
		{tcp_closed, SOCKET} -> 
			(
				fun(true) -> 
					(
						fun(_) ->
							log4erl:info("Tcp Listening Bind Port[~p] From[~s] Exit.", [PORT, CHANNEL:peer()])
						end
					)(
						erlang:erase(CHANNEL:peer())
					)
				end
			)(
				ets:delete(tcpRoute, CHANNEL:peer())
			)
	end.

pack(AGRT, PORT, SOCKET, BINARY, {CONTEXT, CHANNEL}) -> 
	case erlang:bit_size(BINARY) > 0 of
		false -> 
			{bin, binary:bin_to_list(BINARY)};
		 true -> 
			(
				fun({bin, PACKET, MEMORY}) when erlang:is_binary(PACKET) andalso erlang:is_binary(MEMORY) -> 
					if
						erlang:bit_size(PACKET) > 0 -> 
							log4erl:info("Tcp Listening Bind Port[~p] From[~s] Recv:~p", [PORT, CHANNEL:peer(), PACKET]),
							(
								fun
									({err, REASON}) -> 
										(
											fun(_) -> 
												% Continue to decode.
												pack(AGRT, PORT, SOCKET, MEMORY, {CONTEXT, CHANNEL})
											end
										)(
											log4erl:warn("Tcp Listening Bind Port[~p] From[~s] Warn:~p", [PORT, CHANNEL:peer(), REASON])
										);
									(_) -> 
										% Continue to decode.
										pack(AGRT, PORT, SOCKET, MEMORY, {CONTEXT, CHANNEL})
								end
							)(
								try
									{AGRT, {tcp_protocol, CHANNEL}, CHANNEL}:decode(PACKET, CONTEXT)
								catch
									_:Ex -> {err, Ex}
								end
							);
						true -> 
							{bin, binary:bin_to_list(MEMORY)}
					end
				end
			)(
				{AGRT, {tcp_protocol, CHANNEL}, CHANNEL}:encode(BINARY, CONTEXT)
			)
	end.