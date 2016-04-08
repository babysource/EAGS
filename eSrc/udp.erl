-module(udp).
-export([open/2, push/3]).

-include("udp.hrl").

-define(ELIB, "../elib").
-define(ELOG, ?ELIB ++ "/log4erl").
-define(EJSN, ?ELIB ++ "/rfc4627").

open(AGRT, PORT) when erlang:is_atom(AGRT) andalso erlang:is_integer(PORT) -> 
	case code:add_pathz("./core/udp") andalso code:add_pathz("./spec/udp") of
		false -> 
			exit({error, init_udp_exception});
		 true -> 
			case log:init(?ELOG) of ok -> ok; _ -> exit({error, init_log_exception}) end,
			case jsn:init(?EJSN) of ok -> ok; _ -> exit({error, init_jsn_exception}) end,
			(
				fun(ETID) when erlang:is_atom(ETID) -> 
					spawn(
						fun() -> 
							(
								fun({ok, SOCKET}) -> 
									% Concurrent receive ready.
									loop(AGRT, PORT, SOCKET)
								end
							)(
								gen_udp:open(PORT, [binary, {header, 0}, {active, true}, {reuseaddr, true}])
							)
						end
					)
				end
			)(
				ets:new(udpRoute, [public, set, named_table, {keypos, #udpChain.peer}])
			)
	end.

push(AGRT, IMEI, STRUCT) when erlang:is_atom(AGRT) -> todo.

loop(AGRT, PORT, SOCKET) -> 
	receive
		{udp, SOCKET, INHOST, INPORT, SOURCE} -> 
			(
				fun(PEER) -> 
					(
						fun(true) -> 
							log4erl:info("Udp Listening Bind Port[~p] From[~s] Join.", [PORT, PEER]),
							case erlang:is_binary(SOURCE) of
								false -> 
									% Concurrent receive ready.
									loop(AGRT, PORT, SOCKET);
								 true -> 
									(
										fun(BINARY) -> 
											(
												fun
													({bin, MEMORY}) when erlang:is_list(MEMORY) -> 
														case erlang:get(PEER) =:= undefined of
															true -> 
																% Concurrent receive ready. 
																loop(AGRT, PORT, SOCKET);
															false -> 
																(
																	fun(true) -> 
																		% Concurrent receive ready. 
																		loop(AGRT, PORT, SOCKET)
																	end
																)(
																	erlang:is_list(
																		erlang:put(PEER, MEMORY)
																	)
																)
														end;
													({err, REASON}) -> 
														log4erl:info("Udp Listening Bind Port[~p] From[~s] Skip:~p", [PORT, PEER, REASON])
												end
											)(
												try
													pack(AGRT, PORT, SOCKET, BINARY, ets:lookup_element(
														udpRoute, PEER, #udpChain.pipe
													))
												catch
													_:Ex -> {err, Ex}
												end
											)
										end
									)(
										binary:list_to_bin(erlang:get(PEER) ++ binary:bin_to_list(SOURCE))
									)
							end
						end
					)(
						case erlang:length(ets:lookup(udpRoute, PEER)) of
							1 -> 
								true;
							0 -> 
								(
									fun(undefined) -> 
										ets:insert(
											udpRoute, #udpChain{agrt=AGRT, peer=PEER, pipe=udp_channel:new(PORT, PEER, SOCKET)}
										)
									end
								)(
									erlang:put(PEER, [])
								)
						end
					)
				end
			)(
				lists:concat(
					[erlang:element(1, INHOST), '.', erlang:element(2, INHOST), '.', erlang:element(3, INHOST), '.', erlang:element(4, INHOST), ':', INPORT]
				)
			)
	end.

pack(AGRT, PORT, SOCKET, BINARY, CHANNEL) -> 
	case erlang:bit_size(BINARY) > 0 of
		false -> 
			{bin, binary:bin_to_list(BINARY)};
		 true -> 
			(
				fun({bin, PACKET, MEMORY}) when erlang:is_binary(PACKET) andalso erlang:is_binary(MEMORY) -> 
					if
						erlang:bit_size(PACKET) > 0 -> 
							log4erl:info("Udp Listening Bind Port[~p] From[~s] Recv:~p", [PORT, CHANNEL:peer(), PACKET]),
							(
								fun
									({err, REASON}) -> 
										(
											fun(_) -> 
												% Continue to decode.
												pack(AGRT, PORT, SOCKET, MEMORY, CHANNEL)
											end
										)(
											log4erl:warn("Udp Listening Bind Port[~p] From[~s] Warn:~p", [PORT, CHANNEL:peer(), REASON])
										);
									(_) -> 
										% Continue to decode.
										pack(AGRT, PORT, SOCKET, MEMORY, CHANNEL)
								end
							)(
								try
									{AGRT, {udp_protocol, CHANNEL}, CHANNEL}:decode(PACKET)
								catch
									_:Ex -> {err, Ex}
								end
							);
						true -> 
							{bin, binary:bin_to_list(MEMORY)}
					end
				end
			)(
				{AGRT, {udp_protocol, CHANNEL}, CHANNEL}:encode(BINARY)
			)
	end.