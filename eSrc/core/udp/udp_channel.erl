-module(udp_channel, [PORT, PEER, SOCKET]).
-export([auth/1, agrt/0, port/0, peer/0, imei/0, cert/0, wipe/0, mail/1, bind/1, send/1]).

-include("udp.hrl").
-include("pmod.hrl").

auth(EQUI) when erlang:is_binary(EQUI) -> 
	(
		fun(IMEI) -> 
			case not erlang:is_list(IMEI) of
				true ->
					false;
				false ->
					erlang:list_to_binary(IMEI) == EQUI
			end
		end
	)(
		THIS:imei()
	).

agrt() -> 
	case erlang:length(ets:lookup(udpRoute, PEER)) of
		0 -> 
			none;
		1 -> 
			ets:lookup_element(
				udpRoute, PEER, #udpChain.agrt
			)
	end.

port() -> PORT.

peer() -> PEER.

imei() -> 
	case erlang:length(ets:lookup(udpRoute, PEER)) of
		0 -> 
			undefined;
		1 -> 
			ets:lookup_element(
				udpRoute, PEER, #udpChain.imei
			)
	end.

cert() -> 
	case erlang:length(ets:lookup(udpRoute, PEER)) of
		0 -> 
			undefined;
		1 -> 
			ets:lookup_element(
				udpRoute, PEER, #udpChain.cert
			)
	end.

wipe() -> 
	(
		fun(true) -> 
			(
				fun(true)-> 
					log4erl:info("Udp Listening Bind Port[~p] From[~s] Wipe.", [PORT, PEER])
				end
			)(
				erlang:is_list(
					erlang:erase(PEER)
				)
			)
		end
	)(
		ets:delete(udpRoute, PEER)
	).

mail({biz, [CTRL, DATA]}) -> 
	(
		fun(IMEI) when IMEI /= undefined -> 
			(
				fun(MAIL) -> 
					log4erl:info("Udp Listening Bind Port[~p] From[~s] Mail:~s", [PORT, PEER, MAIL]),
					(
						fun(STAMP) -> 
							{'BIZ-MAIL', ?ERPC} ! {self(), MAIL, STAMP},
							receive
								{'BIZ-RESP', STAMP, REPLY} when erlang:is_binary(REPLY) -> 
									REPLY;
								{'BIZ-NONE', STAMP} -> 
									<<>>
							after
								3000 -> 
									<<>>
							end
						end
					)(
						lgs:serial_by_time(PEER)
					)
				end
			)(
				jsn:term_to_json({[
					{imei, erlang:list_to_binary(IMEI)},
					{ctrl, CTRL},
					{data, DATA}
				]})
			)
		end
	)(
		THIS:imei()
	);
mail({lic, [GUID, IMEI, IMSI, IMAC, VERN]}) -> 
	(
		fun(MAIL) -> 
			log4erl:info("Udp Listening Bind Port[~p] From[~s] Mail:~s", [PORT, PEER, MAIL]),
			(
				fun(STAMP) -> 
					{'LIC-MAIL', ?ERPC} ! {self(), MAIL, STAMP},
					receive
						{'LIC-RESP', STAMP, IDENT, TOKEN} -> 
							THIS:bind([
								{imei, IDENT},
								{cert, TOKEN}
							]);
						{'LIC-NONE', STAMP} -> 
							false
					after
						1500 -> 
							false
					end
				end
			)(
				lgs:serial_by_time(PEER)
			)
		end
	)(
		jsn:term_to_json({[
			{guid, GUID},
			{imei, IMEI},
			{imsi, IMSI},
			{imac, IMAC},
			{vern, VERN}
		]})
	).

bind({cert, TOKEN}) -> 
	ets:update_element(
		udpRoute, PEER, {#udpChain.cert, TOKEN}
	);
bind({imei, IDENT}) when erlang:is_list(IDENT) -> 
	(
		fun(_) -> 
			ets:update_element(
				udpRoute, PEER, {#udpChain.imei, IDENT}
			)
		end
	)(
		[(fun(true) -> erlang:erase(X) end)(ets:delete(udpRoute, X)) || X <- ets:select(udpRoute, [
			{#udpChain{peer='$1', imei=IDENT, _='_'}, [{'/=', '$1', PEER}], ['$1']}
		])]
	);
bind([{imei, IDENT}, {cert, TOKEN}]) when erlang:is_list(IDENT) -> 
	THIS:bind({imei, IDENT}) andalso THIS:bind({cert, TOKEN});
bind([{cert, TOKEN}, {imei, IDENT}]) when erlang:is_list(IDENT) -> 
	THIS:bind({imei, IDENT}) andalso THIS:bind({cert, TOKEN}).

send(STREAM) when erlang:is_binary(STREAM) andalso erlang:bit_size(STREAM) > 0 -> 
	(
		fun([H1, H2, H3, H4, HP]) -> 
			case gen_udp:send(SOCKET, {H1, H2, H3, H4}, HP, STREAM) of
							 ok -> log4erl:info("Udp Listening Bind Port[~p] From[~s] Send[ok]:~p", [PORT, PEER, STREAM]);
				{error, REASON} -> log4erl:info("Udp Listening Bind Port[~p] From[~s] Send[~p]:~p", [PORT, PEER, REASON, STREAM])
			end
		end
	)(
		[erlang:list_to_integer(X) || X <- string:tokens(PEER, ".:")]
	).