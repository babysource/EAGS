-module(tcp_channel, [PORT, PEER, SOCKET]).
-export([auth/1, agrt/0, port/0, peer/0, imei/0, cert/0, stop/0, mail/1, bind/1, send/1]).

-include("tcp.hrl").
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
	case erlang:length(ets:lookup(tcpRoute, PEER)) of
		0 -> 
			none;
		1 -> 
			ets:lookup_element(
				tcpRoute, PEER, #tcpChain.agrt
			)
	end.

port() -> PORT.

peer() -> PEER.

imei() -> 
	case erlang:length(ets:lookup(tcpRoute, PEER)) of
		0 -> 
			undefined;
		1 -> 
			ets:lookup_element(
				tcpRoute, PEER, #tcpChain.imei
			)
	end.

cert() -> 
	case erlang:length(ets:lookup(tcpRoute, PEER)) of
		0 -> 
			undefined;
		1 -> 
			ets:lookup_element(
				tcpRoute, PEER, #tcpChain.cert
			)
	end.

stop() -> 
	(
		fun(ok) -> 
			log4erl:info("Tcp Listening Bind Port[~p] From[~s] Stop.", [PORT, PEER])
		end
	)(
		gen_tcp:close(SOCKET)
	).

mail({biz, [CTRL, DATA]}) -> 
	(
		fun(IMEI) when IMEI /= undefined -> 
			(
				fun(MAIL) -> 
					log4erl:info("Tcp Listening Bind Port[~p] From[~s] Mail:~s", [PORT, PEER, MAIL]),
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
			log4erl:info("Tcp Listening Bind Port[~p] From[~s] Mail:~s", [PORT, PEER, MAIL]),
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
		tcpRoute, PEER, {#tcpChain.cert, TOKEN}
	);
bind({imei, IDENT}) when erlang:is_list(IDENT) -> 
	(
		fun(_) -> 
			ets:update_element(
				tcpRoute, PEER, {#tcpChain.imei, IDENT}
			)
		end
	)(
		ets:select_delete(tcpRoute, [{#tcpChain{peer='$1', imei=IDENT, _='_'}, [{'/=', '$1', PEER}], [true]}])
	);
bind([{imei, IDENT}, {cert, TOKEN}]) when erlang:is_list(IDENT) -> 
	THIS:bind({imei, IDENT}) andalso THIS:bind({cert, TOKEN});
bind([{cert, TOKEN}, {imei, IDENT}]) when erlang:is_list(IDENT) -> 
	THIS:bind({imei, IDENT}) andalso THIS:bind({cert, TOKEN}).

send(STREAM) when erlang:is_binary(STREAM) andalso erlang:bit_size(STREAM) > 0 -> 
	case gen_tcp:send(SOCKET, STREAM) of
					 ok -> log4erl:info("Tcp Listening Bind Port[~p] From[~s] Send[ok]:~p", [PORT, PEER, STREAM]);
		{error, REASON} -> log4erl:info("Tcp Listening Bind Port[~p] From[~s] Send[~p]:~p", [PORT, PEER, REASON, STREAM])
	end.