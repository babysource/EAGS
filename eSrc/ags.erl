-module(ags).
-compile(export_all).

-define(SEQ2, 16#0000).
-define(SEQ4, 16#00000000).

% Launch.
launch_tcp([AGRT, PORT]) when erlang:is_list(AGRT) andalso erlang:is_list(PORT) -> 
	spawn(
		fun() -> 
			tcp:open(erlang:list_to_atom(AGRT), erlang:list_to_integer(PORT)),
			receive
				after infinity -> ok
			end
		end
	).

launch_udp([AGRT, PORT]) when erlang:is_list(AGRT) andalso erlang:is_list(PORT) ->
	spawn(
		fun() -> 
			udp:open(erlang:list_to_atom(AGRT), erlang:list_to_integer(PORT)),
			receive
				after infinity -> ok
			end
		end
	).

% Serial.
serial_by_int(PEER) when erlang:is_list(PEER) -> 
	EKEY = PEER ++ ".Serial2",
	(
		fun(PERV) -> 
			case PERV of
				undefined -> 
					erlang:put(EKEY, ?SEQ2), ?SEQ2;
				_ -> 
					(
						fun(NEXT) -> 
							erlang:put(EKEY, NEXT), NEXT
						end
					)((PERV rem 16#FFFF) + 1)
			end
		end
	)(erlang:get(EKEY)).
serial_by_word(PEER) when erlang:is_list(PEER) -> 
	EKEY = PEER ++ ".Serial4",
	(
		fun(PERV) -> 
			case PERV of
				undefined -> 
					erlang:put(EKEY, ?SEQ4), ?SEQ4;
				_ -> 
					(
						fun(NEXT) -> 
							erlang:put(EKEY, NEXT), NEXT
						end
					)((PERV rem 16#FFFFFFFF) + 1)
			end
		end
	)(erlang:get(EKEY)).
serial_by_time(PEER) when erlang:is_list(PEER) -> 
	lgs:serial_by_time() ++ "@" ++ PEER.
serial_by_time() -> 
	(
		fun({MEGA, SECS, TINY}) -> 
			erlang:integer_to_list(MEGA * 1000000000 + SECS * 1000 + TINY div 1000, 16)
		end
	)(erlang:timestamp()).

% Verify.
verify_by_bxor(<<PERV:8, WAIT/binary>>) -> 
	case erlang:size(WAIT) > 0 of
		true -> ?MODULE:verify_by_bxor(WAIT, PERV); false -> PERV
	end.
verify_by_bxor(<<NEXT:8, WAIT/binary>>, PERV) -> 
	case erlang:size(WAIT) > 0 of
		true -> ?MODULE:verify_by_bxor(WAIT, PERV bxor NEXT); false -> PERV bxor NEXT
	end.

% Format.
format_by_date({YY, MM, DD}) -> 
	lists:concat([YY, '-', MM, '-', DD]).
format_by_time({HH, MI, SS}) -> 
	lists:concat([HH, ':', MI, ':', SS]).
format_by_datetime({{YY, MM, DD}, {HH, MI, SS}}) -> 
	lists:concat([YY, '-', MM, '-', DD, ' ', HH, ':', MI, ':', SS]).
