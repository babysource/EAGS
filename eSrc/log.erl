-module(log).
-export([init/1]).

-define(ELOG_BEAM, "/ebin").
-define(ELOG_CONF, "/conf/log4erl.conf").

init(ELOG) -> 
	(
		fun(EBIN) -> 
			case EBIN of
				false -> 
					no;
				 true -> 
					(
						fun(LERL) -> 
							if LERL =:= ok -> log4erl:conf(ELOG ++ ?ELOG_CONF); true -> no end
						end
					)(
						application:start(log4erl)
					)
			end
		end
	)(
		 code:add_pathz(ELOG ++ ?ELOG_BEAM)
	).