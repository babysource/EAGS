-module(jsn).
-export([init/1, term_to_json/1, json_to_term/1]).

-define(EJSN_BEAM, "/ebin").

init(EJSN) -> 
	(
		fun(EBIN) -> 
			if EBIN -> ok; true -> no end
		end
	)(
		code:add_pathz(EJSN ++ ?EJSN_BEAM)
	).

term_to_json(TERM) -> rfc4627:encode(TERM).

json_to_term(JSON) -> rfc4627:decode(JSON).