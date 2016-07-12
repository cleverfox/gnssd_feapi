-module(feapi_subs).

-export([h/3]).

h(<<"GET">>, [<<"sub">>,<<"list">>], Req) ->
	User=axiom_session:get(cur_user, Req),
	if is_map(User) -> ok;
	   true -> throw({return, 401, unauthorized})
	end,
	{SessionID, _Req1} = cowboy_req:cookie(<<"session_id">>, Req),
	Devs=dbapi:get_devsubs(SessionID),
	{200, #{ok=>1,subs=>Devs} };

h(<<"GET">>, [<<"sub">>,<<"refresh">>], Req) ->
	User=axiom_session:get(cur_user, Req),
	if is_map(User) -> ok;
	   true -> throw({return, 401, unauthorized})
	end,
	{SessionID, _Req1} = cowboy_req:cookie(<<"session_id">>, Req),
	{ok,R}=dbapi:upd_devsubs(SessionID,7200),
	lager:info("~p",[R]),
	{200, #{ok=>1, res=>R} };

h(<<"POST">>, [<<"sub">>,<<"subscribe">>], Req) ->
	User=axiom_session:get(cur_user, Req),
	if is_map(User) -> ok;
	   true -> throw({return, 401, unauthorized})
	end,
	Request=apixiom:bodyjs(Req),
	{SessionID, _Req1} = cowboy_req:cookie(<<"session_id">>, Req),
	OrgId=maps:get(organisation_id,User),
	{ok, AllowedDevlist} = dbapi:list_devices("organisation_id=$1",[OrgId]),
	AddSub=maps:get(<<"subscribe">>,Request,[]),
	Clear=case maps:get(<<"clear">>,Request,false) of true -> true; _ -> false end,
	RealAdd=lists:filter(fun(I) when is_integer(I) ->
						 lists:member(I,AllowedDevlist);
					(B) when is_binary(B) ->
						 I=binary_to_integer(B),
						 lists:member(I,AllowedDevlist)
				 end, AddSub),
	Stat=dbapi:add_devsubs(SessionID,RealAdd,Clear),
	Subs=dbapi:get_devsubs(SessionID),
	{200, #{ok=>1, added=>Stat, subs=>Subs} }.

