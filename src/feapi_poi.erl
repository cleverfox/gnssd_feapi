-module(feapi_poi).

-export([h/3]).

h(<<"GET">>, [<<"poi">>,<<"get">>,I1,I2], Req) ->
	User=axiom_session:get(cur_user, Req),
	if is_map(User) -> ok;
	   true -> throw({return, 401, unauthorized})
	end,
	OrgId=maps:get(organisation_id,User),
	{ok, POIS} = dbapi:fetch_pois(
			"id between $2 and $3 and (organisation_id=0 or organisation_id is null or organisation_id = $1)",
		   [OrgId,
			binary_to_integer(I1),
			binary_to_integer(I2)
		   ]),
	{200, #{ok=>1,pois=>POIS} };

h(<<"GET">>, [<<"poi">>,<<"get">>,I1], Req) ->
	User=axiom_session:get(cur_user, Req),
	if is_map(User) -> ok;
	   true -> throw({return, 401, unauthorized})
	end,
	OrgId=maps:get(organisation_id,User),
	Id=binary_to_integer(I1),
	{ok, POIS} = dbapi:fetch_pois("id=$2 and (organisation_id=0 or organisation_id is null or organisation_id = $1)",[OrgId,Id]),
	{200, #{ok=>1,pois=>POIS} };


h(<<"GET">>, [<<"poi">>,<<"list">>], Req) ->
	User=axiom_session:get(cur_user, Req),
	if is_map(User) -> ok;
	   true -> throw({return, 401, unauthorized})
	end,
	OrgId=maps:get(organisation_id,User),
	{ok, POIS} = dbapi:list_pois("organisation_id=9990 or organisation_id is null or organisation_id = $1",[OrgId]),
	{200, #{ok=>1,pois=>POIS} }.

%select id,title,enabled,st_astext(coords),radius,descr,fill_color,stroke_color from pois limit 2
%select id,title,enabled,descr,fill_color,stroke_color from pois where organisation_id=0;
