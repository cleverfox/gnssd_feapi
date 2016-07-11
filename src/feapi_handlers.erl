-module(feapi_handlers).
%-compile(export_all).

-export([handle/3,fetch_users/0,before_filter/1,after_filter/1,h/3]).

before_filter(Req) ->
	apixiom:before_filter(Req).

after_filter(Req) ->
	cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req).

handle(Method, Path, Req) ->
	apixiom:handle(Method, Path, Req, ?MODULE).
	
h(<<"POST">>,[<<"auth">>,<<"login">>], Req) ->
	axiom_session:set(username, undefined, Req),
	Request=bodyjs(Req),
	Username=maps:get(<<"username">>,Request),
	Password=maps:get(<<"password">>,Request),
	U = case fetch_user(Username) of
			{ok, U1} -> U1;
			_ -> throw({return,401,nouser})
		end,
	case check_password(U, Password) of
		true ->
			axiom_session:set(username, Username, Req),
			axiom_session:set(cur_user, U, Req),
			{200, #{ok=>1, user=>maps:remove(encrypted_password,U) } };
		false ->
			{401, #{error=>unauthorized}}
	end;
	

h(<<"GET">>, [<<"auth">>,<<"whoami">>], Req) ->
	case axiom_session:get(cur_user, Req) of
		User when is_map(User) ->
			{200, #{user=>maps:remove(encrypted_password,User)}};
		undefined -> 
			{401, #{error=>unauthorized}}
	end;
	
h(<<"POST">>, [<<"auth">>,<<"logout">>], Req) ->
	axiom_session:delete(Req),
	{200, #{ok=>1}};

h(<<"GET">>, [<<"devices">>,<<"list">>], Req) ->
	User=axiom_session:get(cur_user, Req),
	if is_map(User) -> ok;
	   true -> throw({return, 401, unauthorized})
	end,
	OrgId=maps:get(organisation_id,User),
	{ok, Devlist} =fetch_devices("organisation_id=$1",[OrgId]),
	Devlist1=lists:map(fun(E) ->
							   ID=maps:get(id,E),
							   #{ id => ID,
								  title => maps:get(title,E),
								  icon => maps:get(icon,E),
								  imei => maps:get(imei,E),
								  organisation_id => maps:get(organisation_id,E),
								  number_plate => maps:get(number_plate,E),
								  current_info => get_devstate(ID)
								}
					   end,Devlist),
	lager:info("Devs ~p",[Devlist1]),
	{200, #{ok=>1,devices=>Devlist1} };

h(<<"GET">>, [<<"device">>,<<"track">>,DeviceIDB,StartB,StopB], Req) ->
	User=axiom_session:get(cur_user, Req),
	if is_map(User) -> ok;
	   true -> throw({return, 401, unauthorized})
	end,
	OrgId=maps:get(organisation_id,User),
	DeviceID=binary_to_integer(DeviceIDB),
	{ok, Devlist} =fetch_devices("organisation_id=$1 and id=$2",[OrgId,DeviceID]),
	case Devlist of 
		[] ->
			{403, #{error=>denied}};
		[_] ->
			Start=binary_to_integer(StartB),
			Stop=binary_to_integer(StopB),
			StartH=Start div 3600,
			StopH=Stop div 3600,
			Selector={
			  type,devicedata,
			  device,DeviceID,
			  hour,{
				'$gte',StartH,
				'$lte',StopH
			   }
			 },
			View={ hour,1, '_id',0, 'data.dt',1, 'data.position',1, 'data.sp',1,'data.stop',1 },
			N=mng:find(fe_mng,<<"devicedata">>,Selector,View),
			Result=lists:foldl(fun(Ne,Acc)  ->
									   E=mng:m2proplistr(Ne),
									   [ #{ data => [ maps:from_list( Ed) || Ed <- proplists:get_value(<<"data">>,E) ]
										  } | Acc ]
							   end, [], mc_cursor:rest(N) ),
			%lager:info("Devs ~p ~p ~p",[Selector,DeviceID,Result]),
			mc_cursor:close(N),
			lager:info("H ~p",[Result]),
			{200, #{ok=>1,data=>Result} }
	end;

h(<<"GET">>, [<<"hi">>, Name], _Req) ->
    [<<"<h1>Hello, ">>, Name, <<"!</h1>">>];

h(<<"GET">>, [], Req) ->
    {_Path, Req1} = cowboy_req:path(Req),
    Req2 = cowboy_req:set_meta(resp_status, 200, Req1),
    Body = axiom:dtl(index, []),
    cowboy_req:set_resp_body(Body, Req2).

%private api

bodyjs(Req) ->
	{ReqJSON,_}=cowboy_req:meta(request_json, Req),
	ReqJSON.

get_devstate(Device) ->
	RF=fun(W)->
			   case eredis:q(W,[ "hgetall", "device:lastpos:"++integer_to_list(Device) ]) of
				   {ok, List} ->
					   redis_hash_to_map(List,#{});
				   Any -> 
					   lager:error("Redis returns ~p",[Any]),
					   #{}
			   end
	   end,
	poolboy:transaction(fe_redis,RF).

redis_hash_to_map([],Accumulated) -> Accumulated;
redis_hash_to_map([Key,Val|Rest],Accumulated) ->
	redis_hash_to_map(Rest,
					  maps:put(Key, Val, Accumulated)
					 ).


db2map(Header,Payload) ->
	Header1=lists:map(fun({column,Title,_Type,_,_,_}) ->
							  binary_to_atom(Title,utf8)
					  end, Header),
	lists:map(fun(Item) ->
					  maps:from_list(lists:zip(Header1, tuple_to_list(Item)))
			  end, Payload).

fetch_user(Username) ->
	{ok,Header,Users}=psql:equery(fe_pg,"select * from users where email=$1",[Username]),
	case Users of 
		[] ->
			{error, notfound};
		[_] ->
			[U]=db2map(Header, Users),
			{ok, U}
	end.

fetch_devices(Clause,Params) ->
	{ok,Header,Devices}=psql:equery(fe_pg,"select * from devices where "++Clause,Params),
	{ok,db2map(Header, Devices)}.

check_password(User, Password) when is_binary(Password) or is_list(Password) ->
	EncPass=maps:get(encrypted_password, User, <<>>),
	<<Salt:29/binary,_Hash/binary>> = EncPass,
	{ok, NewHash} = bcrypt:hashpw(Password,Salt),
	NewHash=:=binary_to_list(EncPass).

fetch_users() ->
	{ok,Header,Users}=psql:equery(fe_pg,"select * from users",[]),
	U=db2map(Header, Users),
	{ok, U}.

