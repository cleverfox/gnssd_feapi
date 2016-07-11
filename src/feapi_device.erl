-module(feapi_device).

-export([h/3]).

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
	end.

%% PRIVATE API
%%

fetch_devices(Clause,Params) ->
	{ok,Header,Devices}=psql:equery(fe_pg,"select * from devices where "++Clause,Params),
	{ok,feapi_tools:db2map(Header, Devices)}.

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


