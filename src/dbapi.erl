-module(dbapi).

-export([db2map/2,
		 redis_hash_to_map/1,
		 fetch_devices/2,
		 fetch_pois/2,
		 list_pois/2,
		 list_devices/2,
		 get_devstate/1,
		 get_devsubs/1,
		 add_devsubs/3,
		 del_devsubs/2,
		 upd_devsubs/2
		]).

db2map(Header,Payload) ->
	Header1=lists:map(fun({column,Title,_Type,_,_,_}) ->
							  binary_to_atom(Title,utf8)
					  end, Header),
	lists:map(fun(Item) ->
					  maps:from_list(lists:zip(Header1, tuple_to_list(Item)))
			  end, Payload).
redis_hash_to_map(D) ->
	redis_hash_to_map(D,#{}).
redis_hash_to_map([],Accumulated) -> Accumulated;
redis_hash_to_map([Key,Val|Rest],Accumulated) ->
	redis_hash_to_map(Rest,
					  maps:put(Key, Val, Accumulated)
					 ).

geo2json(<<"MULTIPOINT(",Bin/binary>>) ->
	Points=hd(binary:split(Bin,<<")">>)),
	lists:map(fun(Pair) ->
					  [Bx,By]=binary:split(Pair,<<" ">>,[global]),
					  [
					   try binary_to_float(Bx) catch error:badarg -> binary_to_integer(Bx) end, 
					   try binary_to_float(By) catch error:badarg -> binary_to_integer(By) end
					  ]
			  end,
			  binary:split(Points,<<",">>,[global])
			 );

geo2json(Bin) ->
	Bin.

fetch_devices(Clause,Params) ->
	{ok,Header,Devices}=psql:equery(fe_pg,"select * from devices where "++Clause,Params),
	{ok,feapi_tools:db2map(Header, Devices)}.

list_pois(Clause,Params) ->
	{ok,Header,Devices}=psql:equery(fe_pg,"select id,kind,title,enabled,descr,fill_color,stroke_color,organisation_id from pois where "++Clause,Params),
	{ok,feapi_tools:db2map(Header, Devices)}.

fetch_pois(Clause,Params) ->
	{ok,Header,Devices}=psql:equery(fe_pg,"select id,kind,title,enabled,st_astext(coords) as coords,radius,descr,fill_color,stroke_color,organisation_id from pois where "++Clause,Params),
	{ok,
	 lists:map(fun(E) ->
					   maps:put(coords,geo2json(maps:get(coords,E)),E)
			   end,
			   feapi_tools:db2map(Header, Devices)
			  )}.

list_devices(Clause,Params) ->
	{ok,_Header,Devices}=psql:equery(fe_pg,"select id from devices where "++Clause,Params),
	{ok,lists:map(fun({N}) -> N end, Devices)}.

get_devstate(Device) ->
	RF=fun(W)->
			   case eredis:q(W,[ "hgetall", "device:lastpos:"++integer_to_list(Device) ]) of
				   {ok, List} ->
					   redis_hash_to_map(List);
				   Any -> 
					   lager:error("Redis returns ~p",[Any]),
					   #{}
			   end
	   end,
	poolboy:transaction(fe_redis,RF).

get_devsubs(SessionID) ->
	RF=fun(W)->
			   case eredis:q(W,[ "smembers", iolist_to_binary(["esub:position:",SessionID]) ]) of
				   {ok, List} ->
						List;
				   Any -> 
					   lager:error("Redis returns ~p",[Any]),
					   []
			   end
	   end,
	poolboy:transaction(fe_redis,RF).

add_devsubs(SessionID,Devices,Replace) ->
	RF=fun(W)->
			   if Replace -> eredis:q(W,[ "del", iolist_to_binary(["esub:position:",SessionID]) ]);
				  true -> ok
			   end,
			   R=case eredis:q(W,[ "sadd", iolist_to_binary(["esub:position:",SessionID]) |Devices ]) of
				   {ok, List} ->
					   List;
				   Any -> 
					   lager:error("Redis returns ~p",[Any]),
					   []
			   end,
			   eredis:q(W,[ "expire", iolist_to_binary(["esub:position:",SessionID]), 7200 ]),
			   eredis:q(W,[ "publish", "esub:position", iolist_to_binary(["esub:position:",SessionID]) ]),
			   R
	   end,
	poolboy:transaction(fe_redis,RF).

del_devsubs(SessionID,Devices) ->
	RF=fun(W)->
			   case eredis:q(W,[ "srem", iolist_to_binary(["esub:position:",SessionID]) |Devices ]) of
				   {ok, List} ->
					   eredis:q(W,[ "expire", iolist_to_binary(["esub:position:",SessionID]), 7200 ]),
					   eredis:q(W,[ "publish", "esub:position", iolist_to_binary(["esub:position:",SessionID]), 30 ]),
					   List;
				   Any -> 
					   lager:error("Redis returns ~p",[Any]),
					   []
			   end
	   end,
	poolboy:transaction(fe_redis,RF).


upd_devsubs(SessionID,Timeout) ->
	RF=fun(W)->
			   eredis:q(W,[ "ttl", iolist_to_binary(["esub:position:",SessionID]), Timeout ]),
			   eredis:q(W,[ "publish", "esub:position", iolist_to_binary(["esub:position:",SessionID]) ])
	   end,
	poolboy:transaction(fe_redis,RF).

