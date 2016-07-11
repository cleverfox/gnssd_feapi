-module(feapi_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok,PID}=axiom:start(feapi_handlers,
						   [
							{content_type,"application/json"},
							{preprocessor, pre_hook},
							{postprocessor, post_hook},
							{sessions, []},
							{nb_acceptors, 100},
							{host, '_'},
							{port, 8090},
							{public, "public"}
						   ]),
	lager:info("Axiom pid ~p",[PID]),
	{RedisHost,RedisPort} = case application:get_env(redis) of 
					{ok, {RHost, RPort} } ->
						{RHost,RPort};
					_ ->
						{"127.0.0.1",6379}
				end,
	MongoCfg = case application:get_env(mongodb) of
				   {ok,X} when is_list(X) -> X;
				   _ -> 
					   lager:error("Can't get mongoDB configuration"),
					   []
			   end,
	{ok,
	 {_SupFlags = {one_for_one, 5, 10},
	  [
	   {   fe_pool_redis,
	       {poolboy,start_link,[
				    [{name,{local,fe_redis}},
				     {worker_module,eredis},
				     {size,3},
				     {max_overflow,5}
				    ],
				    [ {host, RedisHost}, 
				      {port, RedisPort}
				    ] 
				   ]}, 
	       permanent, 5000, worker,
	       [poolboy,eredis]
	   },
	   {   fe_pool_mongo,
		   {poolboy,start_link,[
								[{name,{local,fe_mng}},
								 {worker_module,mc_worker},
								 {size,3},
								 {max_overflow,5}
								],
								MongoCfg
							   ]},
		   permanent, 5000, worker, 
		   [poolboy,mc_worker]
	   },
	   {   fe_pool_postgres,
		   {poolboy,start_link,[
								[{name,{local,fe_pg}},
								 {worker_module,pgsql_worker},
								 {size,3},
								 {max_overflow,3}
								],
								[ ]
							   ]},
		   permanent, 5000, worker, 
		   [poolboy]
	   }
	  ]}
	}.

