-module(feapi).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0]).

start() ->
	application:ensure_all_started(feapi).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	application:ensure_all_started(cowboy),
	application:ensure_all_started(mongodb),
	application:ensure_all_started(bcrypt),
	feapi_sup:start_link().

stop(_State) ->
    ok.
