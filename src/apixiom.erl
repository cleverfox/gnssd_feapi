-module(apixiom).

%% apixiom: apixiom library's entry point.

-export([
		 handle/4,
		 before_filter/1,
		 bodyjs/1
		]).


%% API

handle(Method, Path, Req, Target) ->
	case handle_json(Method, Path, Req, Target) of 
		{RCode, Body} when is_map(Body) ->
			{RCode, jsx:encode(Body)};
		{RCode, Headers, Body} when is_map(Body) ->
			{RCode, Headers, jsx:encode(Body)};
		Any ->
			lager:info("Res ~p",[Any]),
			Any
	end.

before_filter(Req) ->
	case cowboy_req:method(Req) of
		{<<"POST">>,_} ->
			try
				{ok, ReqBody, _} = cowboy_req:body(Req),
				ReqJSON=jsx:decode(ReqBody,[return_maps]),
				cowboy_req:set_meta(request_json, ReqJSON, Req)
			catch _:_ -> 
					  lager:info("Post witout or with broken body",[]),
					  cowboy_req:set_meta(request_json, #{}, Req)
			end;
		_ ->
			Req
	end.

bodyjs(Req) ->
	{ReqJSON,_}=cowboy_req:meta(request_json, Req),
	ReqJSON.


%% Internals

handle_json(Method, Path, Req, Target) ->
	try
		Target:h(Method,Path, Req)
	catch 
		throw:{return, Code, MSG} when is_list(MSG) ->
			{Code, #{error=>list_to_binary(MSG)}};
		throw:{return, Code, MSG} ->
			{Code, #{error=>MSG}};
		throw:{return, MSG} when is_list(MSG) ->
			{500, #{error=>list_to_binary(MSG)}};
		throw:{return, MSG} ->
			{500, #{error=>MSG}};
		error:function_clause ->
			case erlang:get_stacktrace() of
				[{Target,h,_,_}|_] ->
					{ReqPath, _} = cowboy_req:path(Req),
					{404, #{error=><<"not found">>,path=>ReqPath}};
				[{_,h,[Method,Path,_],_}|_] ->
					{ReqPath, _} = cowboy_req:path(Req),
					{404, #{error=><<"not found">>,path=>ReqPath}};
				Stack ->
					ST=iolist_to_binary(io_lib:format("~p",[Stack])),
					{400, #{error=>unknown,ecee=><<"error:function_clause">>,stack=>ST}}
			end;
		Ec:Ee ->
			EcEe=iolist_to_binary(io_lib:format("~p:~p",[Ec,Ee])),
			ST=iolist_to_binary(io_lib:format("~p",[erlang:get_stacktrace()])),
			{400, #{error=>unknown,ecee=>EcEe,stack=>ST}}
	end.

%% End of Module.
