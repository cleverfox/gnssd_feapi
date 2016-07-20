-module(feapi_handlers).
%-compile(export_all).

-export([handle/3,before_filter/1,after_filter/1,after_filter/2,h/3]).

before_filter(Req) ->
	apixiom:before_filter(Req).

after_filter(_,_) -> ok.

after_filter(Req) ->
	{Origin,Req0}=cowboy_req:header(<<"origin">>,Req,<<"*">>),
	{AllHdrs,_}=cowboy_req:headers(Req),
	lager:info("Hdr ~p",[AllHdrs]),
	Req1=cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, Origin, Req0),
	Req2=cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"GET, POST, OPTIONS">>, Req1),
	Req3=cowboy_req:set_resp_header(<<"Access-Control-Allow-Credentials">>, <<"true">>, Req2),
	cowboy_req:set_resp_header(<<"Access-Control-Max-Age">>, <<"86400">>, Req3).

handle(Method, Path, Req) ->
	apixiom:handle(Method, Path, Req, ?MODULE).
	
h(Method, [<<"auth">>|_]=Path, Req) ->
	feapi_user:h(Method, Path, Req); 
h(Method, [<<"device">>|_]=Path, Req) ->
	feapi_device:h(Method, Path, Req); 
h(Method, [<<"sub">>|_]=Path, Req) ->
	feapi_subs:h(Method, Path, Req); 
h(Method, [<<"poi">>|_]=Path, Req) ->
	feapi_poi:h(Method, Path, Req); 


h(<<"GET">>, [<<"hi">>, Name], _Req) ->
    [<<"<h1>Hello, ">>, Name, <<"!</h1>">>];

h(<<"GET">>, [], Req) ->
    {_Path, Req1} = cowboy_req:path(Req),
    Req2 = cowboy_req:set_meta(resp_status, 200, Req1),
    Body = axiom:dtl(index, []),
    cowboy_req:set_resp_body(Body, Req2).

%PRIVATE API


