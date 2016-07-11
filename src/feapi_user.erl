-module(feapi_user).

-export([h/3]).

h(<<"POST">>,[<<"auth">>,<<"login">>], Req) ->
	axiom_session:set(username, undefined, Req),
	Request=apixiom:bodyjs(Req),
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
	{200, #{ok=>1}}.

%% PRIVATE METHODS

fetch_user(Username) ->
	{ok,Header,Users}=psql:equery(fe_pg,"select * from users where email=$1",[Username]),
	case Users of 
		[] ->
			{error, notfound};
		[_] ->
			[U]=feapi_tools:db2map(Header, Users),
			{ok, U}
	end.
check_password(User, Password) when is_binary(Password) or is_list(Password) ->
	EncPass=maps:get(encrypted_password, User, <<>>),
	<<Salt:29/binary,_Hash/binary>> = EncPass,
	{ok, NewHash} = bcrypt:hashpw(Password,Salt),
	NewHash=:=binary_to_list(EncPass).


