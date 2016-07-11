-module(feapi_tools).

-export([db2map/2]).

db2map(Header,Payload) ->
	Header1=lists:map(fun({column,Title,_Type,_,_,_}) ->
							  binary_to_atom(Title,utf8)
					  end, Header),
	lists:map(fun(Item) ->
					  maps:from_list(lists:zip(Header1, tuple_to_list(Item)))
			  end, Payload).

