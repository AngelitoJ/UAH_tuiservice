%% TUI Service project
%% (C) 2015 Angel J. Alvarez Miguel


-module(json).
-author("angeljalvarezmiguel@gmail.com").
-export([term_to_json/1]).


%% Quick and dirty JSON like producing functions..


term_to_json({ok, Content}) ->
	Data = term_to_json(Content),
	<<"{ \"status\" : \"200\" , ", Data/binary, " }">>;

term_to_json({error, Code}) ->
	Status = case Code of
		user_not_found        -> <<"-1">>;
		service_not_available -> <<"-2">>;
		not_authorized        -> <<"-3">>;
		missing_parameters    -> <<"-4">>;
		bad_parameter         -> <<"-5">>;
		unknown_error         -> <<"-100">>
	end,
	<<"{ \"status\" : ", Status/binary ," }">>;

term_to_json({user_info, Login, Id, Roles, Name, PhotoURL }) ->
	L = term_to_json({string, login_id, Login}),
	I = term_to_json({string, new_user_id, Id}),
	R = term_to_json({array, roles, Roles}),
	N = term_to_json({string, name, Name}),
	P = term_to_json({string, photo, PhotoURL}),
	<<"\"content\" : { " , L/binary, ",", I/binary, ",", R/binary, ",", N/binary, ",", P/binary, " }">>;

term_to_json({array, Name, List}) when is_atom(Name), is_list(List) ->
	SName        = erlang:atom_to_list(Name),
	BName        = list_to_binary(quote(SName)),
	[First|Rest] = lists:map(fun quote/1, List),
	Values       = lists:foldl(fun(I,Acc) -> [I | "," ++ Acc] end, First ,Rest),
	BValues      = list_to_binary(Values),
	<<BName/binary," : [ ",BValues/binary, " ]">>;

term_to_json({string, Name, Val}) when is_atom(Name), is_binary(Val) ->
	SName = erlang:atom_to_list(Name),
	BName = list_to_binary(quote(SName)),
	<<BName/binary," : \"",Val/binary,"\"">>;

term_to_json({string, Name, Val}) when is_atom(Name), is_list(Val) ->
	SName = erlang:atom_to_list(Name),
	BName = list_to_binary(quote(SName)),
	BVal  = list_to_binary(quote(Val)),
	<<BName/binary," : ",BVal/binary>>;

term_to_json(Other) ->
	io:format("JSON term not understood ~p\n",[Other]),
	<<"">>.


quote(String) -> "\"" ++ String ++ "\"". 