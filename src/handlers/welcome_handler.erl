%% TUI Service project
%% (C) 2015 Angel J. Alvarez Miguel

%% original code based on cowboy examples 

% @doc Hello world handler.
-module(welcome_handler).
-author("angeljalvarezmiguel@gmail.com").
-compile([export_all]).

-export([init/3]).
-export([content_types_provided/2]).
-export([welcome_to_html/2, welcome_to_text/2, welcome_to_json/2]).



init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[
		{<<"text/html">>,        welcome_to_html},
		{<<"application/json">>, welcome_to_json},
		{<<"text/plain">>,       welcome_to_text}
	], Req, State}.


welcome_to_html(Req, State) ->
	welcome_controller(Req, State, fun term_to_html/1).

welcome_to_text(Req, State) ->
	welcome_controller(Req, State, fun term_to_text/1).

welcome_to_json(Req, State) ->
	welcome_controller(Req, State, fun term_to_json/1).

welcome_controller(Req, State, Renderer) ->
	{User, Req2} = cowboy_req:binding(user_id, Req),
	Result       = case db_srv:lookup_user_data(User) of
					{ok, Data}     ->
									{ok, Data};
					{error, Error} -> 
									{error, Error};
					_              ->
									{error, unknown_error}
					end,
	Body         = Renderer(Result),

	{Body, Req2, State}.

term_to_json(Data) ->
	json:term_to_json(Data).

term_to_html(Data) ->
%%	<<"<html><head><meta charset=\"utf-8\"><title>REST TUI</title></head><body><p>", Data/binary, "</p></body></html>">>.
	{ok, Body} = info_dtl:render([{info, Data}]),
	Body.

term_to_text(Data) ->
	list_to_binary(io_lib:format("~p",[Data])).

















