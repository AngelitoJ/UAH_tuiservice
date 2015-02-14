%% TUI Service project
%% (C) 2015 Angel J. Alvarez Miguel


-module(tuiservice_app).
-author("angeljalvarezmiguel@gmail.com").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, StartArgs) ->

    ok               = application:start(crypto),
    ok               = application:start(ranch),
    ok               = application:start(cowlib),
    ok               = application:start(cowboy),
   {ok, CmdlineOpts} = application:get_env(cmdline_options),    %% recover Opts from the enviroment
   AppOptions        = [{start_args,StartArgs}|CmdlineOpts],           %% Combine start aargs with cmdline options


    Routes =  [
                 {"/"                            ,tophandler     ,[]}
                ,{"/welcome/:user_id/"           ,welcomehandler ,[]}
                ,{"/login/:login_id/:password/"  ,loginhandler   ,[]}
                ],

    Dispatch = cowboy_router:compile([ {'_',Routes} ]),
    {ok, _}  = cowboy:start_http(http, 100, [{port, 8080}], [ {env, [{dispatch, Dispatch}]} ]),
  
    case top_sup:start_link(AppOptions) of
        {ok, Pid} ->
                    io:format("Application started...\n\n"),
                    {ok, Pid};
        {error,Other} ->
                    io:format("Application crashed! Error: ~p\n",[Other]),
                    {error, Other}
    end.

stop(_State) ->
    io:format("Application stopped...\n\n"),
    ok.

