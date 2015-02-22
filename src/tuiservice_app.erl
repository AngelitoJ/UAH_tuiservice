%% TUI Service project
%% (C) 2015 Angel J. Alvarez Miguel


-module(tuiservice_app).
-author("angeljalvarezmiguel@gmail.com").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([option_specs/0, check_tcp_port/1]).

%% ------------------------------------------------------------------
%% Extended GetOpt Definitions
%% ------------------------------------------------------------------

option_specs() ->
    {    
        %%List of getopt descriptors {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
        [
            {port,     $P,        "Port",     {integer, 8080},                "Default Listening TCP port"}
        ]
        %% args processing funs required for some options
        ,[
            fun check_tcp_port/1       % Application Timeout
        ]
    }.

%% Check TCP port option

-spec check_tcp_port([any()]) -> [any()].
check_tcp_port(Opts) ->
    Port = proplists:get_value(port, Opts),                       %% Get the listen port requested. 
    case (Port == undefined) or (Port < 1025) or (Port > 65535) of %% Is requested port between bounds?
        true  ->
                [{port,8080} | proplists:delete(port,Opts)];      %% No, correct it and replace the wrong bits. 
        false ->
                Opts                                                  %% Yes, return the original list.
    end.


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, StartArgs) ->

    ok               = application:start(crypto),
    ok               = application:start(ranch),
    ok               = application:start(cowlib),
    ok               = application:start(cowboy),
   {ok, CmdlineOpts} = application:get_env(cmdline_options),    %% recover Opts from the enviroment
   AppOptions        = [{start_args,StartArgs}|CmdlineOpts],           %% Combine start args with cmdline options


    Routes =  [
                 {"/"                            ,top_handler     ,[]}
                ,{"/welcome/:user_id/"           ,welcome_handler ,[]}
                ,{"/login/:login_id/:password/"  ,login_handler   ,[]}
                ],

    Dispatch     = cowboy_router:compile([ {'_',Routes} ]),
    ListenPort   = proplists:get_value(port, AppOptions),
    {ok, _}      = cowboy:start_http(
                                         http
                                        ,100
                                        ,[{port, ListenPort}]
                                        ,[{env, [{dispatch, Dispatch}]}]
                                        ),
  
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

