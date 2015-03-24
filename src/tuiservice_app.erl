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
            {port,     undefined,        "Port",     {integer, 8080},                "Default Listening TCP port"}
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

    {ok, CmdlineOpts} = application:get_env(cmdline_options),    %% recover Opts from the enviroment
    AppOptions        = [{start_args,StartArgs}|CmdlineOpts],           %% Combine start args with cmdline options

    Result = sequence(
                        {ok, none, AppOptions},
                        [
                             fun start_cowboy/2
                            ,fun start_app/2
                        ]
                    ),
    
    case Result of
        {ok, State, _} -> {ok, State};
        {error, Reason, _State} -> {error, Reason}
    end.

  

stop(_State) ->
    io:format("Application stopped...\n\n"),
    ok.


%% ===================================================================
%% Utility Functions
%% ===================================================================


% Bind computations ala Haskell
bind(Fun, {ok, State, Env}) when is_function(Fun) -> Fun(State, Env);
bind(_, {error, Reason, State}) -> {error, Reason, State}.  


%% Sequence a list of computations
sequence(Initial, Computations) ->
    lists:foldl(fun bind/2, Initial, Computations).

%% Star app using options or signal an error
start_app(State, Options) ->
    case top_sup:start_link(Options) of
        {ok, Pid} ->
                    io:format("Application tuiservice started...\n\n"),
                    {ok, Pid, Options};
        {error,Other} ->
                    io:format("Application tuiservice crashed! Error: ~p\n",[Other]),
                    {error, Other, State}
    end.

%% start cowboy and all dependences. 
start_cowboy(State, Options) ->
    ok                = application:start(crypto),
    ok                = application:start(ranch),
    ok                = application:start(cowlib),
    ok                = application:start(cowboy),

    Routes            =  [
                             {"/"                            ,top_handler     ,[]}
                            ,{"/welcome/:user_id/"           ,welcome_handler ,[]}
                            ,{"/login/:login_id/:password/"  ,login_handler   ,[]}
                        ],
    Dispatch          = cowboy_router:compile([ {'_',Routes} ]),
    ListenPort        = proplists:get_value(port, Options),  

    io:format("Starting Cowboy..\n\n"),
    case cowboy:start_http(
                             http
                            ,100
                            ,[{port, ListenPort}]
                            ,[{env, [{dispatch, Dispatch}]}]
                            ) of
        {ok,_}          ->
                            {ok, State, Options};
        {error, Reason} ->
                            {error, Reason, State}
    end.





