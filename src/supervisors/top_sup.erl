%% TUI Service project
%% (C) 2015 Angel J. Alvarez Miguel

-module(top_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILDSUP(I, Args), {I, {I, start_link, [Args]}, permanent, infinity, supervisor, [I]}).
-define(CHILDWRK(I, Args), {I, {I, start_link, [Args]}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Opts) ->  %% Opts is a property list
    supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Opts) ->
    ChildrenSpec = [],
    io:format("[~p]: Init, I got ~p children to spawn..\n", [?MODULE,length(ChildrenSpec)]),

    {ok, { {one_for_one, 5, 10},
    							ChildrenSpec } }.

