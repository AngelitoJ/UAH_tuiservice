%% TUI Service project
%% (C) 2015 Angel J. Alvarez Miguel

-module(db_srv).
-author("angeljalvarezmiguel@gmail.com").
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, option_specs/0, check_db_file/1, lookup_user_data/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% ------------------------------------------------------------------
%% Extended GetOpt Definitions
%% ------------------------------------------------------------------


option_specs() ->
    {    
        [
             {dbformat,$f ,"format", string, "DB format <terms|csv>"} %% Use erlang term or a CSV as file format 
            ,{dbfile  ,$D ,"dbfile", string, "DB Data file."}           %% options spec to request especified DB file
        ]
        ,[
             fun check_db_format/1                                %% check data format
            ,fun check_db_file/1                                  %% fun to check up file.
            ]
    }.

%% Check supplied db file is a valid file.
-spec check_db_file([any()]) -> [any()].
check_db_file(Opts) ->
    File = proplists:get_value(dbfile, Opts, "./users.db"),      
    [{db, File}|Opts].

%% Check supplied dbformat is a valid string among undefined, csv and terms
-spec check_db_format([any()]) -> [any()].
check_db_format(Opts) ->
	case proplists:get_value(dbformat, Opts, undefined) of
		undefined ->
					[{dbformat, terms}|Opts];
		"csv"       ->
					[{dbformat, csv}|Opts];
		"terms"     ->
					[{dbformat, terms}|Opts];
		Other     ->
					{error, {invalid_db_format, Other}}
	end. 


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-record(st, {
				 path   = none      %% Path to DB file
				,format = undefined %% File format i.e. "csv" "terms" 
				,reload = true      %% Load table on first incarnation (false when table heir holds the table after a crash) 
				,table  = none      %% ETS table identifier
			}).

-record(user_info, {
				  id       = none
				 ,login    = none
				 ,roles    = []
				 ,fullname = none
				 ,photoURL = none
			}).


start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).



lookup_user_data(User) ->
	gen_server:call(?SERVER, {info, User}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Opts) ->
    io:format("[~p] Starting up...\n",[?MODULE]),
    Result = sequence(
                         {ok, #st{}, Opts}                 %% Initial empty state and opts from supervisor
                        ,[                        
                             fun init_filepath/2            %% Setup paths to DB file
                            ,fun init_ETS_table/2           %% Setup ETS table
                            ,fun init_user_database/2       %% Load users DB from file
                        ]),

    case Result of
        {ok, State, _} -> {ok, State};
        {error, Reason, _State} -> {stop, Reason}
    end.

%% Serve a user info request
handle_call({info, User}, _From, #st{ table = Table } = State) ->
	io:format("[~p] Looking for ~p \n", [?MODULE, User]),
	Result = case ets:lookup(Table, User) of
		[Object|_]    ->
						{ok, Object};
		[]            ->
						{error, user_not_found};
		_             ->
						{error, ets_lookup_error}
	end,
	{reply, Result, State};

handle_call(_Request, _From, State) ->
    {stop, unknown_call, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

% Bind computations ala Haskell
bind(Fun, {ok, State, Env}) when is_function(Fun) -> Fun(State, Env);
bind(_, {error, _, _} = Other) -> Other.  


%% Sequence a list of computations
sequence(Initial, Computations) ->
    lists:foldl(fun bind/2, Initial, Computations).

%% Setup path to dbfile
init_filepath(State, Opts) ->
    case proplists:is_defined(db, Opts) of
        true -> 
                Format = proplists:get_value(dbformat,Opts),
                Path   = proplists:get_value(db,Opts),
                	{ok, State#st{ path = Path, format = Format }, Opts};
        false ->
                {error, not_dbfile, State}
    end.


%% Create a new ETS table for user data (or recover it from the table heir) 
init_ETS_table(State, Opts) ->
    %% Ask Table heir if table already exists, create a new one if needed and set the heir
    case gen_server:call(ets_master_srv, {get, users}) of
    	{ok, Value}       ->
                			io:format("[~p]: Getting ETS table named ~p from ETS master.\n", [?MODULE, users]),
                    		{ok, State#st{ table = Value, reload = false }, Opts};
        {not_found, Pid}  ->
                			io:format("[~p]: Creating ETS table named ~p..\n", [?MODULE, users]),
                        	Value = ets:new(users, [                            
                                            public
                                            ,{keypos, #user_info.id}
                                            ,{heir, Pid, {name, users}}
                                            ]),
                        	{ok, State#st{ table = Value, reload = true }, Opts}
    end.

%% Load user database if needed and try known formats
init_user_database(#st{ reload = true, format = Format } = State, Opts) ->
	case Format of
		csv   ->
				{error, cvs_db_format_unimplemented, State};
		terms ->
				init_user_database_from_terms(State,Opts)
	end;

init_user_database(#st{ reload = false } = State, Opts) ->
	io:format("[~p]: ETS table is already populated..\n", [?MODULE]),
	{ok, State, Opts}.


%% Load user database from terms in a file using consult 
init_user_database_from_terms(#st{ path = Path, table = Table} = State, Opts) ->
	io:format("[~p]: ETS table is empty loading database records from erlang terms.\n", [?MODULE]),
	case file:consult(Path) of
		{ok, [{database, Records}|_]} ->
										Entries = lists:map(fun build_record/1, Records),
										ets:insert(Table, Entries),
										{ok, State, Opts};
		{error, Error }               ->
										{error, Error, State};
		_                             -> 
										{error, unknown_database_format, State}
	end.


%% Build a user record incrementally folding over provided fields
build_record({user, Fields}) ->
	lists:foldl(fun build_record/2, #user_info{}, Fields).

build_record(Field, Record) ->
	case Field of 
		{id, Id }          ->
								Record#user_info{ id = list_to_binary(Id) };
		{login, Login }    ->
								Record#user_info{ login = Login };
		{roles, Roles }    ->
								Record#user_info{ roles = Roles };
		{fullname, FName } ->
								Record#user_info{ fullname = FName };
		{photo, Url }      ->
								Record#user_info{ photoURL = Url }
	end.








