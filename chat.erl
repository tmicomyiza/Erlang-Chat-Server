%%%-------------------------------------------------------------------
%%% @author Mark A. Sheldon <msheldon@cs.tufts.edu>
%%% @copyright (C) 2020, Mark A. Sheldon
%%% @doc
%%%
%%% @end
%%% Created : 23 Mar 2020 by Mark A. Sheldon <msheldon@cs.tufts.edu>
%%% 
%%% Editor : Mico Theogene Micomyiza
%%% Edited : 15 Feb 2022
%%% 
%%%-------------------------------------------------------------------
-module(chat).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).


-define(SERVER, ?MODULE).

% -record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
stop(Server) -> gen_server:stop(Server).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(_Name) -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link(RoomName) ->
    gen_server:start_link({local, RoomName}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init([]) ->
    process_flag(trap_exit, true),
    {ok, []}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(), hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.

handle_call({send_message, {RoomName, Message}}, _From, State)->
    % erlang:display(Message),
    lists:foreach(fun({_, Pid})  ->
				  Pid ! {node(), RoomName,
					 message, Message}
			  end,
			  State),
    {reply, ok, State};

handle_call({get_members,{RoomName, {_Name, Pid}}}, _From, State) ->
    Names = lists:map(fun({Name, _Pids}) -> Name end, State),
    io:format("getting members ~w~n", [Names]),
    Pid ! {node(), RoomName, message, Names},
    {reply, ok, State};

handle_call(Request, _From, State) ->
    io:format("Unexpected message: ~w~n", [Request]),
    {reply, error, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.

handle_cast({subscribe, Sub = {Name, _Pid}}, State) ->
    NewState = [Sub | State],
    io:format("~s subscribed to ~w~n", [Name, ?SERVER]),
    {noreply, NewState};

handle_cast({unsubscribe, Sub = {Name, _}}, State) ->
    NewState = lists:delete(Sub, State),
    io:format("~s unsubscribed from ~w~n", [Name, ?SERVER]),
    {noreply, NewState};

handle_cast(Request, State) ->
    io:format("Unexpected message:  ~w~n",
		     [Request]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.





















%%%%
%%--------------------------------------------------------------------
%%
%%               Other Default functions/ I didn't change them
%%
%%
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================