%%%-------------------------------------------------------------------
%%% @author dean <dean@dean2>
%%% @copyright (C) 2013, dean wagner
%%% @doc
%%%
%%% @end
%%% Created : 11 Mar 2013 by dean <deanwagner13@gmail.com>
%%%-------------------------------------------------------------------
-module(calc).

-behaviour(gen_fsm).

%% API
-export([start_link/0,
	 reload_data/1,
	 reload_msg/0,
	 compute/0,
	 compute_all/0,
	 get_state/0,
	 set_state/1,
	 next_step/0]).

%% gen_fsm callbacks
-export([init/1, idle/2, idle/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {name="",
	        master,
	        step,
	        vertices,
	        msgs = [],
	        msgs1 = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Loads data in to the ETS and 
%%
%% @spec relaod_data(File_Name) -> 
%% @end
%%--------------------------------------------------------------------
reload_data(Dets_Table) ->
    gen_fsm:send_event(?MODULE, {reload_data, Dets_Table}).

reload_msg() ->
    gen_fsm:send_event(?MODULE, {reload_msg}).

compute() ->
    gen_fsm:send_event(?MODULE, {compute}).

compute_all() ->
    gen_fsm:send_event(?MODULE, {compute_all}).

get_state() ->
    gen_fsm:send_event(?MODULE, {get_state}).

set_state(State) ->
    gen_fsm:send_event(?MODULE, {set_state, State}).

next_step() ->
    gen_fsm:send_event(?MODULE, {next_step}).


%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, idle, #state{name="my_worker"}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

idle({reload_data,File_name}, State) ->
    io:format("reload data~n"),
    Table1 = ets:new(data_table, []),
    {ok, S} = file:open(File_name,read),
    load_data_fm_file(S, Table1),
    State1 = State#state{vertices=Table1},
    {next_state, idle, State1};

%idle({reload_msg,Dets_Table}, State) ->
%  add a list of msgs 
idle({reload_msg}, State) ->
    io:format("reload msg~n"),
%    State1 = State#state{msgs=Msgs},
    State1 = State#state{msgs=[{"A",b,c,d,e,[]}]},
    {next_state, idle, State1};

idle({compute}, State) ->
    io:format("compute~n"),
    Msg_list = State#state.msgs,
    New_Msgs = process_msgs(Msg_list,State),
    State1 = State#state{msgs1=New_Msgs},
    {next_state, idle, State1};

idle({compute_all}, State) ->
    io:format("compute all~n"),
    {next_state, idle, State};

idle({get_state}, State) ->
    io:format("state = ~p~n",[State]),
    {next_state, idle, State};

idle({set_state, State1}, _State) ->
    io:format("set state~n"),
    {next_state, idle, State1};

idle({next_step}, State) ->
    io:format("next_step~n"),
    Next_Msgs = State#state.msgs1,
    State1 = State#state{msgs=Next_Msgs, msgs1=[]},
    {next_state, idle, State1};

idle(_Event, State) ->
    io:format("some other event~n"),
    {next_state, idle, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
idle(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_data_fm_file(S, Table1) ->
    Line = io:read(S,''),
    ldff( Line, S, Table1).

ldff(eof, _S, _Table1) ->
    ok;
ldff({ok,Line},S,Table1) -> 
    io:format("~w~n", [Line]),
    ets:insert(Table1, Line),
    Line1 = io:read(S,''),
    ldff( Line1, S, Table1).

process_msgs(Msg_list, State) ->
    io:format("yo, in process message~n"),
    process_msgs(Msg_list, [], State).

% Msg_list is all messages for the step
process_msgs([], Msg_list1, _State) ->
    Msg_list1;
process_msgs(Msg_list, Msg_list1, State) ->
    [ Msg_head | Msg_tail] = Msg_list,
    Msg_accm = calc_and_new_msg(Msg_head, State),
    Msg_list1_new = Msg_accm ++ Msg_list1,
    process_msgs(Msg_tail,Msg_list1_new, State).

% creates messages+1 for a single message
calc_and_new_msg(Msg_head, State) ->
    {ToID, _FM, _QuerID, _Step, _Finct, Args} = Msg_head,
    Tab = State#state.vertices,
    [Vertex] = ets:lookup(Tab, ToID),  % is it right to assume one vertex?
    {_,_,Edges} = Vertex,
    make_messages(Edges, ToID, Args,[]).

make_messages([], _ToID, _Args, Messages) ->
    Messages;
make_messages(Edges, ToID, Args, Messages) ->
    [{Next_edge_id,_} | Edges_tail] = Edges,
    Args1 = [ToID | Args],
    Messages1 = [{Next_edge_id, ToID, c, d, e, Args1} | Messages],
    make_messages(Edges_tail, ToID, Args, Messages1).
    
