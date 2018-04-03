%%%----------------------------------------------------------------------
%%%
%%% wg @copyright 2009
%%%
%%% @author litao cheng <litaocheng@gmail.com>
%%% @doc the cron server, run the periodic task
%%%
%%%----------------------------------------------------------------------
-module(ecrontab_server).
-author('litaocheng@gmail.com').
-vsn('0.1').
-behaviour(gen_server).
-include("ecrontab.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {
        file = "" :: string(),          % file name
        mtime = 0 :: pos_integer(),     % last modify time
        entrys = [] :: [cron_entry()],  % the cron tasks
        cron_timer :: reference()       % the check cron task timer
    }).

-define(SERVER, ?MODULE).
        
%% @doc start the cron server
-spec start_link() ->
    {'ok', pid()} | 'ignore' | {'error', any()}.
start_link() ->
    ?Debug2("~p start_link~n", [?SERVER]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server callbacks
init(_Args) ->
    process_flag(trap_exit, true),
    {ok,CronFile} = application:get_env(cronfile),
    case ecrontab_parse:parse(CronFile) of
        {ok, Entrys} ->
            ?Debug2("parse the crontab success~n", []),
            State = #state{
                file = CronFile,
                mtime = filelib:last_modified(CronFile),
                entrys = Entrys,
                cron_timer = check_cron_timer()
            },
            {ok, State};
        Error ->
            ?Error2("error :~p~n", [Error]),
            Error
    end.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_cron, State) ->
    #state{entrys = Entrys} = State2 = load_crontab(State),
    State3 = State2#state{
        cron_timer = check_cron_timer()
    },
    check_entrys(Entrys),
    {noreply, State3};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
    
%%-----------------------------------------------------------------------------
%%
%% internal API
%%
%%-----------------------------------------------------------------------------
load_crontab(State = #state{file = File, mtime = MTime}) ->
    MTimeNew = filelib:last_modified(File),
    case  MTimeNew > MTime of
        true -> % reload crontab
            case ecrontab_parse:parse(File) of
                {ok, Entrys} ->
                    ?Debug2("the crontab file ~s load success~n", [File]),
                    State#state{
                        file = File,
                        mtime = MTimeNew,
                        entrys = Entrys
                    };
                _Error ->
                    ?Warn2("the crontab file ~s format error:~p~n", [File, _Error]),
                    State
            end;
        false ->
            State
    end.

%% start the cron tasks timer
check_cron_timer() ->
    {_Date, {_H, _M, S}} = erlang:localtime(), 
    erlang:send_after((60 - S + 1) * 1000, self(), check_cron).

%% check the cron entrys
check_entrys(Entrys) ->
    Now = {Date, _Time} = erlang:localtime(), 
    Week = calendar:day_of_the_week(Date),
    lists:foreach(
        fun(Entry) ->
                case can_run(Entry, Now, Week) of
                    true ->
                        run_task(Entry#cron_entry.mfa);
                    false ->
                        ok
                end
        end,
        Entrys).

can_run(Entry, {{_, CurMon, CurDay}, {CurH, CurM, _}}, Week) ->
    #cron_entry{
        m = M,
        h = H,
        dom = Dom,
        mon = Mon,
        dow = Dow
    } = Entry,
    field_ok(M, CurM) andalso
    field_ok(H, CurH) andalso
    (field_ok(Dom, CurDay) orelse field_ok(Dow, Week)) andalso
    field_ok(Mon, CurMon). 

%% check if the field is ok
field_ok(FieldList, Cur) ->
    lists:member(Cur, FieldList).


%% run the task
run_task({M, F, A} = Task) ->
    proc_lib:spawn(
        fun() ->
            case catch apply(M, F, A) of
                {'EXIT', R} ->
                    ?Error2("cron task ~p error: ~p~n", [Task, R]),
                    ok;
                _ ->
                    ok
            end
        end
    ).
