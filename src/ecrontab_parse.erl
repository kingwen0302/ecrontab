%%%----------------------------------------------------------------------
%%%
%%% wg @copyright 2009
%%%
%%% @author litao cheng <litaocheng@gmail.com>
%%% @doc the crontab parse module (simlar with *unix crontab, please read
%%%      crontab.
%%%
%%%----------------------------------------------------------------------
-module(ecrontab_parse).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("ecrontab.hrl").

-export([parse/1, parse_field/3]).

%% @doc parse the crontab config file
-spec parse(File :: string()) ->
    {ok, [cron_entry()]} | {'error', any()}.
parse(File) ->
    case file:consult(File) of
        {error, enoent} = Error ->
            ?Warn2("crontab file ~p not exist~n", [File]),
            Error;
        {error, R} = Error ->
            ?Warn2("crontab file error: ~p~n", [file:format_error(R)]),
            Error;
        {ok, CronTab} ->
            parse_entrys(CronTab)
    end.

%%-----------------------------------------------------------------------
%%
%% internal API
%%
%%-----------------------------------------------------------------------

%% parse all the entrys
parse_entrys(CronTab) ->
    Entrys =
    lists:foldl(
        fun(Entry, Acc) ->
                case catch parse_entry(Entry) of
                    {ok, CronEntry} ->
                        [CronEntry | Acc];
                    {error, R} ->
                        ?Warn2("the line :~p error:~p~n", [Entry, R]),
                        Acc
                end
        end,
        [],
        CronTab),
    {ok, Entrys}.


%% parse the single entry
parse_entry({{M, H, Dom, Mon, Dow}, {Mod, F, A} = MFA} = SrcCron) when is_atom(Mod), is_atom(F), is_list(A) ->
    Cron =
    #cron_entry{
        src_crontab = SrcCron,
        m = parse_field(M, 0, 59, emin),
        h = parse_field(H, 0, 23, ehour),
        dom = parse_field(Dom, 1, 31, edom),
        mon = parse_field(Mon, 1, 12, emon),
        dow = parse_field(Dow, 0, 7, edow),
        mfa = MFA
    },
    {ok, Cron}; 
parse_entry(_) ->
    {error, eformat}.


%% parset the fileld
parse_field(F, Min, Max, Error) ->
    try parse_field(F, Min, Max)
    catch _:Reason ->
        throw({error,{Error,Reason}})
    end.

parse_field(Field, Min, Max) ->
    %% 将整数转换成字符串统一处理
    FieldList = to_list(Field),
    FieldList1 = re:replace(FieldList, " ", "", [{return, list}, global]),
    FieldList2 = re:split(FieldList1, ",", [{return, list}]),
    do_parse_field(FieldList2, Min, Max, []).

do_parse_field([], Min, Max, AccList) ->
    Fun = fun(Id) -> Min =< Id andalso Id =< Max end,
    NewAccList = lists:usort(lists:filter(Fun, AccList)),
    NewAccList;
do_parse_field(["*"|FieldList], Min, Max, AccList) -> 
    do_parse_field(FieldList, Min, Max, lists:seq(Min, Max) ++ AccList);
do_parse_field([Field|FieldList], Min, Max, AccList) ->
    [First, Last, Inc] =
    case re:split(Field, "[-/]", [{return, list}]) of
        [First1] -> [First1, First1, 1];
        [First1, Last1] -> [First1, Last1, 1];
        [First1, Last1, Inc1] -> [First1, Last1, Inc1]
    end,
    AddList = lists:seq(to_integer(First), to_integer(Last), to_integer(Inc)),
    do_parse_field(FieldList, Min, Max, AddList ++ AccList).

to_integer(Int) when is_integer(Int) -> Int;
to_integer(List) when is_list(List) ->
    list_to_integer(List).

to_list(Int) when is_integer(Int) -> integer_to_list(Int);
to_list(List) when is_list(List) -> List.
