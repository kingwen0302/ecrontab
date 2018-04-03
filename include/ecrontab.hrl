%%%----------------------------------------------------------------------
%%%
%%% wg @copyright 2009
%%%
%%% @author litao cheng <litaocheng@gmail.com>
%%% @doc the crontab record defines
%%%
%%%----------------------------------------------------------------------

-type cronf_num() :: non_neg_integer().
-type cronf_list() :: [cronf_num()].

-record(cron_entry, {
        src_crontab :: tuple(),
        m :: cronf_list(),       % minute
        h :: cronf_list(),       % hour
        dom :: cronf_list(),     % day of month
        mon :: cronf_list(),     % month
        dow :: cronf_list(),     % day of week
        mfa :: tuple()           % the mfa
   }).
-type cron_entry() :: #cron_entry{}.


%% some log defines
-define(Debug2(F, D), (io:format(F, D))).
-define(Warn2(F, D), (io:format(F, D))).
-define(Error2(F, D), (io:format(F, D))).
