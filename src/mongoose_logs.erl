-module(mongoose_logs).

-export([set_global_loglevel/1]).
-export([get_handlers_loglevel/0]).
-export([set_custom/2]).
-export([get_log_files/0]).
-export([dir/0]).

-spec get_handlers_loglevel() -> [{logger:handler_id(), logger:level()}].
get_handlers_loglevel() ->
    [ {ID, Level} || #{id := ID, level := Level} <- logger:get_handler_config() ].


-spec set_global_loglevel(logger:loglevel() | pos_integer()) -> [Result] when
    Result :: {LagerBackend, ok | {error, Reason}},
    %% Yes, these are two different errors!
    Reason :: bad_log_level | bad_loglevel,
    LagerBackend :: lager_console_backend | {lager_file_backend, Path},
    Path :: string().
set_global_loglevel(Level) when is_integer(Level) ->
    set_global_loglevel(log_level(Level));
set_global_loglevel(Level) ->
    [ {ID, logger:set_handler_config(ID, Level)}
      || #{id := ID, level := _OldLevel} <- logger:get_handler_config() ].

-spec set_custom(module(), logger:loglevel() | pos_integer()) -> ok | {error, term()}.
set_custom(Module, Level) when is_integer(Level) ->
    set_custom(Module, log_level(Level));
set_custom(Module, Level) ->
    logger:set_module_level(Module, Level).

-spec get_log_files() -> [filename:name()].
get_log_files() ->
    [ File || #{config := #{file := File}} <- logger:get_handler_config() ].

-spec dir() -> string().
dir() ->
    case logger:get_handler_config(disk_log) of
        {ok, #{config := #{file := Path}}} ->
            filename:dirname(Path);
        _ ->
            ""
    end.

log_level(0) -> none;
log_level(1) -> critical;
log_level(2) -> error;
log_level(3) -> warning;
log_level(4) -> info;
log_level(5) -> debug.
