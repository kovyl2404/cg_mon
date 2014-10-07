-module(cg_mon_app).
-author("Viacheslav V. Kovalev").

-behaviour(application).
-behaviour(supervisor).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Application callbacks
-export([start/2,
    stop/1]).

%% Supervisor callbacks
-export([init/1]).


%% Api functions
-export([
    update_interval/0,
    update_interval/1,
    metrics_table/0
]).




%%%===================================================================
%%% Api functions
%%%===================================================================

update_interval() ->
    {ok, Value} = application:get_env(cg_mon, update_interval),
    Value.

update_interval(Interval) when is_integer(Interval), Interval > 0 ->
    application:set_env(cg_mon, update_interval, Interval).

metrics_table() ->
    cg_mon_metrics.

%%%===================================================================
%%% Application callbacks
%%%===================================================================


start(_StartType, _StartArgs) ->
    SupervisorArgs = [
        cgroup_root(), cgroup_discovery_file(), handler_modules()
    ],
    ets:new( metrics_table(), [public, named_table] ),
    SupervisorStartRes = supervisor:start_link({local, cg_mon_sup}, cg_mon_app, SupervisorArgs),
    case SupervisorStartRes of
        {ok, Pid} ->
            {ok, Pid};
        _ ->
            {error, cgroups_error}
    end.


stop(_State) ->
    ok.


init([CgroupRoot, CgroupDiscovery, AvailableHandlers]) ->
    SupFlags = {one_for_one, 5, 3600},
    case cg_mon_lib:discover_cgroups( CgroupRoot, CgroupDiscovery ) of
        {ok, CgroupsMap} ->
            HandlersMap = orddict:from_list([
                {HandlerModule:provided_resource(), HandlerModule}
                || HandlerModule <- AvailableHandlers
            ]),
            Children =
                lists:foldl(
                    fun({Resource, Path}, Acc) ->
                        case orddict:find(Resource, HandlersMap) of
                            {ok, HandlerModule} ->
                                [ child_spec(HandlerModule, Path) | Acc ];
                            _ ->
                                Acc
                        end
                    end, [], CgroupsMap
                ),
            {ok, {SupFlags, Children}};
        {error, _Reason} ->
            ignore
    end.





%%%===================================================================
%%% Internal functions
%%%===================================================================


child_spec(Module, FullCgroupPath) ->
    Id = {Module, reader},
    Args = [
        Module:provided_resource(),
        Module:key_value_sources(),
        Module:single_value_sources(),
        FullCgroupPath
    ],
    {Id,
        {cg_mon_reader, start_link, Args},
        permanent, 2000, worker, [cg_mon_reader]
    }.




cgroup_root() ->
    case application:get_env(cg_mon, cgroup_root) of
        {ok, Value} when is_list(Value) ->
            Value;
        _ ->
            cg_mon_lib:cgroup_root()
    end.

cgroup_discovery_file() ->
    case application:get_env(cg_mon, cgroup_discovery_file) of
        {ok, Value} when is_list(Value) ->
            Value;
        _ ->
            cg_mon_lib:discovery_file()
    end.

handler_modules() ->
    [cg_mem_sup, cg_cpu_sup].


%%%===================================================================
%%% Unit tests
%%%===================================================================

-ifdef(TEST).

-include_lib("cgmon/include/test_utils.hrl").

reader_spec_test_() ->
    Res = init([
        ?TEST_DATA_DIR(), ?TEST_FILE("cgroup.discovery"),
        [cg_mem_sup]
    ]),
    ExpectedArgs = [
        cg_mem_sup:provided_resource(),
        cg_mem_sup:key_value_sources(),
        cg_mem_sup:single_value_sources(),
        ?TEST_FILE("memory/foo") ++ "/"
    ],
    ExpectedChildren = [
        {{cg_mem_sup, reader}, {cg_mon_reader, start_link, ExpectedArgs},
            permanent, 2000, worker, [cg_mon_reader]}
    ],
    ?_assertMatch(
        {ok, {_, ExpectedChildren}},
        Res
    ).

no_discovery_test_() ->
    Res = init([?TEST_DATA_DIR(), ?TEST_FILE("nonexistent.file"), [cg_mem_sup]]),
    [
        ?_assertEqual(ignore, Res)
    ].

-endif.