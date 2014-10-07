
-module(cgmon_SUITE).
-author("Viacheslav V. Kovalev").

-include_lib("common_test/include/ct.hrl").

%% API
-export([
    all/0,
    groups/0,
    init_per_group/2,
    end_per_group/2
]).



%% Test cases
-export([
    successful_start_stop/1,
    no_discovery_file/1
]).


all() ->
    [
        {group, start_stop_tests}
    ].

groups() ->
    [
        {start_stop_tests, [sequence], [
            successful_start_stop,
            no_discovery_file
        ]},
        {cg_mem_sup_tests, [sequence], [
            update_memory_metrics
        ]}
    ].


init_per_group(start_stop_tests, Config) ->
    CgroupRoot = ?config(data_dir, Config),
    ok = application:load(cg_mon),
    ok = application:set_env( cg_mon, update_interval, 200 ),
    [ {cgroup_root, CgroupRoot} | Config ].



end_per_group(start_stop_tests, Config) ->
    application:stop(cg_mon),
    ok = application:unload(cg_mon),
    Config.



successful_start_stop(Config) ->
    CgroupRoot = ?config(cgroup_root, Config),
    CgroupDiscoveryFile = filename:join(CgroupRoot, "cgroup"),
    ok = application:set_env(cg_mon, cgroup_root, CgroupRoot),
    ok = application:set_env(cg_mon, cgroup_discovery_file, CgroupDiscoveryFile),
    ok = application:start(cg_mon),
    timer:sleep(100),
    11 = cg_mem_sup:cache(),
    12 = cg_mem_sup:rss(),
    13 = cg_mem_sup:rss_huge(),
    ok = application:stop(cg_mon).

no_discovery_file(Config) ->
    CgroupRoot = ?config(cgroup_root, Config),
    CgroupDiscoveryFile = filename:join(CgroupRoot, "non_existent_file"),
    ok = application:set_env(cg_mon, cgroup_root, CgroupRoot),
    ok = application:set_env(cg_mon, cgroup_discovery_file, CgroupDiscoveryFile),
    {error, {cgroups_error, _}} = application:start(cg_mon).



