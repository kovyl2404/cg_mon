
-module(cg_mon_lib).
-author("Viacheslav V. Kovalev").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.



%% API
-export([
    discovery_file/0,
    cgroup_root/0,
    discover_cgroups/0,
    discover_cgroups/1,
    discover_cgroups/2,
    read_cgroups_metrics/5
]).

-export_type([
    cgroup_map/0,
    metric_source/0
]).

-type cgroup_map()      :: [{Resource :: atom(), FullPath :: string()}].
-type metric_source()   :: StatFile :: string() | { Key :: atom(), ValueFile :: string()}.


-spec discovery_file() -> string().
discovery_file() ->
    "/proc/self/cgroup".

-spec cgroup_root() -> string().
cgroup_root() ->
    "/sys/fs/cgroup".

-spec discover_cgroups() ->
    {ok, cgroup_map()}
    | {error, Reason :: atom()}.
discover_cgroups() ->
    discover_cgroups(cgroup_root(), discovery_file()).

-spec discover_cgroups(Root :: string()) ->
    {ok, cgroup_map()}
    | {error, Reason :: atom()}.
discover_cgroups(Root) ->
    discover_cgroups(Root, discovery_file()).

-spec discover_cgroups(Root :: string(), FileName :: string()) ->
    {ok, cgroup_map()}
    | {error, Reason :: atom()}.
discover_cgroups(Root, FileName) ->
    with_file(
        fun(DiscoveryFile) ->
            parse_discovery_file(DiscoveryFile, Root)
        end,
        FileName
    ).


-spec parse_discovery_file(File :: file:io_device(), CgroupRoot :: string()) ->
    {ok, cgroup_map()}
    | {error, Reason :: atom()}.
parse_discovery_file(File, CgroupRoot) ->
    foldl_lines(
        fun(Line, Acc) ->
            [_, Resources, GroupName] = string:tokens(Line, ":"),
            ResourcesList = string:tokens(Resources, ","),
            RelativeName = fixup_group_name(GroupName),
            FullName = filename:join([CgroupRoot, Resources]) ++ RelativeName,
            lists:foldl(
                fun(ResourceName, TmpAcc) ->
                    [ {list_to_atom(ResourceName), FullName} | TmpAcc]
                end, Acc, ResourcesList
            )
        end, [], File
    ).


-spec with_file(fun( (File :: file:io_device()) -> any() ), Filename :: string()) ->
    any().
with_file(Fun, FileName) ->
    case file:open(FileName, [read]) of
        {ok, File} ->
            Result = Fun(File),
            file:close(File),
            Result;
        Error ->
            Error
    end.


-spec foldl_lines(fun((Line :: string(), Acc :: any()) -> any()), InitAcc :: any(), File :: file:io_device()) ->
    any().
foldl_lines(Fun, Acc, File) ->
    case file:read_line(File) of
        {ok, Line} ->
            NewAcc = Fun( string:strip(Line, right, 10), Acc ),
            foldl_lines(Fun, NewAcc, File);
        eof ->
            {ok, Acc};
        Error ->
            Error
    end.

-spec read_cgroups_metrics(
    OutputTable :: ets:tid(),
    Resource :: atom(),
    FullCgroupPath :: string(),
    KeyValueStore :: [ ],
    SingleValueStore :: [ ]
) ->
    [{ FailedSource :: metric_source(), ErrorReason :: atom() }].
read_cgroups_metrics(OutputTable, Resource, FullCgroupPath, KeyValueStore, SingleValueStore) ->
    KeyValueErrors =
        lists:foldl(
            fun(KeyValueFile, Acc) ->
                case read_stat_file(FullCgroupPath, KeyValueFile) of
                    {ok, KeyValueData} ->
                        [
                            ets:insert(OutputTable, {{Resource, list_to_atom(Key)}, Value})
                            || {Key, Value} <- KeyValueData
                        ],
                        Acc;
                    {error, Reason} ->
                        [{KeyValueFile, Reason} | Acc]
                end
            end, [], KeyValueStore
        ),
    lists:foldl(
        fun(Entry = {Key, SingleValueFile}, Acc) ->
            FullFileName = filename:join(FullCgroupPath, SingleValueFile),
            ReadResult =
                with_file(
                    fun(File) ->
                        {ok, Line} = file:read_line(File),
                        {ok, list_to_integer( strip_line_ending(Line) )}
                    end,
                    FullFileName
                ),
            case ReadResult of
                {ok, Value} ->
                    ets:insert(OutputTable, {{Resource, Key}, Value}),
                    Acc;
                {error, Reason} ->
                    [{Entry, Reason} | Acc]
            end
        end, KeyValueErrors, SingleValueStore
    ).


-spec read_stat_file(FullCgroupPath :: string(), StatFile :: string()) ->
    [{Key :: string(), Value :: integer()}].
read_stat_file(FullCgroupPath, StatFile) ->
    with_file(
        fun(File) ->
            foldl_lines(
                fun(Line, Acc) ->
                    [Key, Value] = string:tokens(Line, " "),
                    [{Key, list_to_integer(Value)} | Acc]
                end, [], File
            )
        end,
        filename:join(FullCgroupPath, StatFile)
    ).



-spec fixup_group_name(GroupName :: string()) ->
    string().
fixup_group_name(GroupName) ->
    case strip_line_ending(GroupName) of
        Value = "/" -> Value;
        Value -> Value ++ "/"
    end.

-spec strip_line_ending(Line :: string()) ->
    string().
strip_line_ending(Line) ->
    string:strip(Line, right, 10).

%% ----------------------------------------------------------------------------
%% EUnit test cases
%% ----------------------------------------------------------------------------

-ifdef(TEST).

-include_lib("cgmon/include/test_utils.hrl").

discovery_error_test() ->
    Res = discover_cgroups(cgroup_root(), ?TEST_FILE("non-existent-file")),
    ?assertEqual(Res, {error, enoent}).

discovery_denied_test() ->
    Res = discover_cgroups(cgroup_root(), "/root/some-not-permitted-file"),
    ?assertEqual(Res, {error, eacces}).

discovery_test_() ->
    {ok, Res} = discover_cgroups(cgroup_root(), ?TEST_FILE("cgroup.discovery")),
    [
        ?_assertEqual(4, length(Res)),
        ?_assertEqual("/sys/fs/cgroup/memory/foo/", proplists:get_value(memory, Res)),
        ?_assertEqual("/sys/fs/cgroup/cpu,cpuacct/bar/", proplists:get_value(cpu, Res)),
        ?_assertEqual("/sys/fs/cgroup/cpu,cpuacct/bar/", proplists:get_value(cpuacct, Res)),
        ?_assertEqual("/sys/fs/cgroup/cpuset/", proplists:get_value(cpuset, Res))
    ].


read_stat_file_error_test_() ->
    [
        ?_assertEqual(
            {error, enoent},
            read_stat_file(?TEST_FILE("some/not/existed/"), "file.stat")
        ),
        ?_assertEqual(
            {error, eacces},
            read_stat_file("/root/some/not/permitted", "file.stat")
        )
    ].

read_stat_file_test_() ->
    {ok, Result} = read_stat_file(?TEST_FILE("memory/foo/"), "memory.stat"),
    [
        ?_assertEqual(7, length(Result)),
        ?_assertEqual(1, proplists:get_value("cache", Result))
    ].




read_cgroups_metrics_test_() ->
    KeyValueSources = [
        "memory.stat",
        "unknown.stat"
    ],
    SingleValueSources = [
        {limit, "memory.limit_in_bytes"},
        {usage, "memory.usage_in_bytes"},
        {unknown_metric, "some.unknown_metric"}
    ],
    {setup,
        fun() -> ets:new(?MODULE, []) end,
        fun(Table) -> ets:delete(Table) end,
        fun(Table) ->
            ReadResult =
                read_cgroups_metrics(
                    Table, memory, ?TEST_FILE("memory/foo"),
                    KeyValueSources, SingleValueSources
                ),
            ExpectedErrors = [
                {{unknown_metric, "some.unknown_metric"}, enoent},
                {"unknown.stat", enoent}
            ],
            ExpectedMetrics =
                [{{memory,pgpgout},6},
                    {{memory,pgfault},7},
                    {{memory,rss},2},
                    {{memory,mapped_file},4},
                    {{memory,pgmajfault},8},
                    {{memory,pgpgin},5},
                    {{memory,usage},1},
                    {{memory,cache},1},
                    {{memory,limit},2}
                ],
            [
                ?_assertEqual( lists:sort(ExpectedErrors), lists:sort(ReadResult) ),
                ?_assertEqual(length(ExpectedMetrics), length(ets:tab2list(Table))),
                lists:map(
                    fun({Key, Value}) ->
                        ?_assertEqual( [{Key, Value}], ets:lookup(Table, Key) )
                    end, ExpectedMetrics
                )

            ]
        end
    }.




-endif.