
-module(cg_cpu_sup).
-author("Viacheslav V. Kovalev").

%% API
-export([
    provided_resource/0,
    key_value_sources/0,
    single_value_sources/0
]).

-export([
    usage/0
]).


-define(PROVIDED_RESOURCE, cpuacct).
-define(KEY_VALUE_SOURCES, []).
-define(SINGLE_VALUE_SOURCES, [
    {usage, "cpuacct.usage"}
]).

provided_resource() ->
    ?PROVIDED_RESOURCE.

key_value_sources() ->
    ?KEY_VALUE_SOURCES.

single_value_sources() ->
    ?SINGLE_VALUE_SOURCES.

usage() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, usage).
