
-module(cg_mem_sup).
-author("Viacheslav V. Kovalev").

%% API
-export([
    provided_resource/0,
    key_value_sources/0,
    single_value_sources/0
]).

-export([
    limit/0,
    swlimit/0,
    usage/0,
    swusage/0,
    cache/0,
    rss/0,
    rss_huge/0,
    mapped_file/0,
    pgpgin/0,
    pgpgout/0,
    swap/0,
    writeback/0,
    inactive_anon/0,
    active_anon/0,
    inactive_file/0,
    active_file/0
]).

-define(PROVIDED_RESOURCE, memory).
-define(KEY_VALUE_SOURCES, ["memory.stat"]).
-define(SINGLE_VALUE_SOURCES, [
    {limit, "memory.limit_in_bytes"},
    {swlimit, "memory.memsw.limit_in_bytes"},
    {usage, "memory.usage_in_bytes"},
    {swusage, "memory.memsw.usage_in_bytes"}
]).

provided_resource() ->
    ?PROVIDED_RESOURCE.

key_value_sources() ->
    ?KEY_VALUE_SOURCES.

single_value_sources() ->
    ?SINGLE_VALUE_SOURCES.

usage() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, usage).

swusage() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, swusage).

limit() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, limit).

swlimit() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, swlimit).

cache() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, cache).

rss() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, rss).

rss_huge() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, rss_huge).

mapped_file() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, mapped_file).

pgpgin() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, pgpgin).

pgpgout() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, pgpgout).

swap() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, swap).

writeback() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, writeback).

inactive_anon() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, inactive_anon).

active_anon() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, active_anon).

inactive_file() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, inactive_file).

active_file() ->
    cg_mon_reader:read_meter(?PROVIDED_RESOURCE, active_file).
