-module(cg_mon_reader).
-author("Viacheslav V. Kovalev").


%% API
-export([
    start_link/4,
    read_meter/2,
    read_meter/3
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

%%%===================================================================
%%% Api functions
%%%===================================================================

start_link(ProvidedResource, KeyValueSources, SingleValueSources, FullCgroupPath) ->
    gen_server:start_link(
        ?MODULE, {ProvidedResource, KeyValueSources, SingleValueSources, FullCgroupPath}, []
    ).

read_meter(Resource, Meter, DefaultValue) ->
    case ets:lookup(cg_mon_app:metrics_table(), {Resource, Meter}) of
        [] ->
            DefaultValue;
        [{_, Value}] ->
            Value
    end.

read_meter(Resource, Meter) ->
    read_meter(Resource, Meter, undefined).


-record(
    state, {
        provided_resource,
        key_value_sources,
        single_value_sources,
        full_cgroup_path
    }
).

init({ProvidedResource, KeyValueSources, SingleValueSources, FullCgroupPath}) ->
    error_logger:info_msg(
        "Starting cg_mon_reader for resource ~p to read from ~p",
        [ProvidedResource, FullCgroupPath]
    ),
    erlang:send( self(), update ),
    {ok, #state{
        full_cgroup_path = FullCgroupPath,
        provided_resource = ProvidedResource,
        key_value_sources = KeyValueSources,
        single_value_sources = SingleValueSources
    }}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.


handle_info(
    update,
    #state{
        full_cgroup_path = FullCgroupPath,
        provided_resource = ProvidedResource,
        key_value_sources = KeyValueSources,
        single_value_sources = SingleValueSources
    } = State
) ->
    cg_mon_lib:read_cgroups_metrics(
        cg_mon_app:metrics_table(), ProvidedResource, FullCgroupPath,
        KeyValueSources, SingleValueSources
    ),
    erlang:send_after( cg_mon_app:update_interval(), self(), update ),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

