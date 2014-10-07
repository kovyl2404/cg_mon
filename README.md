cg_mon
======

Simple application to extend osmon functionality when using cgroups.
Now it only supports reading memory and cpuacct cgroup metrics and can't automatically detect where cgroups mounted.

Usage example.
--------------
```
1> ok = application:load(cg_mon).
ok
2> ok = application:set_env(cg_mon, update_interval, 2000). %% Update metrics each 2 seconds. 1 second by default.
ok
3> ok = application:set_env(cg_mon, cgroup_root, "/directory/where/cgroups/mounted"). %% /sys/fs/cgroup by default
ok
4> ok = application:start(cg_mon).
ok
5> cg_mem_sup:rss().
1941913600
6> cg_mem_sup:usage().
4880506880
7> cg_cpu_sup:usage().
8526844361646
```


To be done:
1. Add support for another cgroups such as blkio.
2. Add event notifications (for example, about excessing some limit of memory usage).
3. Add documentation.
