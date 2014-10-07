-ifdef(TEST).

-ifndef(TEST_UTILS_HRL).
-define(TEST_UTILS_HRL, ok).

-include_lib("eunit/include/eunit.hrl").


-define(
    TEST_DATA_DIR(),
    filename:join( code:lib_dir(cgmon), "eunit_test_data" )
).

-define(
    TEST_FILE(FileName),
    filename:join(?TEST_DATA_DIR(), FileName)
).

-endif.
-endif.
