  $ mkdir _esy
  $ mkdir _esy/something
  $ dune runtest
  $ find $(pwd) -name run.t.corrected
  $ DUNE_BUILD_DIR=$(pwd)/_esy/something dune runtest 2>&1 | sed 's|[0-9a-f]\{32\}/default/sub-test|$SANDBOX_HASH/default/sub-test|'
  Error:
  $TESTCASE_ROOT/_esy/something/.sandbox/$SANDBOX_HASH/default/sub-test.t/run.t:
  No such file or directory
  Error: open:
  $TESTCASE_ROOT/_esy/something/default/sub-test.t/.hello.eobjs/hello.ml.d:
  No such file or directory
