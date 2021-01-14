  $ echo 'Hello, world!' > hello.expected
  $ dune runtest
  $ ls
  _build
  hello.expected
  run.t
  $ diff hello.output hello.expected
  diff: hello.output: No such file or directory
  [2]
