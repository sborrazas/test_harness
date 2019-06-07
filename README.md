test_harness
=====

You can use this to test your code! To run the Common Test test suite against your server at `localhost:4567`:

```
./rebar3 ct --verbose --readable=compact
```

This should fetch the dependencies and run the suite. The test code is located in `test/test_SUITE.erl`.

N.B.: If you get weird Erlang BEAM errors running `rebar3`, maybe replace the file with one built for your platform. The provided copy is for Erlang 22 on Debian x64.
