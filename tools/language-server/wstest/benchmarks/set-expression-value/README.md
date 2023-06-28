# set-expression-value

Benchmark for command that sets the expression value. It sends the
`text/applyExpressionValue` command and waits for the visualization message on
binary WebSocket.

## Run

Build Enso distribution.

```bash
sbt buildEngineDistribution
```

Start the Language Server and redirect ouput to `language-server.log`

```bash
built-distribution/enso-engine-0.0.0-dev-linux-amd64/enso-0.0.0-dev/bin/enso \
  --log-level trace \
  --server \
  --root-id 6f7d58dd-8ee8-44cf-9ab7-9f0454033641 \
  --path $(pwd)/tools/language-server/wstest/benchmarks/set-expression-value/Unnamed/ \
  --rpc-port 30616 \
  --data-port 30717 \
  2>&1| tee language-server.log
```

Run the test and redirect output to `wstest.log`

```bash
cargo run -p wstest -- \
  --binary-socket ws://127.0.0.1:30717 \
  --init-binary-socket tools/language-server/wstest/benchmarks/set-expression-value/init.bin \
  --init-text-socket tools/language-server/wstest/benchmarks/set-expression-value/init.txt \
  --wait-after-init 10000 \
  --ignore-text-socket-responses tools/language-server/wstest/benchmarks/set-expression-value/ignore_responses.txt \
  --input tools/language-server/wstest/benchmarks/set-expression-value/input.txt \
  --input-expects-binary-responses \
  --warmup-iterations 100 \
  --wait-after-warmup 3000 \
  --benchmark-iterations 100 \
  ws://127.0.0.1:30616 \
  2>&1| tee wstest.log
```

Analyze logs

```bash
cargo run -p logstat -- \
  --median \
  --spec tools/language-server/logstat/apply-expression-value-spec.txt \
  --wstest-log wstest.log \
  language-server.log
```

Example output

```text
avg [min..max] (of 150 records)
0ms [0..0] [main] wstest sent bench request [{ "jsonrpc": "2.0", "method": "text/applyExpre
0ms [0..2] [org.enso.jsonrpc.JsonRpcServer] Received text message: { "jsonrpc": "2.0", "met
0ms [0..1] [org.enso.languageserver.protocol.json.JsonConnectionController] received handle
1ms [0..2] [org.enso.languageserver.runtime.RuntimeConnector] received handled Request(None
0ms [0..2] [enso] Executing command: SetExpressionValueCmd...
5ms [3..16] [enso] Job EnsureCompiledJob finished in 5 ms.
1ms [0..9] [enso] Visualization computed 524dd815-b652-4bbe-b9f2-26b35d17993a.
0ms [0..1] [org.enso.languageserver.runtime.ContextRegistry] received handled Visualization
1ms [0..1] [main] wstest handled response [<binary>]
8ms [7..22] Total
```
