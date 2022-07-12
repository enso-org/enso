# set-expression-value

Benchmark for command that sets the expression value.

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
  --path tools/language-server/wstest/set-expression-value/Unnamed/ \
  --rpc-port 30616 \
  --data-port 30717 \
  &| tee language-server.log
```

Run the test

```bash
cargo run -p wstest -- \
  --binary-socket ws://127.0.0.1:30717 \
  --init-binary-socket tools/language-server/wstest/set-expression-value/init.bin \
  --init-text-socket tools/language-server/wstest/set-expression-value/init.txt \
  --wait-after-init 10000 \
  --ignore-text-socket-responses tools/language-server/wstest/set-expression-value/ignore_responses.txt \
  --input tools/language-server/wstest/set-expression-value/input.txt \
  --input-expects-binary-responses \
  --input-repeat-times 20 \
  ws://127.0.0.1:30616
```

Analyze the `language-server.log`

```bash
cargo run -p logstat -- \
  --warmup-iterations 20 \
  --spec tools/language-server/logstat/apply-expression-value-spec.txt \
  language-server.log
```

Example output

```text
Stats (of 107 records)
0ms [0..0] [org.enso.jsonrpc.JsonRpcServer] Received text message: { "jsonrpc": "2.0", "met
1ms [0..27] [org.enso.languageserver.protocol.json.JsonConnectionController] received handle
1ms [1..4] [org.enso.languageserver.runtime.RuntimeConnector] received handled Request(None
0ms [0..2] [enso] Executing command: SetExpressionValueCmd...
14ms [8..44] [enso] Job EnsureCompiledJob finished in 19 ms.
2ms [1..13] [enso] Visualisation computed 524dd815-b652-4bbe-b9f2-26b35d17993a.
0ms [0..2] [org.enso.languageserver.runtime.ContextRegistry] received handled Visualisation
22ms [13..53] Total
```
