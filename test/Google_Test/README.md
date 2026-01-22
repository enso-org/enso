This is a set of tests for the Google integration for Enso.

## Testing Google Sheets

To run the tests, you need to supply credentials for Google Sheets. Get your IDE
ready, for example by:

```bash
enso$ ./run ide build
enso$ ls ./dist/ide/enso-linux-*.AppImage
```

You need to log in to your Enso Cloud account that you are running the tests
against and create a new Google credential with `Sheets` scope which must be
called `GoogleSheets`.

<img width="620" height="448" alt="image" src="https://github.com/user-attachments/assets/9b889267-46eb-4972-8dbc-51ba0abe64d3" />

As a result a `$HOME/.enso/credentials` file gets created. Then one can execute
the tests:

```
enso$ sbt
sbt:enso> runEngineDistribution --run test/Google_Test
```

To execute the same tests in _dual JVM mode_ one can alter the last line to:

```
sbt:enso> runEngineDistribution --run test/Google_Test --vm.D=polyglot.enso.classLoading=Standard.Google:guest,enso_dev.Google_Test:guest,hosted
```
