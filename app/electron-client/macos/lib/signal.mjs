export function installCleanupHandlers(cleanup) {
  let running = false

  async function handleSignal(signal) {
    if (running) {
      return
    }
    running = true
    try {
      await cleanup()
    } finally {
      const exitCode = signal === 'SIGINT' ? 130 : 143
      process.exit(exitCode)
    }
  }

  const onSigInt = () => {
    void handleSignal('SIGINT')
  }
  const onSigTerm = () => {
    void handleSignal('SIGTERM')
  }

  process.once('SIGINT', onSigInt)
  process.once('SIGTERM', onSigTerm)

  return () => {
    process.removeListener('SIGINT', onSigInt)
    process.removeListener('SIGTERM', onSigTerm)
  }
}
