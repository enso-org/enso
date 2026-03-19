export function createLogger({ verbose = false } = {}) {
  return {
    info: (...args) => {
      console.log(...args)
    },
    debug: (...args) => {
      if (verbose) {
        console.log(...args)
      }
    },
    warn: (...args) => {
      console.warn(...args)
    },
    error: (...args) => {
      console.error(...args)
    },
  }
}
