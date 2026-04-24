/** @file Application-wide logging setup by overriding standard console methods. */

/** Helper function for displaying errors in logs. */
function serializeError(err: unknown) {
  if (err instanceof Error) {
    return {
      name: err.name,
      message: err.message,
      stack: err.stack,
      cause: err.cause ? String(err.cause) : undefined,
    }
  }
  try {
    return { message: JSON.stringify(err) }
  } catch {
    return { message: String(err) }
  }
}

/**
 * Override standard console methods with our own, that also log to the file using Electron.
 * Also setup event listeners for unhandled errors and resource load errors.
 *
 * Only the following methods are overridden:
 * - `console.log`
 * - `console.info`
 * - `console.error`
 * - `console.warn`
 */
export function setupLogger() {
  if (window.api) {
    const logApi = window.api.log
    const originalConsoleLog = window.console.log
    const originalConsoleInfo = window.console.info
    const originalConsoleError = window.console.error
    const originalConsoleWarn = window.console.warn

    window.console.log = (...args) => {
      originalConsoleLog.apply(window.console, args)
      logApi.log(args)
    }
    window.console.info = (...args) => {
      originalConsoleInfo.apply(window.console, args)
      logApi.info(args)
    }
    window.console.error = (...args) => {
      originalConsoleError.apply(window.console, args)
      logApi.error(args)
    }
    window.console.warn = (...args) => {
      originalConsoleWarn.apply(window.console, args)
      logApi.warn(args)
    }
  }

  window.addEventListener(
    'error',
    (event) => {
      const target = event.target
      if (
        target &&
        (target instanceof HTMLScriptElement ||
          target instanceof HTMLLinkElement ||
          target instanceof HTMLImageElement)
      ) {
        const url =
          'src' in target ? target.src
          : 'href' in target ? target.href
          : undefined
        console.error('Resource load error', { tag: target.tagName, url })
      } else {
        console.error(event.message, {
          filename: event.filename,
          lineno: event.lineno,
          colno: event.colno,
          error: serializeError(event.error),
        })
      }
    },
    { capture: true },
  )

  window.addEventListener(
    'unhandledrejection',
    (event) => {
      console.error('Unhandled promise rejection', { error: serializeError(event.reason) })
    },
    { capture: true },
  )
}
