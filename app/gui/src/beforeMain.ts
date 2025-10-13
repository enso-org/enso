/**
 * @file This module is guaranteed to be imported first, and thus to execute before any
 * other script (including our dependencies).
 */

declare const __REACT_DEVTOOLS_GLOBAL_HOOK__: any
declare const __VUE_DEVTOOLS_KIT_CONTEXT__: any
declare const __VUE_DEVTOOLS_OPEN_IN_EDITOR_BASE_URL__: string | undefined

DEV: {
  // Avoid annoying "use react devtools" log.
  if (
    typeof __REACT_DEVTOOLS_GLOBAL_HOOK__ === 'undefined' ||
    typeof __REACT_DEVTOOLS_GLOBAL_HOOK__.checkDCE !== 'function'
  ) {
    ;(window as any).__REACT_DEVTOOLS_GLOBAL_HOOK__ = { isDisabled: true }
  }

  // Restore "open editor" function in devtools, without introducing extra DOM properties that make react scream.

  if (typeof __VUE_DEVTOOLS_KIT_CONTEXT__ !== 'undefined') {
    __VUE_DEVTOOLS_KIT_CONTEXT__.api.openInEditor = async (options: {
      file: string
      baseUrl?: string
      line?: number
      column?: number
    }) => {
      const { file, baseUrl, line = 0, column = 0 } = options
      const _baseUrl = baseUrl ?? __VUE_DEVTOOLS_OPEN_IN_EDITOR_BASE_URL__ ?? window.location.origin
      const fileLocation = `${file}:${line}:${column}`
      return fetch(`${_baseUrl}/__open-in-editor?file=${encodeURIComponent(fileLocation)}`)
    }
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  interface Object {
    /**
     * Quick and convenient way to `console.debug` the proceeding expression without affecting its output.
     * Note that this has a noticeable overhead due to processing stack traces.
     *
     * ```ts
     * $dbg[complicated.expression] || $dbg[other]
     * ```
     */
    $dbg: this
  }

  Object.defineProperty(Object.prototype, '$dbg', {
    get: function get() {
      const { stackTraceLimit } = Error
      try {
        Error.stackTraceLimit = 0
        const error = new Error()
        Error.stackTraceLimit = 2
        Error.captureStackTrace(Error, get)
        console.log(error.stack, this)
        return this
      } finally {
        Error.stackTraceLimit = stackTraceLimit
      }
    },
  })
}
