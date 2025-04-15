/**
 * @file This module is guaranteed to be imported first, and thus to execute before any
 * other script (including our dependencies).
 */

import * as detect from 'enso-common/src/detect'

if (detect.IS_DEV_MODE) {
  suppressReactAriaConsoleWarnings()
  suppressVueDevToolsConsoleWarnings()
}

function suppressConsoleMessage(
  message: string | RegExp | (string | RegExp)[] | ((...args: unknown[]) => boolean),
  level: 'warn' | 'error' | 'log' | 'debug' | 'info' = 'warn',
) {
  const originalConsoleMethod = console[level]

  console[level] = function overrideConsoleMethod(...args: unknown[]) {
    let shouldSuppress = false

    switch (true) {
      case typeof message === 'function':
        shouldSuppress = message(...args)
        break
      case typeof message === 'string':
        shouldSuppress = args[0] === message
        break
      case Array.isArray(message):
        shouldSuppress = message.some((m) =>
          typeof m === 'string' ? args[0] === m : m.test(args[0] as string),
        )
        break
      default:
        shouldSuppress = message.test(args[0] as string)
        break
    }

    if (shouldSuppress) {
      return
    }

    return originalConsoleMethod.apply(console, args)
  }
}

function suppressReactAriaConsoleWarnings() {
  suppressConsoleMessage(/A PressResponder was rendered without a pressable child/)
  suppressConsoleMessage(/Download the React DevTools for a better development experience/, 'info')
}

function suppressVueDevToolsConsoleWarnings() {
  suppressConsoleMessage((...args) => args[1] === 'data-v-inspector', 'error')
}
