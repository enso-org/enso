import { VueQueryPlugin } from '@tanstack/vue-query'
import { assert } from 'vitest'
import { createApp, type App } from 'vue'

/**
 * An utility for testing composables that actually depend on vue's `setup` environment.
 * The code is taken straight from vue documentation, modified only to add typings:
 * https://vuejs.org/guide/scaling-up/testing.html#testing-composables
 */
export function appWithSetup<T>(composable: () => T): [T, App] {
  let result: { value: T } | undefined
  const app = createApp({
    setup() {
      result = { value: composable() }
      // suppress missing template warning
      return () => {}
    },
  })
  app.use(VueQueryPlugin)
  app.mount(document.createElement('div'))

  assert(result != null)
  // return the result and the app instance
  // for testing provide/unmount
  return [result.value, app]
}

/** A version of {@link appWithSetup} that only returns a result. */
export function withSetup<T>(composable: () => T): T {
  return appWithSetup(composable)[0]
}
