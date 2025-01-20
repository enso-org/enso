import { type App, createApp } from 'vue'

/**
 * An utility for testing composables that actually depend on vue's `setup` environment.
 * The code is taken straight from vue documentation, modified only to add typings:
 * https://vuejs.org/guide/scaling-up/testing.html#testing-composables
 */
export function withSetup<T>(composable: () => T): [T | undefined, App] {
  let result: T | undefined
  const app = createApp({
    setup() {
      result = composable()
      // suppress missing template warning
      return () => {}
    },
  })
  app.mount(document.createElement('div'))
  // return the result and the app instance
  // for testing provide/unmount
  return [result, app]
}
