import { inject, provide, type InjectionKey } from 'vue'

const MISSING = Symbol('MISSING')

export function createProvidable<F extends (...args: any[]) => any>(factory: F) {
  const provideKey = Symbol() as InjectionKey<ReturnType<F>>
  function provideFn(...args: Parameters<F>): ReturnType<F> {
    const constructed = factory(...args)
    provide(provideKey, constructed)
    return constructed
  }

  function useFn(allowMissing: true): ReturnType<F> | undefined
  function useFn(allowMissing?: false): ReturnType<F>
  function useFn(allowMissing = false): ReturnType<F> | undefined {
    const injected = inject<ReturnType<F> | typeof MISSING>(provideKey, MISSING)
    if (injected === MISSING) {
      if (allowMissing) return
      throw new Error(`Trying to inject ${name}, which is not provided`)
    }
    return injected
  }
  return { provideFn: provideFn as F, useFn } as const
}
