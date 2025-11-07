import { assert } from '@/util/assert'
import {
  getCurrentInstance,
  inject,
  provide,
  type ComponentInternalInstance,
  type InjectionKey,
} from 'vue'

const MISSING = Symbol('MISSING')

/**
 * A version of vue's {@link inject} that will also inject a value provided earlier
 * within the same component's setup function.
 */
export function injectImmediate<T>(key: InjectionKey<T>, defaultValue: T): T {
  // NOTE: Usage of Vue internal API. Consult sources in case this breaks during vue update:
  // https://github.com/vuejs/core/blob/45547e69b25baa99a0ed52ba5110c5bd8b4a35e4/packages/runtime-core/src/apiInject.ts#L28
  const instance = getCurrentInstance() as
    | (ComponentInternalInstance & { provides: Record<InjectionKey<T>, T> })
    | null
  const instanceProvides = instance?.provides

  // Assertion to check if the internal propery exist when we expect it. If this fails after update,
  // this function needs to be updated as well.
  DEV: assert(
    instance == null || 'provides' in instance,
    `Expected vue internal 'provides' property to exist.`,
  )

  if (instanceProvides && key in instanceProvides) return instanceProvides[key] as T
  return inject(key, defaultValue)
}

/**
 * A pair of functions, `provideFn` and `injectFn`. The `provideFn` function creates the
 * store instance and provides it to the context, allowing child components to access it. The
 * `injectFn` function retrieves the store instance provided by any of the an ancestor components.
 */
export type ContextStore<Factory extends (...args: any[]) => any> = [
  /**
   * Create the instance of a store and store it in the current component's context. All child
   * components will be able to access that store using the corresponding inject function.
   * @param args The parameters that will be passed to the store factory function.
   * @returns The store instance created by the factory function.
   */
  provideFn: Factory,
  /**
   * Access a store instance provided by an ancestor component. When trying to access a store that
   * has never been provided, the behavior depends on the first parameter value.
   * @param missingBehavior determines what happens when trying to inject a store has never been provided:
   * - If `missingBehavior` is `false` or it is not provided, an exception is thrown.
   * - If `missingBehavior` is `true`, `undefined` is returned. This is also reflected in the return
   * type of the function.
   * - If `missingBehavior` is a closure returning store factory arguments, a new store instance
   * is created and  provided to this component's context, allowing its children to access it.
   * @returns The store instance provided by an ancestor component, or `undefined` if the store
   * has never been provided and `missingBehavior` is `true`.
   */
  injectFn: {
    (allowMissing?: false, immediate?: boolean): ReturnType<Factory>
    (allowMissing: true, immediate?: boolean): ReturnType<Factory> | undefined
    (allowMissing?: boolean, immediate?: boolean): ReturnType<Factory> | undefined
    (orProvideWith: () => Readonly<Parameters<Factory>>, immediate?: boolean): ReturnType<Factory>
  },
]

/**
 * Create a pair of functions that allow to provide and inject a store in the current component's
 * context. The store is created on demand using the provided factory function. Once a store is
 * provided by a component, all its descendants will be able to access it using the corresponding
 * inject function.The store instance is **NOT a singleton**. Each component that provides
 * the store will create its own instance, and each component that injects the store will receive
 * the instance from the closest ancestor component that provided it.
 *
 * When creating a store, you usually want to reexport the `provideFn` and `injectFn` as renamed
 * functions to make it easier to use the store in components without any name collisions.
 * ```ts
 * export const [provideThing, useThing] = createContextStore('specific thing', thatThingFactory)
 * ```
 *
 * Under the hood, this uses Vue's [Context API], therefore it can only be used within a component's
 * setup function.
 * @param name A user-friendly name for the store, used for error messages and debugging. The name
 * has no influence on the standard runtime behavior of the store, and doesn't have to be unique.
 * @param factory A factory function that creates the store. The parameters expected by the factory
 * will match the parameters of the generated `provideFn` function.
 * @returns A pair of provide and inject functions - {@link ContextStore}.
 *
 * [Context API]: https://vuejs.org/guide/components/provide-inject.html#provide-inject
 */
export function createContextStore<F extends (...args: any[]) => any>(
  name: string,
  factory: F,
): ContextStore<F> {
  const provideKey = Symbol.for(`contextStore-${name}`) as InjectionKey<ReturnType<F>>

  function provideFn(...args: Readonly<Parameters<F>>): ReturnType<F> {
    const constructed = factory(...args)
    provide(provideKey, constructed)
    return constructed
  }

  function injectFn(allowMissing: true, immediate?: boolean): ReturnType<F> | undefined
  function injectFn(allowMissing?: false, immediate?: boolean): ReturnType<F>
  function injectFn(allowMissing?: boolean, immediate?: boolean): ReturnType<F> | undefined
  function injectFn(
    orProvideWith: () => Readonly<Parameters<F>>,
    immediate?: boolean,
  ): ReturnType<F>
  function injectFn(
    missingBehavior: boolean | (() => Readonly<Parameters<F>>) = false,
    immediate = false,
  ): ReturnType<F> | undefined {
    const injector = immediate ? injectImmediate : inject
    const injected = injector<ReturnType<F> | typeof MISSING>(provideKey, MISSING)
    if (injected === MISSING) {
      if (missingBehavior === true) return
      if (typeof missingBehavior === 'function') {
        return provideFn(...missingBehavior())
      } else {
        throw new Error(`Trying to inject ${name}, which is not provided`)
      }
    }
    return injected
  }

  return [provideFn as F, injectFn] as const
}
