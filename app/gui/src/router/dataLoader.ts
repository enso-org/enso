import { assert } from '@/util/assert'
import type { Result } from 'enso-common/src/utilities/data/result'
import {
  type Component,
  type ComponentOptionsMixin,
  type ComponentProvideOptions,
  type ComputedOptions,
  defineComponent,
  type DefineComponent,
  type Directive,
  effectScope,
  type EffectScope,
  type EmitsOptions,
  h,
  type MethodOptions,
  reactive,
  type SlotsType,
} from 'vue'
import type { NavigationGuardReturn, RouteLocationNormalizedGeneric } from 'vue-router'

/**
 * A loader which needs be exported from *.vue file to be wrapped in {@link withDataLoader}.
 */
export type DataLoader<Props> = {
  /**
   * Like `beforeRouteEnter` nagivation guard, but returns data to be passed as component props,
   * or `Err` with proper response for navigation failure/redirect.
   */
  beforeRouteEnter(
    to: RouteLocationNormalizedGeneric,
    from: RouteLocationNormalizedGeneric,
  ): Promise<Result<Props, Exclude<NavigationGuardReturn, void | undefined | true>>>

  /**
   * Like `beforeRouteUpdate` nagivation guard, but is given `data` object which may be modified
   * to update component props.
   */
  beforeRouteUpdate?(
    to: RouteLocationNormalizedGeneric,
    from: RouteLocationNormalizedGeneric,
    data: Props,
  ): Promise<NavigationGuardReturn> | NavigationGuardReturn
}

/**
 * Wrap component with data loader, creating a component which loads data as part of
 * navigation and passed them as props for given inner component.
 *
 * It gives a nicer API than [the official way of doing this](https://router.vuejs.org/guide/advanced/data-fetching.html#Fetching-Before-Navigation).
 *
 * The inner component has to export `dataLoader` object of {@link DataLoader} type,
 * which contains `beforeRouteEnter` and optionally `beforeRouteUpdate` navigation guards
 * with enhanced signature:
 * - beforeRouteEnter returns loaded data which will be passed as props to the inner component.
 * - beforeRouteUpdate may update the data (and refresh the inner component's props)
 *
 * Both guards are run in global injection context and in an {@link effectScope} tied to
 * component's lifetime. **Remember, that these both are not passed to promises, so they
 * work only until the first await in async function.** All injections must be done before first
 * await, and if you want to use watchers later, you must manaully run in an attached scope
 * (see `useUserAgrements` for an example).
 */
export function withDataLoader<
  PropsOrPropOptions extends object,
  RawBindings,
  D,
  C extends ComputedOptions,
  M extends MethodOptions,
  Mixin extends ComponentOptionsMixin,
  Extends extends ComponentOptionsMixin,
  E extends EmitsOptions,
  EE extends string,
  PP,
  Props,
  Defaults,
  S extends SlotsType,
  LC extends Record<string, Component>,
  Directives extends Record<string, Directive>,
  Exposed extends string,
  Provide extends ComponentProvideOptions,
  MakeDefaultsOptional extends boolean,
  TypeRefs extends Record<string, unknown>,
  TypeEl extends Element,
>(
  componentPromise: () => Promise<{
    dataLoader: DataLoader<PropsOrPropOptions>
    default: DefineComponent<
      PropsOrPropOptions,
      RawBindings,
      D,
      C,
      M,
      Mixin,
      Extends,
      E,
      EE,
      PP,
      Props,
      Defaults,
      S,
      LC,
      Directives,
      Exposed,
      Provide,
      MakeDefaultsOptional,
      TypeRefs,
      TypeEl
    >
  }>,
) {
  return async () => {
    const { default: component, dataLoader } = await componentPromise()
    let scope: EffectScope | undefined
    let data: any

    return defineComponent({
      beforeRouteEnter(to, from) {
        scope?.stop()
        scope = effectScope()
        return (
          scope.run(async () => {
            const result = await dataLoader.beforeRouteEnter(to, from)
            if (!result?.ok) {
              return result?.error.payload ?? false
            }
            data = reactive(result.value)
            return true
          })! ?? false
        )
      },
      beforeRouteUpdate(to, from) {
        // This in-component guard should not be called before `beforeRouteEnter`.
        DEV: assert(scope != null)
        return scope?.run(() => dataLoader.beforeRouteUpdate?.(to, from, data))
      },
      unmounted() {
        scope?.stop()
      },
      render() {
        return h(component, data)
      },
    })
  }
}
