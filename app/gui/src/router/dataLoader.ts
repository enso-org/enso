import { assert } from '@/util/assert'
import { Result } from '@/util/data/result'
import { QueryClient } from '@tanstack/vue-query'
import {
  Component,
  ComponentOptionsMixin,
  ComponentProvideOptions,
  ComputedOptions,
  defineComponent,
  DefineComponent,
  Directive,
  effectScope,
  EffectScope,
  EmitsOptions,
  h,
  MethodOptions,
  reactive,
  SlotsType,
} from 'vue'
import { NavigationGuardReturn, RouteLocationNormalizedGeneric } from 'vue-router'

declare module 'vue' {
  interface ComponentCustomOptions {
    dataLoader?: (this: undefined, queryClient: QueryClient) => Promise<any>
  }
}

export type NavigationDataLoader<Props> = (
  to: RouteLocationNormalizedGeneric,
  from: RouteLocationNormalizedGeneric,
) => Promise<Result<Props, Exclude<NavigationGuardReturn, void | undefined | true>>>

export type DataLoader<Props> = {
  beforeRouteEnter: NavigationDataLoader<Props>
  beforeRouteUpdate?(
    to: RouteLocationNormalizedGeneric,
    from: RouteLocationNormalizedGeneric,
    data: Props,
  ): Promise<NavigationGuardReturn> | NavigationGuardReturn
}

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
