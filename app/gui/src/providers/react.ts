import {
  allModeReturn,
  createCrossingProviderForPureReactInVue,
  ReactContext,
  VueComponent,
} from 'veaury'

/**
 * A wrapper for {@link createCrossingProviderForPureReactInVue}, because veaury is very bad at
 * types.
 */
export function createContextForReact<T extends allModeReturn>(
  constructor: () => T,
): [useInReact: () => T, Provider: VueComponent, ReactContext] {
  return createCrossingProviderForPureReactInVue(constructor) as any
}
