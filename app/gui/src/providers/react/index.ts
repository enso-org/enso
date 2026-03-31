import type LocalStorage from '#/utilities/LocalStorage'
import { useIsFeatureUnderPaywall as useIsFeatureUnderPaywallVue } from '$/composables/paywall/paywall'
import type { ActionsStore } from '$/providers/actions'
import type { ConfigStore, RemoteConfig } from '$/providers/config'
import type { SessionStore } from '$/providers/session'
import type { TextStore } from '$/providers/text'
import type { HttpClient } from 'enso-common/src/services/HttpClient'
import { createContext, useCallback } from 'react'
import { useInReactFunction, useVueValue } from './common'

export { useAuth, useFullUserSession, useUser, useUserSession } from '$/providers/react/auth'
export { useBackends } from '$/providers/react/backends'
export { useRouter } from '$/providers/react/router'

export const ConfigContext = createContext<ConfigStore | null>(null)
export const useConfig = useInReactFunction(ConfigContext)
/** Use a concrete key of remote config in React. */
export function useRemoteConfig(key: keyof RemoteConfig) {
  const config = useConfig()
  return useVueValue(useCallback(() => config.remoteConfig?.[key], [config]))
}

export const TextContext = createContext<TextStore | null>(null)
export const useText = useInReactFunction(TextContext)

export const HTTPClientContext = createContext<HttpClient | null>(null)
export const useHttpClient = useInReactFunction(HTTPClientContext)

export const LocalStorageContext = createContext<LocalStorage | null>(null)
export const useLocalStorage = useInReactFunction(LocalStorageContext)

export const SessionContext = createContext<SessionStore | null>(null)
export const useSession = useInReactFunction(SessionContext)

export const ActionsContext = createContext<ActionsStore | null>(null)
export const useActionsStore = useInReactFunction(ActionsContext)

export type IsFeatureUnderPaywallFuntion = ReturnType<typeof useIsFeatureUnderPaywallVue>
export const IsFeatureUnderPaywallContext = createContext<IsFeatureUnderPaywallFuntion | null>(null)
export const useIsFeatureUnderPaywall = useInReactFunction(IsFeatureUnderPaywallContext)
