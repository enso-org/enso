import type LocalStorage from '#/utilities/LocalStorage'
import type { ActionsStore } from '$/providers/actions'
import type { SessionStore } from '$/providers/session'
import type { TextStore } from '$/providers/text'
import type { GuiConfig } from '@/providers/guiConfig'
import type { HttpClient } from 'enso-common/src/services/HttpClient'
import { createContext } from 'react'
import { useInReactFunction } from './common'

export { useAuth, useFullUserSession, useUser, useUserSession } from '$/providers/react/auth'
export { useBackends } from '$/providers/react/backends'
export { useRouter } from '$/providers/react/router'

export const ConfigContext = createContext<GuiConfig | null>(null)
export const useConfig = useInReactFunction(ConfigContext)

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
