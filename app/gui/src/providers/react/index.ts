import LocalStorage from '#/utilities/LocalStorage'
import { SessionStore } from '$/providers/session'
import { TextStore } from '$/providers/text'
import { GuiConfig } from '@/providers/guiConfig'
import { HttpClient } from 'enso-common/src/services/HttpClient'
import * as react from 'react'
import { useInReactFunction } from './common'

export { useAuth, useFullUserSession, useUser, useUserSession } from '$/providers/react/auth'
export { useBackends } from '$/providers/react/backends'
export { useRightPanelData } from '$/providers/react/container'
export { useRouter } from '$/providers/react/router'

export const ConfigContext = react.createContext<GuiConfig | null>(null)
export const useConfig = useInReactFunction(ConfigContext)

export const TextContext = react.createContext<TextStore | null>(null)
export const useText = useInReactFunction(TextContext)

export const HTTPClientContext = react.createContext<HttpClient | null>(null)
export const useHttpClient = useInReactFunction(HTTPClientContext)

export const LocalStorageContext = react.createContext<LocalStorage | null>(null)
export const useLocalStorage = useInReactFunction(LocalStorageContext)

export const SessionContext = react.createContext<SessionStore | null>(null)
export const useSession = useInReactFunction(SessionContext)
