import * as React from 'react'

import invariant from 'tiny-invariant'

import type * as projectManager from '#/services/ProjectManager'

import type * as types from '../../../types/types'

/**
 *
 */
export interface AppContextType {
  readonly supportsLocalBackend: boolean
  readonly appRunner: types.EditorRunner | null
  readonly initialProjectName: string | null
  readonly projectManagerUrl: string | null
  readonly ydocUrl: string | null
  readonly projectManagerRootDirectory: projectManager.Path | null
}

const AppContext = React.createContext<AppContextType | null>(null)

/**
 *
 */
export interface AppProviderProps extends React.PropsWithChildren, AppContextType {}

/**
 *
 */
export function AppProvider(props: AppProviderProps) {
  const { children, ...appContext } = props

  return <AppContext.Provider value={appContext}>{children}</AppContext.Provider>
}

/**
 *
 */
export function useAppContext() {
  const context = React.useContext(AppContext)

  invariant(context != null, 'Uh oh')

  return context
}
