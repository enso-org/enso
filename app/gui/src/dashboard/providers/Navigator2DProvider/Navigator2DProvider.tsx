/**
 * @file The React provider for 2D navigation, along with hooks to use the provider via
 * the shared React context.
 */
import { Navigator2D } from '#/utilities/Navigator2D'
import { useState, type PropsWithChildren } from 'react'
import { Navigator2DContext } from './constants'

/** Props for a {@link Navigator2DProvider}. */
export type Navigator2DProviderProps = Readonly<PropsWithChildren>

/** A React Provider that lets components get the 2D navigator. */
export function Navigator2DProvider(props: Navigator2DProviderProps) {
  const { children } = props
  const [navigator2D] = useState(() => new Navigator2D())

  return <Navigator2DContext.Provider value={navigator2D}>{children}</Navigator2DContext.Provider>
}
