/** @file Constants for `Navigator2DProvider`. */
import { Navigator2D } from '#/utilities/Navigator2D'
import { createContext } from 'react'

/** State contained in a `Navigator2DContext`. */
export type Navigator2DContextType = Navigator2D

export const Navigator2DContext = createContext<Navigator2DContextType>(new Navigator2D())
