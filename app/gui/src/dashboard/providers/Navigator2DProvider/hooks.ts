/** @file Hooks for `Navigator2DProvider`. */
import { useContext } from 'react'
import { Navigator2DContext } from './constants'

/** Exposes a property to get the 2D navigator namespace. */
export function useNavigator2D() {
  return useContext(Navigator2DContext)
}
