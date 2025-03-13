/** @file Hooks for `Text`. */
import { useContext } from 'react'
import { TextContext, type TextContextType } from './constants'

/** Hook to get the Text context. */
export function useTextContext(): TextContextType {
  return useContext(TextContext)
}
