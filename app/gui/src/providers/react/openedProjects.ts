import * as react from 'react'
import type { OpenedProjectsStore } from '../openedProjects'
import { useInReactFunction } from './common'

const OpenedProjectsContext = react.createContext<OpenedProjectsStore | null>(null)
export const useOpenedProjects = useInReactFunction(OpenedProjectsContext)
