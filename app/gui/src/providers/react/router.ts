import * as react from 'react'
import { Router, useRoute } from 'vue-router'
import { useInReactFunction } from './common'

export interface RouterForReact {
  router: Router
  route: ReturnType<typeof useRoute>
  searchParams: URLSearchParams
}
export const RouterContext = react.createContext<RouterForReact | null>(null)
export const useRouter = useInReactFunction(RouterContext)
