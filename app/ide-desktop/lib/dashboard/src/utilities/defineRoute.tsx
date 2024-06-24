import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import type * as routerDom from 'react-router-dom'

import * as errorBoundaryComponent from '#/components/ErrorBoundary'
import * as suspense from '#/components/Suspense'

/**
 *
 */
export interface Route {
  readonly path?: string
  readonly prefetchQueries?:
    | reactQuery.UseQueryOptions[]
    | ((request: Request, params: routerDom.Params) => reactQuery.UseQueryOptions[])
  readonly element?: React.ComponentType<Record<never, never>>
  readonly errorBoundary?: React.ComponentType
  readonly lazy?: routerDom.RouteProps['lazy']
}

/**
 *
 */
export default function defineRoute(route: Route): routerDom.RouteProps {
  const {
    prefetchQueries,
    path,
    element: Element,
    errorBoundary = errorBoundaryComponent.ErrorBoundary,
    ...directRouteProps
  } = route

  return {
    ...directRouteProps,
    path,

    // eslint-disable-next-line @typescript-eslint/naming-convention
    ErrorBoundary: errorBoundary,
    hydrateFallbackElement: <suspense.Suspense />,
    element: <suspense.Suspense>{Element != null ? <Element /> : null}</suspense.Suspense>,

    loader: ({ request, params }) => {
      const client = reactQuery.useQueryClient()

      const queries =
        typeof prefetchQueries === 'function'
          ? prefetchQueries(request, params)
          : prefetchQueries ?? []

      return Promise.allSettled(queries.map(query => client.prefetchQuery(query)))
    },
  }
}
