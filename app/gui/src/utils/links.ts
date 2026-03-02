import { useRoute, useRouter, type RouteLocationNormalizedLoaded, type Router } from 'vue-router'

export function isEnsoLink(href: string) {
  return URL.parse(href)?.protocol === 'enso:'
}

export function useNavigateLink(
  router: Pick<Router, 'push'> = useRouter(),
  route: Pick<RouteLocationNormalizedLoaded, 'query'> = useRoute(),
) {
  return async (href: string) => {
    if (isEnsoLink(href)) {
      await router.push({ params: { path: href.split('/') }, query: route.query })
    } else {
      window.open(href, '_blank', 'noopener,noreferrer')
    }
  }
}
