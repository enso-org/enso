import { useRoute, useRouter, type RouteLocationNormalizedLoaded, type Router } from 'vue-router'

/** Check if given link is an enso:// link. */
export function isEnsoLink(href: string) {
  return URL.parse(href)?.protocol === 'enso:'
}

/**
 * Composable returning a method for navigation to links, including proper handling of enso://
 * links.
 */
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
