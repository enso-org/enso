/**
 * Normalize the vue-router route param to string by joining possible array segments with
 * separators.
 */
export function normalizeRouteParamToString(routeParam: string | string[] | undefined) {
  return routeParam instanceof Array ? routeParam.join('/') : (routeParam ?? '')
}
