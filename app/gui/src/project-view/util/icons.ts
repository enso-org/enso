import missingIconUri from '@/assets/icon-missing.svg?no-inline'
import iconsUri from '@/assets/icons.svg?no-inline'
import { isUrlString, type URLString } from '@/util/data/urlString'
import { isIconName, type Icon } from '@/util/iconMetadata/iconName'

export type AnyIcon = Icon | URLString

export type AnyWidgetIcon = AnyIcon | '$evaluating'

/**
 * Get the SVG use `href` property value for given icon.
 *
 * Expects icons of type {@link AnyIcon}. Returns a "missing icon" URL for invalid icons.
 */
export function svgUseHref(icon: AnyIcon | string): string {
  return (
    isIconName(icon) ? iconsUri + '#' + icon
    : isUrlString(icon) && icon.match(/\.svg(#|$)/) ? encodeURI(icon)
    : missingIconUri + '#missing'
  )
}
