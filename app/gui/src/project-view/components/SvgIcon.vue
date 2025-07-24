<script lang="ts">
import iconsSvgUrl from '@/assets/icons.svg'
import { type URLString } from '@/util/data/urlString'
import type { Icon } from '@/util/iconMetadata/iconName'
import { embedSvgSymbols, svgUseHref } from '@/util/icons'

let embeddedSvg: SVGSVGElement | null = null

// Embed icons SVG into the document body, so the symbols can be referenced in SVG `use` element directly by their ID.
// We used to reference the icons using `icons.svg#symbol-id` format, but that occassionaly causes issues with
// cross-origin requests. This is impacting the playwright test snapshots, which would end up requesting the svg using
// the origin used during test run and attempt to resolve it with a service-worker. That request unfortunately gets
// rejected immediately. To avoid that, we embed the resource directly on page load and use local IDs. That approach
// also works inside shadow-root, so it is safe to reference the same icon symbols in visualizations.
function fetchIcons() {
  fetch(iconsSvgUrl, { cache: 'default', mode: 'same-origin' }).then(
    async (response) => (embeddedSvg = embedSvgSymbols(await response.text())),
  )
}

// Skip fetching icons in unit tests, node's `fetch` there would fail to resolve the URL properly
// and we don't need the icons to actually be loaded for any purpose anyway.
if (process.env.NODE_ENV !== 'test') {
  fetchIcons()
}

// In case we hot-reload the icons, remove previously embedded SVG.
if (import.meta.hot) {
  import.meta.hot.dispose(() => {
    embeddedSvg?.remove()
  })
}

/**
 * A component displaying a SVG icon.
 *
 * It displays one group defined in `@/assets/icons.svg` file, specified by `variant` property.
 */
export default {}
</script>

<script setup lang="ts">
const { name } = defineProps<{ name: Icon | URLString }>()
</script>

<template>
  <svg class="SvgIcon" viewBox="0 0 16 16" preserveAspectRatio="xMidYMid slice">
    <use :href="svgUseHref(name)"></use>
  </svg>
</template>

<style scoped>
svg.SvgIcon {
  overflow: visible; /* Prevent slight cutting off icons that are using all available space. */
  width: var(--icon-width, var(--icon-size, 16px));
  height: var(--icon-height, var(--icon-size, 16px));
  transform: var(--icon-transform);
  transform-origin: var(--icon-transform-origin, center center);
}
</style>
