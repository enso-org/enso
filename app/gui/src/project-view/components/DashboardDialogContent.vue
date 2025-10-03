<script setup lang="ts">
/**
 * @file Implements the internal layout of the dashboard's `Dialog` non-modally (suitably for
 * embedding in another component).
 */

import {
  DIALOG_MODAL_STYLES,
  DIALOG_OVERLAY_STYLES,
  DIALOG_STYLES,
} from '#/components/Dialog/variants'
import { TEXT_STYLE } from '#/components/Text/variants'
import { computed } from 'vue'

const { heading, blockInteractions = false } = defineProps<{
  heading: string
  /** This is used to determine styles (this component doesn't implement modal behaviour). */
  blockInteractions?: boolean
}>()

const overlayClasses = computed(() => DIALOG_OVERLAY_STYLES({ blockInteractions }))
const modalClasses = DIALOG_MODAL_STYLES({})
const styles = DIALOG_STYLES({ padding: 'medium' })
const headingClasses = TEXT_STYLE({ weight: 'semibold', variant: 'h1' })
</script>

<template>
  <div
    class="DashboardDialogContent position-unset width-full border-radius-inner"
    :class="overlayClasses"
  >
    <div class="position-unset width-full border-radius-inner" :class="modalClasses">
      <div class="width-full border-radius-inner" :class="styles.base()">
        <header :class="styles.header()">
          <h2 :class="`${styles.heading()} ${headingClasses}`">{{ heading }}</h2>
        </header>
        <div class="width-full border-radius-inner" :class="styles.content()">
          <slot />
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.width-full {
  width: 100%;
}

.border-radius-inner {
  /*noinspection CssUnresolvedCustomProperty*/
  border-radius: 0 0 var(--dialog-border-radius, var(--border-radius-inner))
    var(--dialog-border-radius, var(--border-radius-inner));
}

.position-unset {
  position: unset;
}
</style>
