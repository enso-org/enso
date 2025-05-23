<script setup lang="ts">
import { ROTATING_ELEMENT_SIZE } from '#/components/Spinner'
import { twJoin } from '#/utilities/tailwindMerge'

const SVG_VIEWBOX_SIZE = '24'
/* NOTE: Due to some bug in vue, when a component is used both as web components and normally,
  It's styles are applied only on the former. Therefore the tailwind classes are applied here 
  as well */
const SPINNER_CSS_CLASSES = {
  initial: 'spinner-initial dasharray-5 ease-linear',
  'loading-slow': 'spinner-loading spinner-slow dasharray-75 duration-spinner-slow ease-linear',
  'loading-medium':
    'spinner-loading spinner-medium dasharray-75 duration-spinner-medium ease-linear',
  'loading-fast': 'spinner-loading spinner-fast dasharray-75 duration-spinner-fast ease-linear',
  done: 'spinner-done spinner-fast dasharray-100 duration-spinner-fast ease-in',
} as const

export type SpinnerPhase = keyof typeof SPINNER_CSS_CLASSES

export interface SpinnerProps {
  size?: number | undefined
  phase: SpinnerPhase
  thickness?: number | undefined
}

const { size = 30, phase, thickness = 3 } = defineProps<SpinnerProps>()
</script>

<template>
  <svg
    :width="size"
    :height="size"
    class="LoadingSpinner"
    :viewBox="`0 0 ${SVG_VIEWBOX_SIZE} ${SVG_VIEWBOX_SIZE}`"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
    aria-hidden="true"
    data-testid="spinner"
    :style="{
      '--full-radius': `calc(pi * ${SVG_VIEWBOX_SIZE})`,
    }"
  >
    <rect
      :x="thickness / 2"
      :y="thickness / 2"
      :width="ROTATING_ELEMENT_SIZE - thickness"
      :height="ROTATING_ELEMENT_SIZE - thickness"
      :rx="ROTATING_ELEMENT_SIZE / 2 - thickness / 2"
      stroke="currentColor"
      strokeLinecap="round"
      :strokeWidth="thickness"
      class="spinner spinner-loading"
      :class="
        twJoin(
          'pointer-events-none origin-center !animate-spin-ease transition-stroke-dasharray',
          SPINNER_CSS_CLASSES[phase],
        )
      "
    />
  </svg>
</template>

<style scoped>
.LoadingSpinner {
  pointer-events: none;
}

@keyframes spin {
  to {
    transform: rotate(360deg);
  }
}

.spinner {
  pointer-events: none;
  animation-timing-function: linear;
  animation: spin cubic-bezier(0.67, 0.33, 0.33, 0.67) 1.5s infinite !important;
  transform-origin: center;
  transition-property: stroke-dasharray;
}

.spinner-initial {
  stroke-dasharray: calc(0.05 * var(--full-radius)) var(--full-radius);
  transition-timing-function: linear;
}

.spinner-loading {
  stroke-dasharray: calc(0.75 * var(--full-radius)) var(--full-radius);
  transition-timing-function: linear;
}

.spinner-slow {
  transition-duration: 90s;
}

.spinner-medium {
  transition-duration: 5s;
}

.spinner-fast {
  transition-duration: 1s;
}

.spinner-done {
  stroke-dasharray: var(--full-radius) var(--full-radius);
  transition-timing-function: cubic-bezier(0.4, 0, 1, 1);
}
</style>
