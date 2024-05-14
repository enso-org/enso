<script setup lang="ts">
import {
  cssAngularColorStop,
  gradientPoints,
  rangesForInputs,
} from '@/components/ColorRing/gradient'
import { injectInteractionHandler } from '@/providers/interactionHandler'
import { endOnClickOutside } from '@/util/autoBlur'
import { cssSupported, ensoColor, formatCssColor, parseCssColor } from '@/util/colors'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed, onMounted, ref } from 'vue'

/**
 *  Hue picker
 *
 *  # Angles
 *
 *  All angles are measured in turns, starting from the 12-o'clock position, normalized to the range 0-1, unless
 *  otherwise specified.
 *  - This is the axis used by CSS gradients (adjustment is necessary when working with trigonometric functions, which
 *    start from the positive x-axis).
 *  - Turns allow constants to be expressed as simple numbers, and can be easily converted to the units used by external
 *    APIs (radians for math, degrees for culori).
 */

// If the browser doesn't support OKLCH gradient interpolation, the gradient will be specified by computing the number
// of points specified here in OKLCH, converting to sRGB if the browser doesn't support OKLCH colors at all, and
// interpolating in sRGB. This number has been found to be enough to look close to the intended colors, without
// excessive gradient complexity (which may affect performance).
const NONNATIVE_OKLCH_INTERPOLATION_STEPS = 12

const FIXED_RANGE_WIDTH = 1 / 16

const selectedColor = defineModel<string | undefined>()
const props = defineProps<{
  matchableColors: Set<string>
}>()
const emit = defineEmits<{
  close: []
}>()

const browserSupportsOklchInterpolation = cssSupported(
  'background-image: conic-gradient(in oklch increasing hue, red, blue)',
)

const svgElement = ref<HTMLElement>()

const interaction = injectInteractionHandler()

onMounted(() => {
  interaction.setCurrent(
    endOnClickOutside(svgElement, {
      cancel: () => emit('close'),
      end: () => emit('close'),
    }),
  )
})

const mouseSelectedAngle = ref<number>()

const triangleAngle = computed(() => {
  if (mouseSelectedAngle.value) return mouseSelectedAngle.value
  if (selectedColor.value) {
    const color = parseCssColor(selectedColor.value)
    if (color?.h) return color.h / 360
  }
  return undefined
})

function cssColor(hue: number) {
  return formatCssColor(ensoColor(hue))
}

// === Events ===

function eventAngle(event: MouseEvent) {
  if (!svgElement.value) return 0
  const origin = Rect.FromDomRect(svgElement.value.getBoundingClientRect()).center()
  const offset = Vec2.FromXY(event).sub(origin)
  return Math.atan2(offset.y, offset.x) / (2 * Math.PI) + 0.25
}

function ringHover(event: MouseEvent) {
  mouseSelectedAngle.value = eventAngle(event)
}
function ringClick(event: MouseEvent) {
  mouseSelectedAngle.value = eventAngle(event)
  if (triangleHue.value != null) selectedColor.value = cssColor(triangleHue.value)
  emit('close')
}

// === Gradient colors ===

const fixedRanges = computed(() => {
  const inputHues = new Set<number>()
  for (const rawColor of props.matchableColors) {
    if (rawColor === selectedColor.value) continue
    const color = parseCssColor(rawColor)
    const hueDeg = color?.h
    if (hueDeg == null) continue
    const hue = hueDeg / 360
    inputHues.add(hue < 0 ? hue + 1 : hue)
  }
  return rangesForInputs(inputHues, FIXED_RANGE_WIDTH / 2)
})

const triangleHue = computed(() => {
  const target = triangleAngle.value
  if (target == null) return undefined
  for (const range of fixedRanges.value) {
    if (target < range.start) break
    if (target <= range.end) return range.hue
  }
  return target
})

// === CSS ===

const cssGradient = computed(() => {
  const points = gradientPoints(
    fixedRanges.value,
    browserSupportsOklchInterpolation ? undefined : NONNATIVE_OKLCH_INTERPOLATION_STEPS,
  )
  const angularColorStopList = Array.from(points, cssAngularColorStop)
  const colorStops = angularColorStopList.join(',')
  return browserSupportsOklchInterpolation ?
      `conic-gradient(in oklch increasing hue,${colorStops})`
    : `conic-gradient(${colorStops})`
})
const cssTriangleAngle = computed(() =>
  triangleAngle.value != null ? `${triangleAngle.value}turn` : undefined,
)
const cssTriangleColor = computed(() =>
  triangleHue.value != null ? cssColor(triangleHue.value) : undefined,
)
</script>

<template>
  <div class="ColorRing">
    <svg v-if="cssTriangleAngle != null" class="svg" viewBox="-2 -2 4 4">
      <polygon class="triangle" points="0,-1 -0.4,-1.35 0.4,-1.35" />
    </svg>
    <div
      ref="svgElement"
      class="gradient"
      @pointerleave="mouseSelectedAngle = undefined"
      @pointermove="ringHover"
      @click.stop="ringClick"
    />
  </div>
</template>

<style scoped>
.ColorRing {
  position: relative;
  pointer-events: none;
  width: 100%;
  height: 100%;
}

.svg {
  position: absolute;
  margin: -50%;
}

.gradient {
  position: absolute;
  inset: 0;
  pointer-events: auto;
  margin-top: auto;
  background: v-bind('cssGradient');
  cursor: crosshair;
  border-radius: var(--radius-full);
  animation: grow 0.1s forwards;
}
@keyframes grow {
  from {
    transform: scale(0);
  }
  to {
    transform: scale(1);
  }
}

.triangle {
  transform: rotate(v-bind('cssTriangleAngle'));
  fill: v-bind('cssTriangleColor');
}
</style>
