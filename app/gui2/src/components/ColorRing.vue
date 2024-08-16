<script setup lang="ts">
import {
  cssAngularColorStop,
  gradientPoints,
  rangesForInputs,
} from '@/components/ColorRing/gradient'
import { injectInteractionHandler } from '@/providers/interactionHandler'
import { endOnClickOutside } from '@/util/autoBlur'
import { cssSupported, ensoColor, formatCssColor, normalizeHue, parseCssColor } from '@/util/colors'
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
  /** Angle, measured in degrees from the positive Y-axis, where the initially-selected color should be placed. */
  initialColorAngle?: number
}>()
const emit = defineEmits<{
  close: []
}>()

const initialColor = selectedColor.value
const selectedHue = computed(() => {
  if (!selectedColor.value) return undefined
  const color = parseCssColor(selectedColor.value)
  if (color?.h != null) return color.h / 360
  return undefined
})
const rotation: number = (selectedHue.value ?? 0) - (props.initialColorAngle ?? 0) / 360
function hueToAngle(hue: number): number {
  return normalizeHue(hue - rotation)
}
function angleToHue(angle: number): number {
  return normalizeHue(angle + rotation)
}

const browserSupportsOklchInterpolation = cssSupported(
  'background-image: conic-gradient(in oklch increasing hue, red, blue)',
)

const svgElement = ref<HTMLElement>()

const interaction = injectInteractionHandler()

onMounted(() => {
  interaction.setCurrent(
    endOnClickOutside([svgElement], {
      cancel: () => {
        selectedColor.value = initialColor
        emit('close')
      },
      end: () => emit('close'),
    }),
  )
})

const mouseAngle = ref<number>()

const triangleAngle = computed(() =>
  mouseAngle.value != null ? mouseAngle.value
  : selectedHue.value != null ? hueToAngle(selectedHue.value)
  : undefined,
)

function cssColor(hue: number) {
  return formatCssColor(ensoColor(hue))
}

// === Events ===

function eventAngle(event: MouseEvent) {
  if (!svgElement.value) return 0
  const origin = Rect.FromDomRect(svgElement.value.getBoundingClientRect()).center()
  const offset = Vec2.FromXY(event).sub(origin)
  return normalizeHue(Math.atan2(offset.y, offset.x) / (2 * Math.PI) + 0.25)
}

function setColorForEvent(event: MouseEvent) {
  mouseAngle.value = eventAngle(event)
  if (triangleStyle.value?.fill !== selectedColor.value)
    selectedColor.value = triangleStyle.value?.fill
}
function ringClick(event: MouseEvent) {
  setColorForEvent(event)
  emit('close')
}

// === Gradient colors ===

const fixedRanges = computed(() => {
  const inputHues = new Set<number>()
  for (const rawColor of props.matchableColors) {
    const color = parseCssColor(rawColor)
    const hueDeg = color?.h
    if (hueDeg == null) continue
    inputHues.add(hueToAngle(hueDeg / 360))
  }
  return rangesForInputs(inputHues, FIXED_RANGE_WIDTH / 2)
})

function snapAngle(angle: number) {
  for (const range of fixedRanges.value) {
    if (angle < range.start) break
    if (angle <= range.end) return range.hue
  }
  return angle
}

// === CSS ===

const cssGradient = computed(() => {
  const points = gradientPoints(
    fixedRanges.value,
    browserSupportsOklchInterpolation ? undefined : NONNATIVE_OKLCH_INTERPOLATION_STEPS,
  )
  const angularColorStopList = points.map((point) =>
    cssAngularColorStop({ ...point, hue: angleToHue(point.hue) }),
  )
  const colorStops = angularColorStopList.join(',')
  return browserSupportsOklchInterpolation ?
      `conic-gradient(in oklch increasing hue,${colorStops})`
    : `conic-gradient(${colorStops})`
})

const triangleStyle = computed(() =>
  triangleAngle.value != null ?
    {
      transform: `rotate(${triangleAngle.value}turn)`,
      fill: cssColor(angleToHue(snapAngle(triangleAngle.value))),
    }
  : undefined,
)
</script>

<template>
  <div class="ColorRing">
    <svg v-if="triangleStyle != null" class="svg" viewBox="-2 -2 4 4">
      <polygon :style="triangleStyle" points="0,-1 -0.4,-1.35 0.4,-1.35" />
    </svg>
    <div
      ref="svgElement"
      class="gradient"
      :style="{ background: cssGradient }"
      @pointerleave="mouseAngle = undefined"
      @pointermove="setColorForEvent"
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
</style>
