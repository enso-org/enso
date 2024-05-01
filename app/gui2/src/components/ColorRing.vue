<script setup lang="ts">
import { injectInteractionHandler } from '@/providers/interactionHandler'
import { targetIsOutside } from '@/util/autoBlur'
import { browserSupportsOklch, ensoColor, formatCssColor, parseCssColor } from '@/util/colors'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { Resumable } from 'shared/util/data/iterable'
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

// If the browser doesn't support OKLCH, the gradient will be specified by computing the number of points specified here
// in OKLCH, converting to sRGB, and interpolating in HSL. This number has been found to be enough to look close to the
// intended colors, without excessive gradient complexity (which may affect performance).
const NONNATIVE_OKLCH_INTERPOLATION_STEPS = 300

const selectedColor = defineModel<string | undefined>()
const props = defineProps<{
  matchableColors: Set<string>
}>()
const emit = defineEmits<{
  close: []
}>()

const svgElement = ref<HTMLElement>()

const interaction = injectInteractionHandler()

onMounted(() => {
  interaction.setCurrent({
    cancel: () => emit('close'),
    pointerdown: (e: PointerEvent) => {
      if (targetIsOutside(e, svgElement.value)) emit('close')
      return false
    },
  })
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

interface FixedRange {
  start: number
  end: number
  hue: number
  meetsPreviousRange: boolean
  meetsNextRange: boolean
}
const FIXED_RANGE_WIDTH = 1 / 16

function rangesForInputs(inputHues: number[], radius: number) {
  const inputs = new Array<number>()
  const firstInput = inputHues[0]
  const lastInput = inputHues[inputHues.length - 1]
  if (lastInput != null && lastInput + radius > 1) inputs.push(lastInput - 1)
  inputs.push(...inputHues)
  if (firstInput != null && firstInput < radius) inputs.push(firstInput + 1)
  const ranges = new Array<FixedRange>()
  for (const hue of inputs) {
    const preferredStart = Math.max(hue - radius, 0)
    const preferredEnd = Math.min(hue + radius, 1)
    const prev = ranges[ranges.length - 1]
    if (prev && preferredStart < prev.end) {
      const midpoint = (hue + prev.hue) / 2
      prev.end = midpoint
      prev.meetsNextRange = true
      ranges.push({
        start: midpoint,
        end: preferredEnd,
        hue,
        meetsPreviousRange: true,
        meetsNextRange: false,
      })
    } else {
      const meetsPreviousRange = prev !== undefined && preferredStart < prev.end
      if (meetsPreviousRange) prev.meetsNextRange = true
      ranges.push({
        start: preferredStart,
        end: preferredEnd,
        hue,
        meetsPreviousRange,
        meetsNextRange: false,
      })
    }
  }
  return ranges
}

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
  return rangesForInputs([...inputHues].sort(), FIXED_RANGE_WIDTH / 2)
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
  const numStops = browserSupportsOklch ? 2 : NONNATIVE_OKLCH_INTERPOLATION_STEPS
  const fixedRangeIter = new Resumable(fixedRanges.value)
  const stops = new Set<number>()
  for (let i = 0; i < numStops; i++) {
    const angle = i / numStops
    fixedRangeIter.advanceWhile((range) => range.end <= angle)
    const nextFixedRange = fixedRangeIter.peek()
    if (!nextFixedRange || nextFixedRange.start > angle) stops.add(angle)
  }
  const clauses = new Array<{ start: number; clause: string }>()
  const interpolationColorspace = browserSupportsOklch ? 'oklch' : 'hsl'
  clauses.push({ start: -1, clause: `in ${interpolationColorspace} increasing hue` })
  const interpolationPoint = (angle: number) => [cssColor(angle), `${angle}turn`].join(' ')
  for (const stop of stops) clauses.push({ start: stop, clause: interpolationPoint(stop) })
  for (const { start, end, hue, meetsPreviousRange, meetsNextRange } of fixedRanges.value) {
    const color = cssColor(hue)
    if (!meetsPreviousRange) clauses.push({ start, clause: interpolationPoint(start) })
    clauses.push({ start, clause: `${color} ${start}turn ${end}turn` })
    if (!meetsNextRange) clauses.push({ start: end, clause: interpolationPoint(end) })
  }
  clauses.sort((a, b) => a.start - b.start)
  const sortedClauses = clauses.map((clause) => clause.clause)
  sortedClauses.push(cssColor(0))
  return `conic-gradient(${sortedClauses.join(',')})`
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
      @pointerdown.stop
      @pointerup.stop
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
