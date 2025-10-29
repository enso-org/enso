import { clamp } from '$/utils/data/math'
import { useTransitionalFrame, useTransitioning } from '@/composables/animation'
import { computed, toValue, type WatchSource } from 'vue'

// === Constants ===

const INITIAL_COLOR_DEFAULT = 'var(--progress-background-initial)'
const FINAL_COLOR_DEFAULT = 'var(--progress-background-final)'
const STEP_DURATION = 'var(--progress-animation-step-duration, 0.2s)'
const FINAL_DURATION = `var(--progress-animation-final-duration, ${STEP_DURATION})`

// === Pure-function utilities ===

const STEP_TRANSITION = `background-position-x ${STEP_DURATION} ease`
const FINAL_TRANSITION = `background-position-x ${FINAL_DURATION} ease`
function progressTransition(state: 'initial' | 'step' | 'final') {
  switch (state) {
    case 'initial':
      return
    case 'step':
      return STEP_TRANSITION
    case 'final':
      return FINAL_TRANSITION
  }
}

/** @internal */
export function backgroundPositionForProgress(percent: number): string {
  return `${200 - percent}%`
}

// === API ===

/**
 * Computes CSS styles that can be used to render the background of an element as a progress bar.
 */
export function useProgressBackground(
  progressPercent: WatchSource<number>,
  {
    progressId,
    initialColor,
    finalColor,
  }: {
    /**
     * Watch source that, if changed, indicates that animation should be restarted from 0 to the new
     * progress value. The values must satisfy the constraints of {@link useTransitionalFrame}; a
     * monotonic counter is suitable.
     */
    progressId?: WatchSource<number> | undefined
    /**
     * The color of the part of the progress bar below the current progress.
     *
     * Defaults to {@link INITIAL_COLOR_DEFAULT}. This can be overridden to enable referencing a
     * property with an animation.
     */
    initialColor?: string
    /**
     * The color of the part of the progress bar above the current progress.
     *
     * Defaults to {@link FINAL_COLOR_DEFAULT}. This can be overridden to enable referencing a
     * property with an animation.
     */
    finalColor?: string
  } = {},
) {
  const gradient = `linear-gradient(to right, ${initialColor ?? INITIAL_COLOR_DEFAULT} 50%, ${finalColor ?? FINAL_COLOR_DEFAULT} 50%)`
  function makeProgressStyles(percent: number, state: 'initial' | 'step' | 'final') {
    const backgroundPositionTransition = progressTransition(state)
    return {
      'background-image': gradient,
      'background-size': '200% 100%',
      'background-position-x': backgroundPositionForProgress(percent),
      ...(backgroundPositionTransition ? { transition: backgroundPositionTransition } : {}),
    }
  }

  const { isTransitionalFrame } = useTransitionalFrame(progressId)
  const normalizedPercent = computed(() => clamp(toValue(progressPercent), 0, 100))
  const progressStyles = computed(() => {
    return isTransitionalFrame.value ?
        makeProgressStyles(0, 'initial')
      : makeProgressStyles(
          normalizedPercent.value,
          normalizedPercent.value === 100 ? 'final' : 'step',
        )
  })

  return {
    /** The styles to apply to the progress bar background. */
    progressStyles,
    watchProgress: () => {
      const { active: progressAnimating, events: backgroundProgressEvents } = useTransitioning(
        new Set(['background-position-x']),
      )
      return {
        /**
         * Whether the progress bar is currently animating. To use this, the
         * `backgroundProgressEvents` listeners must be attached.
         */
        progressAnimating,
        /** Event listeners to attach to the progress bar element to track progress animation. */
        backgroundProgressEvents,
      }
    },
  }
}
