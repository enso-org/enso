import { useTransitioning } from '@/composables/animation'
import { clamp } from 'enso-common/src/utilities/data/math'
import { computed, toValue, type WatchSource } from 'vue'

const STEP_DURATION = 'var(--progress-animation-step-duration, 0.2s)'
const FINAL_DURATION = `var(--progress-animation-final-duration, ${STEP_DURATION})`

function progressTransition(state: 'initial' | 'step' | 'final') {
  if (state === 'initial') return
  const duration = state === 'step' ? STEP_DURATION : FINAL_DURATION
  return { transition: `background-position ${duration} ease` }
}

function progressStyles(percent: number, state: 'initial' | 'step' | 'final') {
  return {
    'background-image':
      'linear-gradient(to right, var(--progress-background-initial) 50%, var(--progress-background-final) 50%)',
    'background-size': '200% 100%',
    'background-position': `${200 - percent}% 0`,
    ...(progressTransition(state) ?? {}),
  }
}

/**
 * Computes CSS styles that can be used to render the background of an element as a progress bar.
 */
export function useProgressBackground(
  progressPercent: WatchSource<number>,
  {
    counterId,
  }: {
    /**
     * Watch source that, if changed, indicates that animation should be restarted from 0 to the new
     * progress value. Values are only compared for distinctness. Previous values should not be
     * reused.
     */
    counterId?: WatchSource<number | undefined>
  } = {},
) {
  const normalizedPercent = computed(() => clamp(toValue(progressPercent), 0, 100))
  const currentCounter = computed(() => (counterId ? toValue(counterId) : 1))

  let currentAnimation: number | undefined = undefined

  function getProgressStyles() {
    const state =
      currentAnimation !== currentCounter.value ? 'initial'
      : normalizedPercent.value === 100 ? 'final'
      : 'step'
    if (state === 'initial') currentAnimation = currentCounter.value
    return progressStyles(normalizedPercent.value, state)
  }

  return {
    /**
     * The styles to apply to the progress bar background. The result should be applied to the
     * target element each time this is called, or else the animation will be incorrect.
     */
    getProgressStyles,
    watchProgress: () => {
      const { active: progressAnimating, events: backgroundProgressEvents } = useTransitioning(
        new Set(['background-position']),
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
