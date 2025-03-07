/** @file Progress bar. */
import {
  ProgressBar as AriaProgressBar,
  type ProgressBarProps as AriaProgressBarProps,
} from '#/components/aria'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'

/** `1` as a percentage. */
const WHOLE_PERCENTAGE = 100

const PROGRESS_BAR_STYLES = tv({
  base: 'h-2 rounded-full bg-primary/10',
  slots: {
    progressBar: 'h-full overflow-clip rounded-full bg-accent transition-width duration-1000',
    indeterminateProgressBar: 'animate-horizontal-loader-1/6 h-full w-1/6 bg-white/30',
  },
})

/** Props for a {@link ProgressBar}. */
export interface ProgressBarProps
  extends Omit<AriaProgressBarProps, 'value'>,
    VariantProps<typeof PROGRESS_BAR_STYLES> {
  /** A number from 0 (not yet started, or just started) to 1 (about to complete, or completed). */
  readonly progress: number | 'indeterminate'
}

/** Progress bar. */
export function ProgressBar(props: ProgressBarProps) {
  const { progress, variants = PROGRESS_BAR_STYLES, ...rest } = props
  const progressNumber = progress === 'indeterminate' ? 1 : progress

  const styles = variants()

  return (
    <AriaProgressBar
      isIndeterminate={progress === 'indeterminate'}
      value={progressNumber}
      maxValue={1}
      {...rest}
    >
      {/* When indeterminate, the percentage is `undefined`, so a fallback must be provided. */}
      {({ percentage = WHOLE_PERCENTAGE }) => (
        <div className={styles.base()}>
          <div className={styles.progressBar()} style={{ width: percentage + '%' }}>
            {progress === 'indeterminate' && <div className={styles.indeterminateProgressBar()} />}
          </div>
        </div>
      )}
    </AriaProgressBar>
  )
}
