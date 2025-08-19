/** @file Step component. */
import DoneIcon from '#/assets/check_mark.svg'
import SvgMask from '#/components/SvgMask'
import { Text } from '#/components/Text'
import { tv } from '#/utilities/tailwindVariants'
import * as React from 'react'
import type { RenderStepProps } from './types'

/** A prop with the given type, or a function to produce a value of the given type. */
type StepProp<T> = T | ((props: RenderStepProps) => T)

/** Props for {@link Step} component. */
export interface StepProps extends RenderStepProps {
  readonly className?: StepProp<string | null | undefined>
  readonly icon?: StepProp<React.ReactElement | string | null | undefined>
  readonly completeIcon?: StepProp<React.ReactElement | string | null | undefined>
  readonly title?: StepProp<React.ReactElement | string | null | undefined>
  readonly description?: StepProp<React.ReactElement | string | null | undefined>
  readonly children?: StepProp<React.ReactNode>
}

const STEP_STYLES = tv({
  base: 'relative flex items-center gap-2 select-none',
  slots: {
    icon: 'w-6 h-6 border-0.5 flex-none border-current rounded-full flex items-center justify-center transition-colors duration-200',
    titleContainer: '-mt-1 flex flex-col items-start justify-start transition-colors duration-200',
    content: 'flex-1',
  },
  variants: {
    position: { first: 'rounded-l-full', last: 'rounded-r-full' },
    status: {
      completed: {
        base: 'text-primary',
        icon: 'bg-primary border-transparent text-invert',
        content: 'text-primary',
      },
      current: { base: 'text-primary', content: 'text-primary/30' },
      next: { base: 'text-primary/30', content: 'text-primary/30' },
    },
  },
})

/** A step component is used to represent a single step in a stepper component. */
export function Step(props: StepProps) {
  const {
    index,
    title,
    description,
    isCompleted,
    goToStep,
    nextStep,
    previousStep,
    totalSteps,
    currentStep,
    isCurrent,
    isLast,
    isFirst,
    isDisabled,
    className,
    children,
    icon = (
      <Text variant="subtitle" color="current" aria-hidden>
        {index + 1}
      </Text>
    ),
    completeIcon = DoneIcon,
  } = props

  const renderStepProps = {
    isCompleted,
    goToStep,
    nextStep,
    previousStep,
    totalSteps,
    currentStep,
    isCurrent,
    isLast,
    isFirst,
    isDisabled,
    index,
  } satisfies RenderStepProps

  const classes = typeof className === 'function' ? className(renderStepProps) : className
  const descriptionElement =
    typeof description === 'function' ? description(renderStepProps) : description
  const titleElement = typeof title === 'function' ? title(renderStepProps) : title
  const iconElement = typeof icon === 'function' ? icon(renderStepProps) : icon
  const doneIconElement =
    typeof completeIcon === 'function' ? completeIcon(renderStepProps) : completeIcon

  const styles = STEP_STYLES({
    className: classes,
    position:
      isFirst ? 'first'
      : isLast ? 'last'
      : undefined,
    status:
      isCompleted ? 'completed'
      : isCurrent ? 'current'
      : 'next',
  })

  return (
    <div className={styles.base()}>
      <div key={isCompleted ? 'done' : 'icon'} className={styles.icon()}>
        {(() => {
          const renderIconElement = isCompleted ? doneIconElement : iconElement

          if (renderIconElement == null) {
            return null
          } else if (typeof renderIconElement === 'string') {
            return <SvgMask src={renderIconElement} />
          } else {
            return renderIconElement
          }
        })()}
      </div>

      <div className={styles.titleContainer()}>
        {titleElement != null && (
          <div>
            {typeof titleElement === 'string' ?
              <Text nowrap color="current">
                {titleElement}
              </Text>
            : titleElement}
          </div>
        )}

        {descriptionElement != null && (
          <div>
            {typeof descriptionElement === 'string' ?
              <Text variant="body" color="current" truncate="2">
                {descriptionElement}
              </Text>
            : descriptionElement}
          </div>
        )}
      </div>
      <div className={styles.content()}>
        {typeof children === 'function' ? children(renderStepProps) : children}
      </div>
    </div>
  )
}
