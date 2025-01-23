/**
 * @file
 * Visually highlight selected items by sliding a background into view when hovered over or clicked.
 */
import type { Transition, Variants } from 'framer-motion'
import { AnimatePresence, motion } from 'framer-motion'
import type { PropsWithChildren } from 'react'
import { createContext, memo, useContext, useId, useMemo } from 'react'

import { twJoin } from '#/utilities/tailwindMerge'
import invariant from 'tiny-invariant'

/** Props for {@link AnimatedBackground}. */
interface AnimatedBackgroundProps extends PropsWithChildren {
  /**
   * Active value.
   * You can omit this prop if you want to use the `isSelected` prop on {@link AnimatedBackground.Item}.
   */
  readonly value?: string
  readonly transition?: Transition
}

const AnimatedBackgroundContext = createContext<{
  value: string | undefined
  transition: Transition
  layoutId: string
} | null>(null)

/* eslint-disable @typescript-eslint/no-magic-numbers */
const DEFAULT_TRANSITION: Transition = {
  type: 'spring',
  stiffness: 350,
  damping: 20,
  mass: 0.3,
  velocity: 8,
}

/* eslint-enable @typescript-eslint/no-magic-numbers */

/** `<AnimatedBackground />` component visually highlights selected items by sliding a background into view when hovered over or clicked. */
export function AnimatedBackground(props: AnimatedBackgroundProps) {
  const { value, transition = DEFAULT_TRANSITION, children } = props

  const layoutId = useId()

  const contextValue = useMemo(
    () => ({ value, transition, layoutId }),
    [value, transition, layoutId],
  )

  return (
    <AnimatedBackgroundContext.Provider value={contextValue}>
      {children}
    </AnimatedBackgroundContext.Provider>
  )
}

/** Props for {@link AnimatedBackground.Item}. */
type AnimatedBackgroundItemProps = PropsWithChildren<
  AnimatedBackgroundItemPropsWithSelected | AnimatedBackgroundItemPropsWithValue
> & {
  readonly className?: string
  readonly animationClassName?: string
  readonly underlayElement?: React.ReactNode
}

/** Props for {@link AnimatedBackground.Item} with a `value` prop. */
interface AnimatedBackgroundItemPropsWithValue {
  readonly value: string
  readonly isSelected?: never
}

/** Props for {@link AnimatedBackground.Item} with a `isSelected` prop. */
interface AnimatedBackgroundItemPropsWithSelected {
  readonly isSelected: boolean
  readonly value?: never
}

/** Item within an {@link AnimatedBackground}. */
AnimatedBackground.Item = memo(function AnimatedBackgroundItem(props: AnimatedBackgroundItemProps) {
  const {
    value,
    className,
    animationClassName,
    children,
    isSelected,
    underlayElement: rawUnderlayElement,
  } = props

  const defaultUnderlayElement = useMemo(
    () => <div className={twJoin('h-full w-full', animationClassName)} />,
    [animationClassName],
  )

  const underlayElement = rawUnderlayElement ?? defaultUnderlayElement

  const context = useContext(AnimatedBackgroundContext)
  invariant(context, '<AnimatedBackground.Item /> must be placed within an <AnimatedBackground />')
  const { value: activeValue, transition, layoutId } = context

  invariant(
    activeValue === undefined || isSelected === undefined,
    'isSelected shall be passed either directly or via context by matching the value prop in <AnimatedBackground.Item /> and value from <AnimatedBackground />',
  )

  const isActive = isSelected ?? activeValue === value

  return (
    <div className={twJoin('relative *:isolate', className)}>
      <AnimatedBackgroundItemUnderlay
        isActive={isActive}
        underlayElement={underlayElement}
        layoutId={layoutId}
        transition={transition}
      />

      <div className="isolate contents *:isolate">{children}</div>
    </div>
  )
})

/** Props for {@link AnimatedBackgroundItemUnderlay}. */
interface AnimatedBackgroundItemUnderlayProps {
  readonly isActive: boolean
  readonly underlayElement: React.ReactNode
  readonly layoutId: string
  readonly transition: Transition
}

const VARIANTS: Variants = {
  hidden: { opacity: 0 },
  visible: { opacity: 1 },
}

/** Underlay for {@link AnimatedBackground.Item}. */
// eslint-disable-next-line no-restricted-syntax
const AnimatedBackgroundItemUnderlay = memo(function AnimatedBackgroundItemUnderlay(
  props: AnimatedBackgroundItemUnderlayProps,
) {
  const { isActive, underlayElement, layoutId, transition } = props

  return (
    <AnimatePresence initial={!isActive}>
      {isActive && (
        <motion.div
          layout
          layoutId={`background-${layoutId}`}
          className="pointer-events-none absolute inset-0 isolate"
          transition={transition}
          variants={VARIANTS}
          initial="hidden"
          animate="visible"
          exit="hidden"
        >
          {underlayElement}
        </motion.div>
      )}
    </AnimatePresence>
  )
})
