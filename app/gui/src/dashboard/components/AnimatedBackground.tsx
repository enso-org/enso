/**
 * @file
 *
 * `<AnimatedBackground />` component visually highlights selected items by sliding a background into view when hovered over or clicked.
 */
import type { Transition } from 'framer-motion'
import { AnimatePresence, motion } from 'framer-motion'
import type { PropsWithChildren } from 'react'
import { createContext, useContext, useId } from 'react'

import { twJoin } from '#/utilities/tailwindMerge'
import invariant from 'tiny-invariant'

/** Props for {@link AnimatedBackground}. */
interface AnimatedBackgroundProps extends PropsWithChildren {
  readonly value: string
  readonly transition?: Transition
}

const AnimatedBackgroundContext = createContext<{
  value: string | null
  transition: Transition
  layoutId: string
} | null>(null)

const DEFAULT_TRANSITION: Transition = {
  type: 'spring',
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  stiffness: 300,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  damping: 20,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  mass: 0.1,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  velocity: 12,
}

/** `<AnimatedBackground />` component visually highlights selected items by sliding a background into view when hovered over or clicked. */
export function AnimatedBackground(props: AnimatedBackgroundProps) {
  const { value, transition = DEFAULT_TRANSITION, children } = props
  const layoutId = useId()

  return (
    <AnimatedBackgroundContext.Provider value={{ value, transition, layoutId }}>
      {children}
    </AnimatedBackgroundContext.Provider>
  )
}

/** Props for {@link AnimatedBackground.Item}. */
interface AnimatedBackgroundItemProps extends PropsWithChildren {
  readonly value: string
  readonly className?: string
  readonly animationClassName?: string
}

/** Item within an {@link AnimatedBackground}. */
AnimatedBackground.Item = function AnimatedBackgroundItem(props: AnimatedBackgroundItemProps) {
  const context = useContext(AnimatedBackgroundContext)
  invariant(context, 'useAnimatedBackground must be used within an AnimatedBackgroundProvider')

  const { value, className, animationClassName, children } = props
  const { value: activeValue, transition, layoutId } = context

  return (
    <div className={twJoin('relative *:isolate', className)}>
      <AnimatePresence initial={false}>
        {activeValue === value && (
          <motion.div
            layoutId={`background-${layoutId}`}
            className={twJoin('absolute inset-0', animationClassName)}
            transition={transition}
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
            exit={{ opacity: 0 }}
          />
        )}
      </AnimatePresence>

      {children}
    </div>
  )
}
