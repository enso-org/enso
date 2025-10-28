/** @file A contextual help component that displays a help message when the user hovers over an element. */
import type { AriaLabelingProps } from '@react-types/shared'
import type { PropsWithChildren } from 'react'
import type { OverlayTriggerProps } from 'react-stately'
import { Button } from '../Button'
import { Popover } from '../Dialog'
import type { Placement, SvgUseIcon, TestIdProps } from '../types'

/**
 * Props for the {@link ContextualHelp} component.
 */
export interface ContextualHelpProps
  extends TestIdProps,
    OverlayTriggerProps,
    AriaLabelingProps,
    PropsWithChildren {
  readonly placement: Placement
  readonly variant?: 'help' | 'info'
}

/**
 * A contextual help component that displays a help message when the user hovers over an element.
 */
export function ContextualHelp(props: ContextualHelpProps) {
  const { placement, testId, defaultOpen, isOpen, onOpenChange, variant = 'help', children } = props

  const icon: SvgUseIcon = variant === 'help' ? 'help' : 'metadata'

  return (
    <Popover.Trigger defaultOpen={defaultOpen} isOpen={isOpen} onOpenChange={onOpenChange}>
      <Button testId={testId} icon={icon} variant="icon" />

      <Popover placement={placement}>{children}</Popover>
    </Popover.Trigger>
  )
}
