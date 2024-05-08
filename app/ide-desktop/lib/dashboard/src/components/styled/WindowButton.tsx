/** @file A button for performing an action on a window. */
import * as React from 'react'

import UnstyledButton from '#/components/UnstyledButton'

// =================
// === Constants ===
// =================

const ROLE_CLASSES: Readonly<Record<WindowButtonRole, string>> = {
  close: 'hover:bg-window-close',
}

// ========================
// === WindowButtonRole ===
// ========================

/** The role of a {@link WindowButton}. */
export type WindowButtonRole = 'close'

// ====================
// === WindowButton ===
// ====================

/** Props for a {@link WindowButton}. */
export interface WindowButtonProps {
  readonly onPress: () => void
  readonly role: WindowButtonRole
}

/** A button for performing an action on a window. */
export default function WindowButton(props: WindowButtonProps) {
  const { onPress, role } = props
  return (
    <UnstyledButton
      onPress={onPress}
      className={`bg-not-selected size-3 rounded-full transition-colors duration-200 ${ROLE_CLASSES[role]}`}
    />
  )
}
