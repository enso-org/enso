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
// This alias is fine; it is a literal union that currently happens to only have one member.
// eslint-disable-next-line no-restricted-syntax
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
      className={`size-3 rounded-full bg-not-selected transition-colors duration-200 ${ROLE_CLASSES[role]}`}
    />
  )
}
