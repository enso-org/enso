/** @file A type-safe wrapper around `react-router`'s `Navigate` component. */
import * as React from 'react'

import { Navigate as RouterNavigate, type NavigateProps as RouterNavigateProps } from 'react-router'

import type { AppFullPath } from '#/appUtils'

// ================
// === Navigate ===
// ================

/** Props for a {@link Navigate}. */
export interface NavigateProps extends Readonly<RouterNavigateProps> {
  readonly to: AppFullPath
}

/** A type-safe wrapper around `react-router`'s `Navigate` component. */
export default function Navigate(props: NavigateProps) {
  return <RouterNavigate {...props} />
}
