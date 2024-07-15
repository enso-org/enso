/** @file Placeholder component for GUI used during e2e tests. */

import type * as types from 'enso-common/src/types'

/** Placeholder component for GUI used during e2e tests. */
export function TestAppRunner(props: types.EditorProps) {
  // eslint-disable-next-line no-restricted-syntax
  return props.hidden ? <></> : <div data-testid="gui-editor-root">Vue app loads here.</div>
}
