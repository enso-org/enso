/** @file Placeholder component for GUI used during e2e tests. */
import type * as editor from '#/layouts/Editor'

/** Placeholder component for GUI used during e2e tests. */
export function TestAppRunner(props: editor.GraphEditorProps) {
  // eslint-disable-next-line no-restricted-syntax
  return props.hidden ? <></> : <div data-testid="gui-editor-root">Vue app loads here.</div>
}
