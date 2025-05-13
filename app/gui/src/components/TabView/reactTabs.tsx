import { Suspense } from '#/components/Suspense'
import type { DriveProps } from '#/layouts/Drive'
import { EditorProps } from '#/layouts/Editor'
import * as react from 'react'
import { applyPureReactInVue } from 'veaury'

const ReactDrive = react.lazy(() => import('#/layouts/Drive'))
const ReactEditor = react.lazy(() => import('#/layouts/Editor'))
const ReactSettings = react.lazy(() => import('#/layouts/Settings'))

function SuspendedDrive(props: DriveProps) {
  return (
    <Suspense>
      <ReactDrive {...props} />
    </Suspense>
  )
}
function SuspendedEditor(props: EditorProps) {
  return (
    <Suspense>
      <ReactEditor {...props} />
    </Suspense>
  )
}
function SuspendedSettings() {
  return (
    <Suspense>
      <ReactSettings />
    </Suspense>
  )
}

export const Drive = applyPureReactInVue(SuspendedDrive)
export const Editor = applyPureReactInVue(SuspendedEditor)
export const Settings = applyPureReactInVue(SuspendedSettings)
