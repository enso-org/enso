import { createContextStore } from '@/providers'
import type { ToValue } from '@/util/reactivity'
import type Backend from 'enso-common/src/services/Backend'
import { proxyRefs, toRef } from 'vue'

export const [provideBackend, injectBackend] = createContextStore(
  'backend',
  ({ project, remote }: { project: ToValue<Backend | null>; remote: ToValue<Backend | null> }) =>
    proxyRefs({
      project: toRef(project),
      remote: toRef(remote),
    }),
)
