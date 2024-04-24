import { useEvent } from '@/composables/events'
import { useProjectStore } from '@/stores/project'
import { useToast } from '@/util/toast'
import { ProjectManagerEvents } from '../../../../ide-desktop/lib/dashboard/src/services/ProjectManager'

export function useGraphEditorToasts() {
  const projectStore = useProjectStore()
  const toastStartup = useToast.info({ autoClose: false })
  const toastConnectionLost = useToast.error({ autoClose: false })
  const toastLspError = useToast.error()
  const toastExecutionFailed = useToast.error()

  toastStartup.show('Initializing the project. This can take up to one minute.')
  projectStore.firstExecution.then(toastStartup.dismiss)

  useEvent(document, ProjectManagerEvents.loadingFailed, () =>
    toastConnectionLost.show('Lost connection to Language Server.'),
  )

  projectStore.lsRpcConnection.client.onError((e) =>
    toastLspError.show(`Language server error: ${e}`),
  ),
    projectStore.executionContext.on('executionComplete', () => toastExecutionFailed.dismiss())
  projectStore.executionContext.on('executionFailed', (e) =>
    toastExecutionFailed.show(`Execution Failed: ${JSON.stringify(e)}`),
  )
}
