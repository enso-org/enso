/** @file A function to create a new project. */
import {
  backendMutationOptions,
  useDeleteAsset,
  useEnsureListDirectory,
} from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useOpenProjectLocally, useOpenProjectNatively } from '#/hooks/projectHooks'
import type { Category } from '#/layouts/Drive/CategorySwitcher'
import type Backend from '#/services/Backend'
import type { DirectoryId } from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import { useMutationCallback } from '#/utilities/tanstackQuery'

/** A function to create a new project. */
export function useNewProject(backend: Backend, category: Category) {
  const ensureListDirectory = useEnsureListDirectory(backend, category)
  const openProjectLocally = useOpenProjectLocally()
  const openProjectNatively = useOpenProjectNatively()
  const deleteAsset = useDeleteAsset(backend, category)

  const createProjectMutation = useMutationCallback(
    backendMutationOptions(backend, 'createProject'),
  )

  return useEventCallback(
    async (
      {
        templateName,
        templateId,
        ensoPath,
      }: {
        templateName: string | null | undefined
        templateId?: string | null | undefined
        ensoPath?: string | null | undefined
      },
      parentId: DirectoryId,
      runLocally = true,
    ) => {
      const siblings = await ensureListDirectory(parentId)
      const projectName = (() => {
        const prefix = `${templateName ?? 'New Project'} `
        const projectNameTemplate = new RegExp(`^${prefix}(?<projectIndex>\\d+)$`)
        const projectIndices = siblings
          .filter(backendModule.assetIsProject)
          .map((item) => projectNameTemplate.exec(item.title)?.groups?.projectIndex)
          .map((maybeIndex) => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
        return `${prefix}${Math.max(0, ...projectIndices) + 1}`
      })()

      const placeholderItem = backendModule.createPlaceholderProjectAsset(projectName, parentId)

      return await createProjectMutation([
        {
          parentDirectoryId: placeholderItem.parentId,
          projectName: placeholderItem.title,
          ...(templateId == null ? {} : { projectTemplateName: templateId }),
          ...(ensoPath == null ? {} : { ensoPath }),
        },
      ])
        .catch((error) => {
          void deleteAsset(placeholderItem.id, parentId)
          throw error
        })
        .then((createdProject) => {
          const openProjectParams = {
            id: createdProject.projectId,
            parentId: placeholderItem.parentId,
            title: createdProject.name,
            ...(createdProject.ensoPath != null ? { ensoPath: createdProject.ensoPath } : {}),
          } satisfies Partial<backendModule.ProjectAsset>
          if (runLocally) {
            // Open in background.
            void openProjectLocally(openProjectParams, backend.type)
          } else {
            void openProjectNatively(openProjectParams, backend.type)
          }

          return createdProject
        })
    },
  )
}
