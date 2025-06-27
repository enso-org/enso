import { EnsoPath, ProjectId } from '#/services/Backend'
import LocalBackend, { isLocalProjectId } from '#/services/LocalBackend'
import RemoteBackend from '#/services/RemoteBackend'
import { injectOpenedProjects } from '$/providers/openedProjects'
import { useProjectFiles } from '@/stores/projectFiles'
import { Err, Ok, Result } from '@/util/data/result'
import { ResourceContext } from '../asyncResources'
import { ResourceDefinition } from './AsyncResource'
import { parseResourceUrl } from './parse'

export type AsyncResourceResolver = ReturnType<typeof initAsyncResourceResolver>

/**
 * Create a resource resolver function that will use provided backends for accessing resource data.
 *
 * Part of 'asyncResources' store.
 * @internal
 */
export function initAsyncResourceResolver(
  backends: {
    localBackend: LocalBackend | undefined | null
    remoteBackend: RemoteBackend | undefined | null
  },
  openedProjects: ReturnType<typeof injectOpenedProjects>,
) {
  function resolveResourceInContext(
    unparsedAssetUrl: string,
    context: ResourceContext,
  ): Result<ResourceDefinition> {
    const parsedUrl = parseResourceUrl(unparsedAssetUrl, context.basePathSegments)
    console.log('parsedUrl', unparsedAssetUrl, '->', parsedUrl)
    if (!parsedUrl.ok) return Err(parsedUrl.error)
    switch (parsedUrl.value.kind) {
      case 'ensoPath': {
        return Ok(resolveEnsoPathResource(parsedUrl.value.ensoPath))
      }
      case 'webUrl': {
        return Ok(resolveWebResource(parsedUrl.value.url))
      }
      case 'projectRelative': {
        {
          const project = context.project()
          if (project == null) return Err('Cannot resolve relative path outside of project')
          return Ok(resolveProjectResource(project, parsedUrl.value.relativePath))
        }
      }
    }
  }

  function resolveEnsoPathResource(path: EnsoPath): ResourceDefinition {
    return {
      cacheKey: `ensoPath-${path}`,
      async fetch() {
        return Err('EnsoPath unimplemented')
      },
    }
  }

  function resolveWebResource(url: URL): ResourceDefinition {
    return {
      cacheKey: `web-${url}`,
      async fetch() {
        return Ok(url)
      },
    }
  }

  function resolveProjectResource(projectId: ProjectId, relativePath: string): ResourceDefinition {
    return {
      cacheKey: `projectRelative-${projectId}-${relativePath}`,
      async fetch(abort) {
        const openedProject = openedProjects.get(projectId)
        if (openedProject) {
          // Remote/local projects are treated the same when opened - contact LS for a file.
          const rootId = await openedProject.store.projectRootId
          if (rootId == null) return Err('Could not identify project root.')
          if (abort.aborted) return Err(abort)

          const projectFiles = useProjectFiles(openedProject.store)
          return projectFiles.readFileBinary({ rootId, segments: relativePath.split('/') }, abort)
        } else {
          // project not opened
          if (isLocalProjectId(projectId)) {
            // unopened local project
            const localBackend = backends.localBackend
            if (!localBackend) return Err('Cannot query local resource without local backend.')
            const data = await localBackend.resolveProjectAssetData(projectId, relativePath)
            if (abort.aborted) return Err(abort)
            return Ok(data)
          } else {
            // unopened remote project
            const cloudBackend = backends.remoteBackend
            if (cloudBackend == null)
              return Err('Cannot query cloud resource without cloud backend.')
            try {
              return Ok(await cloudBackend.resolveProjectAssetData(projectId, relativePath, abort))
            } catch (e) {
              return Err(e)
            }
          }
        }
      },
    }
  }

  return resolveResourceInContext
}
