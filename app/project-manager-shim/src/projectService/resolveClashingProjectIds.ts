import * as crypto from 'node:crypto'

export interface ProjectWithClashingId {
  readonly id: string
  readonly directoryCreationTime?: string
}

/** Rewrite duplicate project IDs, keeping oldest project unchanged. */
export async function resolveClashingProjectIds<T extends ProjectWithClashingId>(
  projects: readonly T[],
  rewriteProjectId: (project: T, newId: string) => Promise<T>,
): Promise<T[]> {
  const idGroups = new Map<string, T[]>()

  for (const project of projects) {
    const group = idGroups.get(project.id) ?? []
    group.push(project)
    idGroups.set(project.id, group)
  }

  const result: T[] = []

  for (const group of idGroups.values()) {
    if (group.length === 1) {
      result.push(group[0]!)
      continue
    }

    group.sort((a, b) => {
      const timeA = a.directoryCreationTime ? new Date(a.directoryCreationTime).getTime() : 0
      const timeB = b.directoryCreationTime ? new Date(b.directoryCreationTime).getTime() : 0
      return timeA - timeB
    })

    result.push(group[0]!)

    for (let index = 1; index < group.length; index++) {
      result.push(await rewriteProjectId(group[index]!, crypto.randomUUID()))
    }
  }

  return result
}
