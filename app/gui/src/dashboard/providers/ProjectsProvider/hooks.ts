/** @file Hooks related to global projects state. */
import { useStore } from '#/hooks/storeHooks'
import type { ProjectId } from '#/services/Backend'
import { createStore } from 'zustand'

export const projectsStore = createStore<{
  readonly openingProjects: ReadonlySet<ProjectId>
  readonly addOpeningProject: (id: ProjectId) => void
  readonly removeOpeningProject: (id: ProjectId) => void
}>()((set) => ({
  openingProjects: new Set(),
  addOpeningProject: (id) => {
    set(({ openingProjects }) => ({
      openingProjects: new Set([...openingProjects, id]),
    }))
  },
  removeOpeningProject: (id) => {
    set(({ openingProjects }) => ({
      openingProjects: new Set([...openingProjects].filter((otherId) => otherId !== id)),
    }))
  },
}))

/** Return a function to add a project to the 'opening' list. */
export function useAddOpeningProject() {
  return useStore(projectsStore, ({ addOpeningProject }) => addOpeningProject)
}

/** Return a function to remove a project from the 'opening' list. */
export function useRemoveOpeningProject() {
  return useStore(projectsStore, ({ removeOpeningProject }) => removeOpeningProject)
}
