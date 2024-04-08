/** @file The React provider for the {@link ProjectManager} API wrapper, along with a hook to use
 * the provider via the shared React context. */
import * as React from 'react'

import type ProjectManager from '#/services/ProjectManager'

// =============================
// === ProjectManagerContext ===
// =============================

const ProjectManagerContext = React.createContext<ProjectManager | null>(null)

/** Props for a {@link ProjectManagerProvider}. */
export interface ProjectManagerProviderProps extends Readonly<React.PropsWithChildren> {
  readonly projectManager: ProjectManager | null
}

// ==============================
// === ProjectManagerProvider ===
// ==============================

/** A React provider (and associated hooks) for determining whether the current focus contex
 * is vertical or horizontal. */
export default function ProjectManagerProvider(props: ProjectManagerProviderProps) {
  const { projectManager, children } = props
  return (
    <ProjectManagerContext.Provider value={projectManager}>
      {children}
    </ProjectManagerContext.Provider>
  )
}

/** The current direction in which focus siblings are located. */
export function useProjectManager() {
  return React.useContext(ProjectManagerContext)
}
