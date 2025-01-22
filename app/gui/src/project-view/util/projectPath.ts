import {
  qnJoin,
  qnSplit,
  type IdentifierOrOperatorIdentifier,
  type QualifiedName,
} from '@/util/qualifiedName'
import { assert, assertDefined } from 'ydoc-shared/util/assert'

export type ProjectName = QualifiedName

/** Parses the qualified name as a literal project path. */
export function parseAbsoluteProjectPath(path: QualifiedName): ProjectPath {
  const parts = /^([^.]+\.[^.]+)(?:\.(.+))?$/.exec(path)
  assert(parts != null)
  assertDefined(parts[1])
  return ProjectPath.create(
    parts[1] as QualifiedName,
    parts[2] ? (parts[2] as QualifiedName) : undefined,
  )
}

/** Prints a literal project path. */
export function printAbsoluteProjectPath(path: AbsoluteProjectPath): QualifiedName {
  return path.path ? qnJoin(path.project, path.path) : path.project
}

/**
 * Represents a qualified name (e.g. an import path or a definition path) as a project identity, and a path within it.
 */
export class ProjectPath {
  private constructor(
    /** `undefined` identifies the current project; otherwise, this will be a two-segment path. */
    readonly project: QualifiedName | undefined,
    /** `undefined` indicates that the project's path is the entire path. */
    readonly path: QualifiedName | undefined,
  ) {}

  /** Construct a literal project path from the given project name and path within it. */
  static create(project: QualifiedName, path: QualifiedName | undefined): AbsoluteProjectPath
  /** Construct a project path from the given project name and path within it. */
  static create(project: QualifiedName | undefined, path: QualifiedName | undefined): ProjectPath
  /** Construct a project path from the given project name and path within it. */
  static create(project: QualifiedName | undefined, path: QualifiedName | undefined): ProjectPath {
    return new ProjectPath(project, path)
  }

  /** @returns a new path within the same project. */
  withPath(path: QualifiedName | undefined): ProjectPath {
    return new ProjectPath(this.project, path)
  }

  /** Checks for equality */
  equals(b: ProjectPath): boolean {
    return this.path === b.path && this.project === b.project
  }

  /** Returns the path with the given qualified name appended */
  append(append: QualifiedName): ProjectPath {
    return this.withPath(this.path ? qnJoin(this.path, append) : append)
  }

  /** Return the path without the last identifier, and the identifier */
  splitAtName(): [ProjectPath, IdentifierOrOperatorIdentifier] | undefined {
    if (!this.path) return
    const [parent, ident] = qnSplit(this.path)
    return [this.withPath(parent || undefined), ident]
  }

  /** Removes the `Main` segment representing the top level module of the project, if present, and returns the result. */
  normalized(): ProjectPath {
    if (!this.path) return this
    const normalized = this.path.match(/^Main(?:\.(.+))?$/)
    if (normalized) {
      return this.withPath(normalized[1] ? (normalized[1] as QualifiedName) : undefined)
    } else {
      return this
    }
  }

  /**
   * Checks if given full qualified name is considered a top element of some project.
   *
   * The fully qualified names consists of namespace, project name, and then a path (possibly empty).
   * The element is considered a top element if there is max 1 segment in the path.
   */
  isTopElement(): boolean {
    return !this.path || !this.path.includes('.')
  }

  /** Convert to a plain, JSON-compatible object; this is useful for testing. */
  toJSON(): object {
    return { project: this.project || null, path: this.path || null }
  }
}

/** A project path with a literal project name. */
export interface AbsoluteProjectPath extends ProjectPath {
  readonly project: QualifiedName
}
