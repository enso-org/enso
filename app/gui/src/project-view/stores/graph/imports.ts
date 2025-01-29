import { type ProjectNameStore } from '@/stores/projectNames'
import { SuggestionDb } from '@/stores/suggestionDatabase'
import { SuggestionKind, type SuggestionEntry } from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import {
  astToQualifiedName,
  parseIdents,
  type Identifier,
  type IdentifierOrOperatorIdentifier,
} from '@/util/ast/abstract'
import { type ProjectPath } from '@/util/projectPath'
import {
  normalizeQualifiedName,
  qnLastSegment,
  qnSegments,
  type QualifiedName,
} from '@/util/qualifiedName'

// ========================
// === Imports analysis ===
// ========================

/** Read imports from given module block */
export function* readImports(ast: Ast.BodyBlock): Generator<RawImport> {
  for (const stmt of ast.statements()) {
    if (stmt instanceof Ast.Import) {
      const recognized = recognizeImport(stmt)
      if (recognized) yield recognized
    }
  }
}

/** Parse import statement. */
export function recognizeImport(ast: Ast.Import): RawImport | null {
  const moduleAst = ast.from ?? ast.import_
  const module = moduleAst ? astToQualifiedName(moduleAst) : null
  if (!module) return null
  if (ast.all) {
    const except = (ast.hiding != null ? parseIdents(ast.hiding) : []) ?? []
    return {
      from: module,
      imported: { kind: 'All', except },
    }
  } else if (ast.from && ast.import_) {
    const names = parseIdents(ast.import_) ?? []
    return {
      from: module,
      imported: { kind: 'List', names },
    }
  } else if (ast.import_) {
    const alias = ast.as instanceof Ast.Ident ? ast.as.code() : null
    return {
      from: module,
      imported: alias ? { kind: 'Module', alias } : { kind: 'Module' },
    }
  } else {
    console.error('Unrecognized import', ast.code())
    return null
  }
}

/** Read the imports and transform their literal project paths to logical project paths. */
export function* analyzeImports(
  ast: Ast.BodyBlock,
  projectNames: ProjectNameStore,
): Generator<AbstractImport> {
  for (const { from, imported } of readImports(ast)) {
    const parsed = projectNames.parseProjectPath(normalizeQualifiedName(from))
    if (parsed.ok) {
      yield {
        from: parsed.value,
        imported,
      }
    }
  }
}

/** Information about parsed import statement. */
export interface Import<ModulePath = QualifiedName> {
  from: ModulePath
  imported: ImportedNames
}

export type RawImport = Import<QualifiedName>
export type AbstractImport = Import<ProjectPath>

export type ImportedNames = Module | List | All

/** import Module.Path (as Alias)? */
export interface Module {
  kind: 'Module'
  alias?: Identifier
}

/** from Module.Path import (Ident),+ */
export interface List {
  kind: 'List'
  names: Identifier[]
}

/** from Module.Path import all (hiding (Ident),*)? */
export interface All {
  kind: 'All'
  except: Identifier[]
}

// ========================
// === Required imports ===
// ========================

/** Import required for the suggestion entry. */
export type RequiredImport = QualifiedImport | UnqualifiedImport

/** import Module.Path */
export interface QualifiedImport {
  kind: 'Qualified'
  module: ProjectPath
}

/** from Module.Path import SomeIdentifier */
export interface UnqualifiedImport {
  kind: 'Unqualified'
  from: ProjectPath
  import: Identifier
}

/** Insert the given imports into the given block at an appropriate location. */
export function addImports(
  scope: Ast.MutableBodyBlock,
  importsToAdd: RequiredImport[],
  projectNames: ProjectNameStore,
) {
  const imports = importsToAdd.map((info) => requiredImportToAst(info, projectNames, scope.module))
  const position = newImportsLocation(scope)
  scope.insert(position, ...imports)
}

/**
 * Return a suitable location in the given block to insert an import statement.
 *
 *  The location chosen will be before the first non-import line, and after all preexisting imports.
 *  If there are any blank lines in that range, it will be before them.
 */
function newImportsLocation(scope: Ast.BodyBlock): number {
  let lastImport
  const lines = scope.lines
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]!
    if (line.statement) {
      if (line.statement.node instanceof Ast.Import) {
        lastImport = i
      } else {
        break
      }
    }
  }
  return lastImport === undefined ? 0 : lastImport + 1
}

/**
 * Create an AST representing the required import statement.
 * @internal
 */
export function requiredImportToAst(
  value: RequiredImport,
  projectNames: ProjectNameStore,
  module?: Ast.MutableModule,
) {
  const module_ = module ?? Ast.MutableModule.Transient()
  switch (value.kind) {
    case 'Qualified':
      return Ast.Import.Qualified(qnSegments(projectNames.printProjectPath(value.module)), module_)!
    case 'Unqualified':
      return Ast.Import.Unqualified(
        qnSegments(projectNames.printProjectPath(value.from)),
        value.import,
        module_,
      )!
  }
}

/** A list of required imports for a specific suggestion entry. */
export function requiredImports(
  db: SuggestionDb,
  entry: SuggestionEntry,
  directConImport: boolean = false,
): RequiredImport[] {
  const unqualifiedImport = (from: ProjectPath): UnqualifiedImport[] => [
    {
      kind: 'Unqualified',
      from: from.normalized(),
      import: entry.name as Identifier,
    },
  ]
  switch (entry.kind) {
    case SuggestionKind.Module:
      return entry.reexportedIn ?
          unqualifiedImport(entry.reexportedIn)
        : [
            {
              kind: 'Qualified',
              module: entry.definedIn.normalized(),
            },
          ]
    case SuggestionKind.Type:
      return unqualifiedImport(entry.reexportedIn ?? entry.definedIn)
    case SuggestionKind.Constructor:
      if (directConImport) {
        return unqualifiedImport(entry.reexportedIn ?? entry.memberOf)
      } else {
        const selfType = selfTypeEntry(db, entry)
        return selfType ? requiredImports(db, selfType) : []
      }
    case SuggestionKind.Method: {
      const isStatic = entry.selfType == null
      const selfType = selfTypeEntry(db, entry)
      const isExtension = selfType && !selfType.definedIn.equals(entry.definedIn)
      const definedIn = definedInEntry(db, entry)
      const extensionImports = isExtension && definedIn ? requiredImports(db, definedIn) : []
      const selfTypeImports = isStatic && selfType ? requiredImports(db, selfType) : []
      if (isStatic) {
        return [...extensionImports, ...selfTypeImports]
      } else {
        return [...extensionImports]
      }
    }
    case SuggestionKind.Function:
    case SuggestionKind.Local:
    default:
      return []
  }
}

/** TODO: Add docs */
export function requiredImportsByProjectPath(
  db: SuggestionDb,
  projectPath: ProjectPath,
  directConImport: boolean = false,
) {
  const entry = db.getEntryByProjectPath(projectPath)
  return entry ? requiredImports(db, entry, directConImport) : []
}

function selfTypeEntry(db: SuggestionDb, entry: SuggestionEntry): SuggestionEntry | undefined {
  if (
    (entry.kind === SuggestionKind.Method || entry.kind === SuggestionKind.Constructor) &&
    entry.memberOf
  ) {
    return db.getEntryByProjectPath(entry.memberOf)
  }
}

function definedInEntry(db: SuggestionDb, entry: SuggestionEntry): SuggestionEntry | undefined {
  return db.getEntryByProjectPath(entry.definedIn)
}

function entryPathFromRequiredImport(importStatement: RequiredImport): ProjectPath {
  return importStatement.kind === 'Qualified' ?
      importStatement.module
    : importStatement.from.append(importStatement.import)
}

/** TODO: Add docs */
export function requiredImportEquals(left: RequiredImport, right: RequiredImport): boolean {
  if (left.kind != right.kind) return false
  switch (left.kind) {
    case 'Qualified':
      return left.module.equals((right as QualifiedImport).module)
    case 'Unqualified':
      return (
        left.from.equals((right as UnqualifiedImport).from) &&
        left.import === (right as UnqualifiedImport).import
      )
  }
}

/** Check if `existing` import statement covers `required`. */
export function covers(existing: AbstractImport, required: RequiredImport): boolean {
  if (required.kind === 'Qualified') {
    if (existing.imported.kind === 'Module' && existing.from.equals(required.module)) return true
    const parentAndName = required.module.splitAtName()
    if (!parentAndName) return false
    const [parent, name] = parentAndName
    return coversUnqualified(existing, parent, name)
  } else {
    return coversUnqualified(existing, required.from, required.import)
  }
}

function coversUnqualified(
  existing: AbstractImport,
  parent: ProjectPath,
  name: IdentifierOrOperatorIdentifier,
) {
  const importedInList =
    existing.imported.kind === 'List' &&
    existing.from.equals(parent) &&
    existing.imported.names.includes(name as Identifier)
  const importedWithAll =
    existing.imported.kind === 'All' &&
    existing.from.equals(parent) &&
    !existing.imported.except.includes(name as Identifier)
  return importedInList || importedWithAll
}

/** TODO: Add docs */
export function filterOutRedundantImports(
  existing: AbstractImport[],
  required: RequiredImport[],
): RequiredImport[] {
  return required.filter((info) => !existing.some((existing) => covers(existing, info)))
}

/* Information about detected conflict import, and advisory on resolution. */
export interface DetectedConflict {
  /* Always true, for more expressive API usage. */
  detected: boolean
  /* Advisory to replace the following name (qualified name or single ident)… */
  pattern: QualifiedName | Identifier
  /* … with this fully qualified name. */
  fullyQualified: ProjectPath
}

export type ConflictInfo = DetectedConflict | undefined

/** Detect possible name clash when adding `importsForEntry` with `existingImports` present. */
export function detectImportConflicts(
  suggestionDb: SuggestionDb,
  existingImports: AbstractImport[],
  importToCheck: RequiredImport,
): ConflictInfo {
  const entryPath = entryPathFromRequiredImport(importToCheck)
  const entryId = suggestionDb.findByProjectPath(entryPath)
  if (entryId == null) return
  const name =
    entryPath.path ? qnLastSegment(entryPath.path)
    : entryPath.project ? qnLastSegment(entryPath.project)
    : undefined
  if (!name) return
  const conflictingIds = suggestionDb.conflictingNames.lookup(name)

  for (const id of conflictingIds) {
    if (id === entryId) continue
    const e = suggestionDb.get(id)
    const required = e ? requiredImports(suggestionDb, e) : []
    for (const req of required) {
      if (existingImports.some((existing) => covers(existing, req))) {
        return {
          detected: true,
          pattern: name,
          fullyQualified: entryPath,
        }
      }
    }
  }
}
