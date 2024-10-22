import { SuggestionDb } from '@/stores/suggestionDatabase'
import { SuggestionKind, type SuggestionEntry } from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import { MutableModule, parseIdent, parseIdents, parseQualifiedName } from '@/util/ast/abstract'
import { unwrap } from '@/util/data/result'
import {
  qnLastSegment,
  qnSegments,
  qnSplit,
  tryQualifiedName,
  type IdentifierOrOperatorIdentifier,
  type QualifiedName,
} from '@/util/qualifiedName'

// ========================
// === Imports analysis ===
// ========================

/** Parse import statement. */
export function recognizeImport(ast: Ast.Import): Import | null {
  const from = ast.from
  const as = ast.as
  const import_ = ast.import_
  const all = ast.all
  const hiding = ast.hiding
  const moduleAst = from ?? import_
  const module = moduleAst ? parseQualifiedName(moduleAst) : null
  if (!module) return null
  if (all) {
    const except = (hiding != null ? parseIdents(hiding) : []) ?? []
    return {
      from: module,
      imported: { kind: 'All', except },
    }
  } else if (from && import_) {
    const names = parseIdents(import_) ?? []
    return {
      from: module,
      imported: { kind: 'List', names },
    }
  } else if (import_) {
    const alias = as ? parseIdent(as) : null
    return {
      from: module,
      imported: alias ? { kind: 'Module', alias } : { kind: 'Module' },
    }
  } else {
    console.error('Unrecognized import', ast.code())
    return null
  }
}

export type ModuleName = QualifiedName

/** Information about parsed import statement. */
export interface Import {
  from: ModuleName
  imported: ImportedNames
}

export type ImportedNames = Module | List | All

/** import Module.Path (as Alias)? */
export interface Module {
  kind: 'Module'
  alias?: IdentifierOrOperatorIdentifier
}

/** from Module.Path import (Ident),+ */
export interface List {
  kind: 'List'
  names: IdentifierOrOperatorIdentifier[]
}

/** from Module.Path import all (hiding (Ident),*)? */
export interface All {
  kind: 'All'
  except: IdentifierOrOperatorIdentifier[]
}

// ========================
// === Required imports ===
// ========================

/** Import required for the suggestion entry. */
export type RequiredImport = QualifiedImport | UnqualifiedImport

/** import Module.Path */
export interface QualifiedImport {
  kind: 'Qualified'
  module: QualifiedName
}

/** from Module.Path import SomeIdentifier */
export interface UnqualifiedImport {
  kind: 'Unqualified'
  from: QualifiedName
  import: IdentifierOrOperatorIdentifier
}

/** Read imports from given module block */
export function readImports(ast: Ast.Ast): Import[] {
  const imports: Import[] = []
  ast.visitRecursiveAst((node) => {
    if (node instanceof Ast.Import) {
      const recognized = recognizeImport(node)
      if (recognized) {
        imports.push(recognized)
      }
      return false
    }
    return true
  })
  return imports
}

/** Insert the given imports into the given block at an appropriate location. */
export function addImports(scope: Ast.MutableBodyBlock, importsToAdd: RequiredImport[]) {
  const imports = importsToAdd.map((info) => requiredImportToAst(info, scope.module))
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
    if (line.expression) {
      if (line.expression.node?.innerExpression() instanceof Ast.Import) {
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
export function requiredImportToAst(value: RequiredImport, module?: MutableModule) {
  const module_ = module ?? MutableModule.Transient()
  switch (value.kind) {
    case 'Qualified':
      return Ast.Import.Qualified(qnSegments(value.module), module_)!
    case 'Unqualified':
      return Ast.Import.Unqualified(qnSegments(value.from), value.import, module_)!
  }
}

/** A list of required imports for a specific suggestion entry. */
export function requiredImports(
  db: SuggestionDb,
  entry: SuggestionEntry,
  directConImport: boolean = false,
): RequiredImport[] {
  const unqualifiedImport = (from: QualifiedName): UnqualifiedImport[] => [
    {
      kind: 'Unqualified',
      from,
      import: entry.name,
    },
  ]
  switch (entry.kind) {
    case SuggestionKind.Module:
      return entry.reexportedIn ?
          unqualifiedImport(entry.reexportedIn)
        : [
            {
              kind: 'Qualified',
              module: entry.definedIn,
            },
          ]
    case SuggestionKind.Type:
      return unqualifiedImport(entry.reexportedIn ? entry.reexportedIn : entry.definedIn)
    case SuggestionKind.Constructor:
      if (directConImport) {
        return (
          entry.reexportedIn ? unqualifiedImport(entry.reexportedIn)
          : entry.memberOf ? unqualifiedImport(entry.memberOf)
          : []
        )
      } else {
        const selfType = selfTypeEntry(db, entry)
        return selfType ? requiredImports(db, selfType) : []
      }
    case SuggestionKind.Method: {
      const isStatic = entry.selfType == null
      const selfType = selfTypeEntry(db, entry)
      const isExtension = selfType && selfType.definedIn !== entry.definedIn
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
export function requiredImportsByFQN(
  db: SuggestionDb,
  fqn: QualifiedName,
  directConImport: boolean = false,
) {
  const entry = db.getEntryByQualifiedName(fqn)
  if (!entry) return []
  return requiredImports(db, entry, directConImport)
}

function selfTypeEntry(db: SuggestionDb, entry: SuggestionEntry): SuggestionEntry | undefined {
  if (entry.memberOf) {
    return db.getEntryByQualifiedName(entry.memberOf)
  }
}

function definedInEntry(db: SuggestionDb, entry: SuggestionEntry): SuggestionEntry | undefined {
  return db.getEntryByQualifiedName(entry.definedIn)
}

function entryFQNFromRequiredImport(importStatement: RequiredImport): QualifiedName {
  if (importStatement.kind === 'Qualified') {
    return importStatement.module
  } else {
    return unwrap(tryQualifiedName(`${importStatement.from}.${importStatement.import}`))
  }
}

/** TODO: Add docs */
export function requiredImportEquals(left: RequiredImport, right: RequiredImport): boolean {
  if (left.kind != right.kind) return false
  switch (left.kind) {
    case 'Qualified':
      return left.module === (right as QualifiedImport).module
    case 'Unqualified':
      return (
        left.from === (right as UnqualifiedImport).from &&
        left.import === (right as UnqualifiedImport).import
      )
  }
}

/** Check if `existing` import statement covers `required`. */
export function covers(existing: Import, required: RequiredImport): boolean {
  const [parent, name] =
    required.kind === 'Qualified' ? qnSplit(required.module)
    : required.kind === 'Unqualified' ? [required.from, required.import]
    : [undefined, '']
  const directlyImported =
    required.kind === 'Qualified' &&
    existing.imported.kind === 'Module' &&
    existing.from === required.module
  const importedInList =
    existing.imported.kind === 'List' &&
    parent != null &&
    existing.from === parent &&
    existing.imported.names.includes(name)
  const importedWithAll =
    existing.imported.kind === 'All' &&
    parent != null &&
    existing.from === parent &&
    !existing.imported.except.includes(name)
  return directlyImported || importedInList || importedWithAll
}

/** TODO: Add docs */
export function filterOutRedundantImports(
  existing: Import[],
  required: RequiredImport[],
): RequiredImport[] {
  return required.filter((info) => !existing.some((existing) => covers(existing, info)))
}

/* Information about detected conflict import, and advisory on resolution. */
export interface DetectedConflict {
  /* Always true, for more expressive API usage. */
  detected: boolean
  /* Advisory to replace the following name (qualified name or single ident)… */
  pattern: QualifiedName | IdentifierOrOperatorIdentifier
  /* … with this fully qualified name. */
  fullyQualified: QualifiedName
}

export type ConflictInfo = DetectedConflict | undefined

/* Detect possible name clash when adding `importsForEntry` with `existingImports` present. */
/** TODO: Add docs */
export function detectImportConflicts(
  suggestionDb: SuggestionDb,
  existingImports: Import[],
  importToCheck: RequiredImport,
): ConflictInfo {
  const entryFQN = entryFQNFromRequiredImport(importToCheck)
  const [entryId] = suggestionDb.nameToId.lookup(entryFQN)
  if (entryId == null) return
  const name = qnLastSegment(entryFQN)
  const conflictingIds = suggestionDb.conflictingNames.lookup(name)
  // Obviously, the entry doesn’t conflict with itself.
  conflictingIds.delete(entryId)

  for (const id of conflictingIds) {
    const e = suggestionDb.get(id)
    const required = e ? requiredImports(suggestionDb, e) : []
    for (const req of required) {
      if (existingImports.some((existing) => covers(existing, req))) {
        return {
          detected: true,
          pattern: name,
          fullyQualified: entryFQN,
        }
      }
    }
  }
}
