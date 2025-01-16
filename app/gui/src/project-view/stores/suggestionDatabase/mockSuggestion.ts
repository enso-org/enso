import {
  type SuggestionEntry,
  type SuggestionEntryArgument,
} from '@/stores/suggestionDatabase/entry'
import { SuggestionUpdateProcessor } from '@/stores/suggestionDatabase/lsUpdate'
import { ANY_TYPE_QN } from '@/util/ensoTypes'
import { isQualifiedName, qnParent, qnSplit } from '@/util/qualifiedName'
import * as lsTypes from 'ydoc-shared/languageServerTypes/suggestions'
import { assert } from 'ydoc-shared/util/assert'
import { unwrap } from 'ydoc-shared/util/data/result'

function makeEntry(lsEntry: lsTypes.SuggestionEntry) {
  return unwrap(new SuggestionUpdateProcessor([]).entryFromLs(lsEntry))
}

const EMPTY_SCOPE = { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } }

/** Mock a module suggestion entry. */
export function makeModule(fqn: string): SuggestionEntry {
  return makeEntry({
    type: 'module',
    module: fqn,
  })
}

/** Mock a type suggestion entry. */
export function makeType(fqn: string): SuggestionEntry {
  assert(isQualifiedName(fqn))
  const [definedIn, name] = qnSplit(fqn)
  assert(definedIn != null)
  return makeEntry({
    type: 'type',
    module: definedIn,
    name,
    params: [],
  })
}

/** Mock a type constructor suggestion entry. */
export function makeConstructor(fqn: string): SuggestionEntry {
  assert(isQualifiedName(fqn))
  const [type, name] = qnSplit(fqn)
  assert(type != null)
  const definedIn = qnParent(type)
  assert(definedIn != null)
  return makeEntry({
    type: 'constructor',
    name,
    module: definedIn,
    arguments: [],
    returnType: type,
    annotations: [],
  })
}

/** Mock a type method suggestion entry. */
export function makeMethod(
  fqn: string,
  returnType: string = ANY_TYPE_QN,
  isStatic: boolean = false,
): SuggestionEntry {
  assert(isQualifiedName(fqn))
  const [type, name] = qnSplit(fqn)
  assert(type != null)
  const definedIn = qnParent(type)
  assert(definedIn != null)
  return makeEntry({
    type: 'method',
    name,
    module: definedIn,
    arguments: [],
    selfType: type,
    returnType,
    isStatic,
    annotations: [],
  })
}

/** Mock a static type method suggestion entry. */
export function makeStaticMethod(fqn: string, returnType: string = ANY_TYPE_QN): SuggestionEntry {
  return makeMethod(fqn, returnType, true)
}

/** Mock a module method suggestion entry. */
export function makeModuleMethod(fqn: string, returnType: string = ANY_TYPE_QN): SuggestionEntry {
  assert(isQualifiedName(fqn))
  const [module, name] = qnSplit(fqn)
  assert(module != null)
  return makeEntry({
    type: 'method',
    name,
    module,
    arguments: [],
    selfType: module,
    returnType,
    isStatic: true,
    annotations: [],
  })
}

/** Mock a function suggestion entry. */
export function makeFunction(
  definedIn: string,
  name: string,
  returnType: string = ANY_TYPE_QN,
): SuggestionEntry {
  return makeEntry({
    type: 'function',
    name,
    module: definedIn,
    arguments: [],
    returnType,
    scope: EMPTY_SCOPE,
  })
}

/** Mock a local variable suggestion entry. */
export function makeLocal(
  definedIn: string,
  name: string,
  returnType: string = ANY_TYPE_QN,
): SuggestionEntry {
  return makeEntry({
    type: 'local',
    name,
    module: definedIn,
    returnType,
    scope: EMPTY_SCOPE,
  })
}

/** Mock a suggestion entry argument specification. */
export function makeArgument(name: string, type: string = ANY_TYPE_QN): SuggestionEntryArgument {
  return {
    name,
    reprType: type,
    isSuspended: false,
    hasDefault: false,
  }
}
