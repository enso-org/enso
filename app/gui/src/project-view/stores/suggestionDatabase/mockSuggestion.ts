import {
  type SuggestionEntry,
  type SuggestionEntryArgument,
} from '@/stores/suggestionDatabase/entry'
import { SuggestionUpdateProcessor } from '@/stores/suggestionDatabase/lsUpdate'
import { ANY_TYPE_QN } from '@/util/ensoTypes'
import { isQualifiedName, qnParent, qnSplit, tryQualifiedName } from '@/util/qualifiedName'
import * as lsTypes from 'ydoc-shared/languageServerTypes/suggestions'
import { assert } from 'ydoc-shared/util/assert'
import { unwrap } from 'ydoc-shared/util/data/result'

function makeEntry(lsEntry: lsTypes.SuggestionEntry) {
  return unwrap(
    new SuggestionUpdateProcessor([
      { name: 'MockGroup1', project: unwrap(tryQualifiedName('Standard.Base')) },
      { name: 'MockGroup2', project: unwrap(tryQualifiedName('Standard.Base')) },
    ]).entryFromLs(lsEntry),
  )
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
export function makeConstructor(
  fqn: string,
  opts: { args?: SuggestionEntryArgument[]; annotations?: string[] } = {},
): SuggestionEntry {
  assert(isQualifiedName(fqn))
  const [type, name] = qnSplit(fqn)
  assert(type != null)
  const definedIn = qnParent(type)
  assert(definedIn != null)
  return makeEntry({
    type: 'constructor',
    name,
    module: definedIn,
    arguments: opts.args ?? [],
    returnType: type,
    annotations: opts.annotations ?? [],
  })
}

interface MakeMethodOptions extends DocOptions {
  returnType?: string
  isStatic?: boolean
  args?: SuggestionEntryArgument[]
  annotations?: string[]
}

/** Mock a type method suggestion entry. */
export function makeMethod(fqn: string, opts: MakeMethodOptions = {}): SuggestionEntry {
  assert(isQualifiedName(fqn))
  const [type, name] = qnSplit(fqn)
  assert(type != null)
  const definedIn = qnParent(type)
  assert(definedIn != null)
  return makeEntry({
    type: 'method',
    name,
    module: definedIn,
    arguments: opts.args ?? [],
    selfType: type,
    returnType: opts.returnType ?? ANY_TYPE_QN,
    isStatic: opts.isStatic ?? false,
    annotations: opts.annotations ?? [],
    documentation: makeDocumentation(opts),
  })
}

/** Mock a static type method suggestion entry. */
export function makeStaticMethod(
  fqn: string,
  opts: Omit<MakeMethodOptions, 'isStatic'> = {},
): SuggestionEntry {
  return makeMethod(fqn, { ...opts, isStatic: true })
}

interface DocOptions {
  aliases?: string[]
  group?: string
}
function makeDocumentation({ aliases, group }: DocOptions): string {
  const lines = []
  if (aliases?.length) lines.push(`ALIAS ${aliases.join(', ')}`)
  if (group) lines.push(`GROUP ${group}`)
  return lines.join('\n')
}

/** Mock a module method suggestion entry. */
export function makeModuleMethod(
  fqn: string,
  opts: { returnType?: string } & DocOptions = {},
): SuggestionEntry {
  assert(isQualifiedName(fqn))
  const [module, name] = qnSplit(fqn)
  assert(module != null)
  return makeEntry({
    type: 'method',
    name,
    module,
    arguments: [],
    selfType: module,
    returnType: opts.returnType ?? ANY_TYPE_QN,
    isStatic: true,
    annotations: [],
    documentation: makeDocumentation(opts),
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
