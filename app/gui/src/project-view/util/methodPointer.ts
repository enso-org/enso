import { type ProjectNameStore } from '@/stores/projectNames'
import { type ProjectPath } from '@/util/projectPath'
import { type QualifiedName } from '@/util/qualifiedName'
import * as encoding from 'lib0/encoding'
import { type Identifier, type IdentifierOrOperatorIdentifier } from 'ydoc-shared/ast'
import type {
  LocalCall,
  ExplicitCall as LSExplicitCall,
  LSMethodPointer,
} from 'ydoc-shared/languageServerTypes'

/** Serialize a {@link MethodPointer}. */
export function encodeMethodPointer(enc: encoding.Encoder, ptr: MethodPointer) {
  encoding.writeVarString(enc, ptr.module.project ?? '')
  encoding.writeVarString(enc, ptr.module.path ?? '')
  encoding.writeVarString(enc, ptr.name)
  encoding.writeVarString(enc, ptr.definedOnType.project ?? '')
  encoding.writeVarString(enc, ptr.definedOnType.path ?? '')
}

export interface MethodPointer {
  /** The fully qualified module name. */
  module: ProjectPath
  /** The type on which the method is defined. */
  definedOnType: ProjectPath
  /** The method name. */
  name: IdentifierOrOperatorIdentifier
}

/** Whether one {@link MethodPointer} deeply equals another. */
export function methodPointerEquals(left: MethodPointer, right: MethodPointer): boolean {
  return (
    left.module.equals(right.module) &&
    left.definedOnType.equals(right.definedOnType) &&
    left.name === right.name
  )
}

/** Translate a method pointer from the raw protocol version to the logical version with project names abstracted. */
export function parseMethodPointer(
  ptr: LSMethodPointer,
  projectNames: ProjectNameStore,
): MethodPointer {
  const { module, definedOnType, name } = ptr
  return {
    module: projectNames.parseProjectPath(module as QualifiedName),
    definedOnType: projectNames.parseProjectPath(definedOnType as QualifiedName),
    name: name as Identifier,
  }
}

export type StackItem = ExplicitCall | LocalCall

export type ExplicitCall = Omit<LSExplicitCall, 'methodPointer'> & {
  methodPointer: MethodPointer
}

export interface MethodCall {
  /** The method pointer of a call. */
  methodPointer: MethodPointer

  /** Indexes of arguments that have not been applied to this method. */
  notAppliedArguments: number[]
}

/** Whether one {@link StackItem} is deeply equal to another. */
export function stackItemsEqual(left: StackItem, right: StackItem): boolean {
  if (left.type !== right.type) return false
  if (left.type === 'ExplicitCall') {
    const explicitRight = right as ExplicitCall
    return methodPointerEquals(left.methodPointer, explicitRight.methodPointer)
  } else {
    const localRight = right as LocalCall
    return left.expressionId === localRight.expressionId
  }
}
