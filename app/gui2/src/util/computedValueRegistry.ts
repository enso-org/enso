import type {
  ExpressionId,
  ExpressionUpdate,
  ExpressionUpdatePayload,
  MethodCall,
} from '../../shared/languageServerTypes.ts'

export interface ExpressionInfo {
  typename: string | undefined
  methodCall: MethodCall | undefined
  payload: ExpressionUpdatePayload
}

//* This class holds the computed values that have been received from the language server. */
export class ComputedValueRegistry {
  private internalMap: Map<ExpressionId, ExpressionInfo>

  constructor() {
    this.internalMap = new Map()
  }

  processUpdate(update: ExpressionUpdate) {
    const exprId = update.expressionId
    const info = {
      typename: update.type,
      methodCall: update.methodCall,
      payload: update.payload,
    }
    this.internalMap.set(exprId, info)
    return { exprId, info }
  }

  getExpressionInfo(exprId: ExpressionId): ExpressionInfo | undefined {
    return this.internalMap.get(exprId)
  }
}
