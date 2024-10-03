import type { Page } from '@playwright/test'
import type { ExpressionUpdate, MethodCall } from 'ydoc-shared/languageServerTypes'

export type ExpressionLocator = string | { binding: string; expr: string }

/** Provide method call info for collapsed function call. */
export async function mockCollapsedFunctionInfo(
  page: Page,
  expression: ExpressionLocator,
  functionName: string,
) {
  await mockMethodCallInfo(page, expression, {
    methodPointer: {
      module: 'local.Mock_Project.Main',
      definedOnType: 'local.Mock_Project.Main',
      name: functionName,
    },
    notAppliedArguments: [],
  })
}

/** Provide custom method call info for the specific node. */
export async function mockMethodCallInfo(
  page: Page,
  expression: ExpressionLocator,
  methodCallInfo: MethodCall,
) {
  await mockExpressionUpdate(page, expression, { methodCall: methodCallInfo })
}

/** Provide custom expression update for the specific node. */
export async function mockExpressionUpdate(
  page: Page,
  expression: ExpressionLocator,
  update: Partial<ExpressionUpdate>,
) {
  await page.evaluate(
    ({ expression, update }) => (window as any)._mockExpressionUpdate(expression, update),
    { expression, update },
  )
}
