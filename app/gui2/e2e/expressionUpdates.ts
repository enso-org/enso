import type { Page } from '@playwright/test'
import type { ExpressionUpdate, MethodCall } from 'shared/languageServerTypes'

/** Provide method call info for collapsed function call. */
export async function mockCollapsedFunctionInfo(page: Page, binding: string, functionName: string) {
  await mockMethodCallInfo(page, binding, {
    methodPointer: {
      module: 'local.Mock.Main',
      definedOnType: 'local.Mock.Main',
      name: functionName,
    },
    notAppliedArguments: [],
  })
}

/** Provide custom method call info for the specific node. */
export async function mockMethodCallInfo(page: Page, binding: string, methodCallInfo: MethodCall) {
  await mockExpressionUpdate(page, binding, { methodCall: methodCallInfo })
}

/** Provide custom expression update for the specific node. */
export async function mockExpressionUpdate(
  page: Page,
  binding: string,
  update: Partial<ExpressionUpdate>,
) {
  await page.evaluate(
    ({ binding, update }) => (window as any).mockExpressionUpdate(binding, update),
    { binding, update },
  )
}
