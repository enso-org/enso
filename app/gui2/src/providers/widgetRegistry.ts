import WidgetHierarchy from '@/components/GraphEditor/widgets/WidgetHierarchy.vue'
import WidgetNumber from '@/components/GraphEditor/widgets/WidgetNumber.vue'
import WidgetToken from '@/components/GraphEditor/widgets/WidgetToken.vue'
import { Token, Tree } from '@/generated/ast'
import type { AstExtended } from '@/util/ast'
import type { Component } from 'vue'
import { createContextStore } from '.'

export type WidgetComponent<Ast> = Component<{ ast: Ast }>

interface SelectedWidget<GivenAst extends AcceptedAst, AcceptedAst> {
  component: WidgetComponent<AcceptedAst>
  ast: GivenAst
}

function selected<GivenAst extends AcceptedAst, AcceptedAst>(
  ast: GivenAst,
  component: WidgetComponent<AcceptedAst>,
): SelectedWidget<GivenAst, AcceptedAst> {
  return {
    component,
    ast,
  }
}

export { injectFn as injectWidgetRegistry, provideFn as provideWidgetRegistry }
const { provideFn, injectFn } = createContextStore('Widget registry', () => new WidgetRegistry())

class WidgetRegistry {
  private selectToken(ast: AstExtended<Token>): SelectedWidget<AstExtended<Token>, any> {
    switch (ast.inner.type) {
      default:
        return selected(ast, WidgetToken)
    }
  }

  private selectTree(ast: AstExtended<Tree>): SelectedWidget<AstExtended<Tree>, any> {
    if (ast.isTree(Tree.Type.UnaryOprApp)) {
      if (
        ast.map((t) => t.opr).repr() === '-' &&
        ast.tryMap((t) => t.rhs)?.isTree(Tree.Type.Number)
      ) {
        return selected(ast, WidgetNumber)
      }
    }
    switch (ast.inner.type) {
      case Tree.Type.Number:
        return {
          component: WidgetNumber,
          ast,
        }
      default:
        return {
          component: WidgetHierarchy,
          ast: ast,
        }
    }
  }

  select(ast: AstExtended): SelectedWidget<AstExtended, any> {
    if (ast.isTree()) {
      return this.selectTree(ast)
    } else if (ast.isToken()) {
      return this.selectToken(ast)
    } else {
      throw new Error('Unknown node type')
    }
  }
}
