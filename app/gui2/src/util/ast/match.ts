import { Ast } from '@/util/ast'
import { MutableModule } from '@/util/ast/abstract'

export class Pattern {
  private readonly tokenTree: Ast.TokenTree
  private readonly template: string
  private readonly placeholder: string

  constructor(template: string, placeholder: string) {
    this.tokenTree = Ast.tokenTree(Ast.parse(template))
    this.template = template
    this.placeholder = placeholder
  }

  /** Parse an expression template in which a specified identifier (by default `__`)
   *  may match any arbitrary subtree. */
  static parse(template: string, placeholder: string = '__'): Pattern {
    return new Pattern(template, placeholder)
  }

  /** If the given expression matches the pattern, return the subtrees that matched the holes in the pattern. */
  match(target: Ast.Ast): Ast.AstId[] | undefined {
    const extracted: Ast.AstId[] = []
    if (this.tokenTree.length === 1 && this.tokenTree[0] === this.placeholder) {
      return [target.id]
    }
    if (
      isMatch_(
        this.tokenTree,
        target.concreteChildren(),
        target.module,
        this.placeholder,
        extracted,
      )
    ) {
      return extracted
    }
  }

  /** Create a new concrete example of the pattern, with the placeholders replaced with the given subtrees.
   *  The subtree IDs provided must be accessible in the `edit` module. */
  instantiate(edit: MutableModule, subtrees: Ast.AstId[]): Ast.Ast {
    const ast = Ast.parse(this.template, edit)
    for (const matched of placeholders(ast, this.placeholder)) {
      const replacement = subtrees.shift()
      if (replacement === undefined) break
      const replacementAst = edit.splice(edit.get(replacement))
      if (replacementAst === null) {
        console.error(
          'Subtree ID provided to `instantiate` is not accessible in the `edit` module.',
        )
        continue
      }
      replacementAst.parent = matched.parent
      matched.ref.node = replacement
    }
    return ast
  }
}

function isMatch_(
  pattern: Ast.TokenTree,
  target: Iterator<Ast.NodeChild>,
  module: Ast.Module,
  placeholder: string,
  extracted: Ast.AstId[],
): boolean {
  for (const subpattern of pattern) {
    const next = target.next()
    if (next.done) return false
    const astOrToken = next.value.node
    const isPlaceholder = typeof subpattern !== 'string' && subpattern[0] === placeholder
    if (typeof subpattern === 'string') {
      if (!(astOrToken instanceof Ast.Token) || astOrToken.code() !== subpattern) {
        return false
      }
    } else if (astOrToken instanceof Ast.Token) {
      return false
    } else if (isPlaceholder) {
      extracted.push(astOrToken)
    } else {
      const ast = module.get(astOrToken)
      if (!ast) return false
      if (!isMatch_(subpattern, ast.concreteChildren(), module, placeholder, extracted))
        return false
    }
  }
  return true
}

type PlaceholderRef = {
  ref: Ast.NodeChild<Ast.AstId>
  parent: Ast.AstId
}

function placeholders(ast: Ast.Ast, placeholder: string, outIn?: PlaceholderRef[]) {
  const out = outIn ?? []
  for (const child of ast.concreteChildren()) {
    if (!(child.node instanceof Ast.Token)) {
      // The type of `child` has been determined by checking the type of `child.node`
      const nodeChild = child as Ast.NodeChild<Ast.AstId>
      const subtree = ast.module.get(child.node)!
      if (subtree instanceof Ast.Ident && subtree.code() === placeholder) {
        out.push({ ref: nodeChild, parent: ast.id })
      } else {
        placeholders(subtree, placeholder, out)
      }
    }
  }
  return out
}
