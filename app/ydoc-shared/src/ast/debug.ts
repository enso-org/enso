import { Ast } from './tree'

/** Returns a GraphViz graph illustrating parent/child relationships in the given subtree. */
export function graphParentPointers(ast: Ast) {
  const sanitize = (id: string) => id.replace('ast:', '').replace(/[^A-Za-z0-9]/g, '')
  const parentToChild = new Array<{ parent: string; child: string }>()
  const childToParent = new Array<{ child: string; parent: string }>()
  ast.visitRecursiveAst(ast => {
    for (const child of ast.children()) {
      if (child instanceof Ast)
        parentToChild.push({ child: sanitize(child.id), parent: sanitize(ast.id) })
    }
    const parent = ast.parentId
    if (parent) childToParent.push({ child: sanitize(ast.id), parent: sanitize(parent) })
  })
  let result = 'digraph parentPointers {\n'
  for (const { parent, child } of parentToChild) result += `${parent} -> ${child};\n`
  for (const { child, parent } of childToParent)
    result += `${child} -> ${parent} [weight=0; color=red; style=dotted];\n`
  result += '}\n'
  return result
}
