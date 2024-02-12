import { Ast } from './tree'

/// Returns a GraphViz graph illustrating parent/child relationships in the given subtree.
export function graphParentPointers(ast: Ast) {
  const sanitize = (id: string) => id.replace('ast:', '').replace(/[^A-Za-z0-9]/g, '')
  const graph = new Array<{ parent: string; child: string }>()
  ast.visitRecursiveAst((ast) => {
    const parent = ast.parentId
    if (parent) graph.push({ child: sanitize(ast.id), parent: sanitize(parent) })
  })
  let result = 'digraph parentPointers {\n'
  for (const { parent, child } of graph) result += `${parent} -> ${child};\n`
  result += '}\n'
  return result
}
