import { getCurrentInstance } from 'vue'

/**
 * Replace a function under given key on provided object and provide a hook that is called right
 * before the original function is executed.
 */
export function hookBeforeFunctionCall(object: any, key: PropertyKey, hook: () => void) {
  const original = object[key] as unknown
  if (typeof original === 'function') {
    object[key] = function (this: unknown, ...args: unknown[]) {
      hook()
      return original.apply(this, args)
    }
  }
}

/**
 * Force propagation of parent scopes into all root nodes of this component, i.e. children of it's
 * top-level Fragment VNode.
 *
 * By default, Vue is restricting scope propagation to only happen within components with only
 * singular root node. Unfortunately, that also happens to block propagation when components adds an
 * additional root-node, such as `Teleport`, or when rendering a slot as root. This is a workaround
 * that tricks Vue into actually propagating the scopes past this component's top-level `Fragment`
 * element, allowing parent scoped styles to affect its actual root node or rendered slots.
 */
export function usePropagateScopesToAllRoots() {
  const instance = getCurrentInstance()
  if (instance != null) {
    let _subTree = instance.subTree
    // Intercept all `subTree` assignments for this instance, getting an early opportunity to modify
    // its internal root VNode (usually a Fragment).
    Object.defineProperty(instance, 'subTree', {
      get: () => _subTree,
      set: (value) => {
        _subTree = value
        // Gather all scopes that would naturally propagate to this node, and assign them to
        // `slotScopeIds`, which Vue propagates through fragments on its own. This is an internal
        // mechanism used in implementation of `:scoped` custom CSS selector.
        collectParentScopes(((_subTree as any).slotScopeIds ??= []), _subTree, null, null, instance)
      },
    })

    // Mimics Vue's internal `setScopeIds`, but instead collects the scopes into an array.
    const collectParentScopes = (
      outScopes: string[],
      vnode: typeof instance.vnode,
      scopeId: string | null,
      slotScopeIds: string[] | null,
      parentComponent: typeof instance | null,
    ) => {
      if (scopeId) outScopes.push(scopeId)
      if (slotScopeIds) outScopes.push(...slotScopeIds)
      if (parentComponent) {
        const subTree = parentComponent.subTree
        if (vnode === subTree) {
          const parentVNode = parentComponent.vnode
          collectParentScopes(
            outScopes,
            parentVNode,
            parentVNode.scopeId,
            (parentVNode as any).slotScopeIds,
            parentComponent.parent,
          )
        }
      }
    }
  }
}
