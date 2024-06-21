/* eslint-disable vue/one-component-per-file */
import { mount } from '@vue/test-utils'
import { describe, expect, test } from 'vitest'
import { defineComponent } from 'vue'
import { usePropagateScopesToAllRoots } from '../patching'

describe('usePropagateScopesToAllRoots', () => {
  function makeComponents(doPropagate: boolean) {
    const InnerComponent = defineComponent({
      template: '<div class="inner"></div>',
      __scopeId: 'inner',
    })

    const WrapperComponent = defineComponent({
      setup() {
        if (doPropagate) usePropagateScopesToAllRoots()
      },
      template: '<slot />',
      __scopeId: 'wrapper',
    })

    const WrappingComponent = defineComponent({
      components: { Inner: InnerComponent, Wrapper: WrapperComponent },
      template: '<Wrapper><Inner /></Wrapper>',
      __scopeId: 'wrapping',
    })

    const OuterComponent = defineComponent({
      components: { Wrapping: WrappingComponent },
      template: '<Wrapping />',
      __scopeId: 'outer',
    })
    return { InnerComponent, OuterComponent }
  }

  test('scopes propagate through wrapper root slot', () => {
    const { InnerComponent, OuterComponent } = makeComponents(true)
    const outer = mount(OuterComponent)
    expect(outer.findComponent(InnerComponent).element.getAttributeNames()).toEqual([
      'inner',
      'wrapping',
      'wrapper-s',
      'outer',
      'class',
    ])
  })

  test('manual propagation required', () => {
    const { InnerComponent, OuterComponent } = makeComponents(false)
    const outer = mount(OuterComponent)
    expect(outer.findComponent(InnerComponent).element.getAttributeNames()).toEqual([
      'inner',
      'wrapping',
      'wrapper-s',
      'class',
    ])
  })
})
