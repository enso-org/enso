import type { SuggestionId } from '@/stores/suggestionDatabase/entry'
import type { ComputedRef, Ref } from 'vue'
import { computed, reactive, ref } from 'vue'

/**
 * Simple stack for going forward and backward through the history of visited documentation pages
 */
export class HistoryStack {
  private stack: SuggestionId[]
  private index: Ref<number>
  public current: ComputedRef<SuggestionId | undefined>

  constructor() {
    this.stack = reactive([])
    this.index = ref(0)
    this.current = computed(() => this.stack[this.index.value] ?? undefined)
  }

  public reset(current: SuggestionId) {
    this.stack.length = 0
    this.stack.push(current)
    this.index.value = 0
  }

  public record(id: SuggestionId) {
    this.stack.splice(this.index.value + 1)
    this.stack.push(id)
    this.index.value = this.stack.length - 1
  }

  public forward() {
    if (this.canGoForward()) {
      this.index.value += 1
    }
  }

  public backward() {
    if (this.canGoBackward()) {
      this.index.value -= 1
    }
  }

  public canGoBackward(): boolean {
    return this.index.value > 0
  }

  public canGoForward(): boolean {
    return this.index.value < this.stack.length - 1
  }
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest
  const ID_1 = 10
  const ID_2 = 20
  const ID_3 = 30

  test('HistoryStack basic operations', () => {
    const stack = new HistoryStack()
    expect(stack.current.value).toBeUndefined()
    expect(stack.canGoBackward()).toBeFalsy()
    expect(stack.canGoForward()).toBeFalsy()

    stack.reset(ID_1)
    expect(stack.current.value).toStrictEqual(ID_1)
    stack.forward()
    expect(stack.current.value).toStrictEqual(ID_1)
    expect(stack.canGoBackward()).toBeFalsy()
    expect(stack.canGoForward()).toBeFalsy()

    stack.record(ID_2)
    expect(stack.current.value).toStrictEqual(ID_2)
    expect(stack.canGoBackward()).toBeTruthy()
    expect(stack.canGoForward()).toBeFalsy()

    stack.backward()
    expect(stack.current.value).toStrictEqual(ID_1)
    expect(stack.canGoBackward()).toBeFalsy()
    expect(stack.canGoForward()).toBeTruthy()
    stack.backward()
    expect(stack.current.value).toStrictEqual(ID_1)
    stack.forward()
    expect(stack.current.value).toStrictEqual(ID_2)
    expect(stack.canGoForward()).toBeFalsy()
    stack.forward()
    expect(stack.current.value).toStrictEqual(ID_2)

    stack.backward()
    expect(stack.current.value).toStrictEqual(ID_1)
    stack.record(ID_3)
    expect(stack.current.value).toStrictEqual(ID_3)
    stack.forward()
    expect(stack.current.value).toStrictEqual(ID_3)
    stack.backward()
    expect(stack.current.value).toStrictEqual(ID_1)
  })
}
