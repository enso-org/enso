import type { SuggestionId } from '@/stores/suggestionDatabase/entry'
import type { ComputedRef, Ref } from 'vue'
import { computed, reactive, ref } from 'vue'

/** Simple stack for going forward and backward through the history of visited documentation pages */
export class HistoryStack {
  private stack: SuggestionId[]
  private index: Ref<number>
  public current: ComputedRef<SuggestionId | undefined>

  /** TODO: Add docs */
  constructor() {
    this.stack = reactive([])
    this.index = ref(0)
    this.current = computed(() => this.stack[this.index.value] ?? undefined)
  }

  /** TODO: Add docs */
  public reset(current: SuggestionId) {
    this.stack.length = 0
    this.stack.push(current)
    this.index.value = 0
  }

  /** TODO: Add docs */
  public record(id: SuggestionId) {
    this.stack.splice(this.index.value + 1)
    this.stack.push(id)
    this.index.value = this.stack.length - 1
  }

  /** TODO: Add docs */
  public forward() {
    if (this.canGoForward()) {
      this.index.value += 1
    }
  }

  /** TODO: Add docs */
  public backward() {
    if (this.canGoBackward()) {
      this.index.value -= 1
    }
  }

  /** TODO: Add docs */
  public canGoBackward(): boolean {
    return this.index.value > 0
  }

  /** TODO: Add docs */
  public canGoForward(): boolean {
    return this.index.value < this.stack.length - 1
  }
}
