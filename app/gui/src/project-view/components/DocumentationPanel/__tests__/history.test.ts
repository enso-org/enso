import { HistoryStack } from '@/components/DocumentationPanel/history'
import { expect, test } from 'vitest'

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
