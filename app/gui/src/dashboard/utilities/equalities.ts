/**
 * @file
 *
 * This file contains functions for checking equality between values.
 */
import { shallow } from 'zustand/shallow'

/**
 * Strict equality check.
 */
export function refEquality<T>(a: T, b: T) {
  return a === b
}

/**
 * Object.is equality check.
 */
export function objectEquality<T>(a: T, b: T) {
  return Object.is(a, b)
}

/**
 * Shallow equality check.
 */
export function shallowEquality<T>(a: T, b: T) {
  return shallow(a, b)
}
