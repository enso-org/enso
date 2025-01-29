import { expect, test } from 'vitest'
import { getCellValueType, isNumericType } from '../TableVisualization/tableVizUtils'

test('getCellValueType (Text)', () => {
  expect(getCellValueType('Alan')).toEqual('Char')
})

test('getCellValueType (Number)', () => {
  expect(getCellValueType('123')).toEqual('Integer')
})

test('getCellValueType (Date)', () => {
  expect(getCellValueType('1997-02-01')).toEqual('Date')
})

test('getCellValueType (Time Of Day hh:mm:ss)', () => {
  expect(getCellValueType('12:30:45')).toEqual('Time')
})

test('getCellValueType (Time Of Day hh:mm:ss with nanoseccond etc)', () => {
  expect(getCellValueType('12:30:45.02')).toEqual('Time')
})

test('getCellValueType (Date_Time)', () => {
  expect(getCellValueType('1997-02-01 12:30:45')).toEqual('Date_Time')
})

test('getCellValueType (Date_Time with nanoseccond etc)', () => {
  expect(getCellValueType('1997-02-01 12:30:45.02')).toEqual('Date_Time')
})

test('getCellValueType (Date_Time with time zone and with nanoseccond etc)', () => {
  expect(getCellValueType('1997-02-01 12:30:45.02[+02:00]')).toEqual('Date_Time')
})

test('isNumericType (Numeric Type)', () => {
  expect(isNumericType('Integer')).toEqual(true)
})

test('isNumericType (Char Type)', () => {
  expect(isNumericType('Char')).toEqual(false)
})
