import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'

type ValueTypes = 'Date'|
'Time'|
'Date_Time'|
'Integer'|
'Char'
type ValueTypeArgumentChild = {valueType: ValueTypes; value: string}
type ValueTypeArgumentParent = {valueType: ValueTypes; value: string} | {valueType: 'Mixed'; value: ValueTypeArgumentChild[]}
type PossibleArguments = string | ValueTypeArgumentParent
type Argument = string | Array<PossibleArguments>

const parseSingleArgument = (value: ValueTypeArgumentParent, tempModule: Ast.MutableModule) : Ast.Owned<Ast.MutableExpression> => {
  switch (value.valueType) {
    case 'Date': {
      const datePattern = Pattern.parseExpression('(Date.new __ __ __)')
      const dateParts = value.value
        .match(/\d+/g)!
        .slice(0, 3)
        .map((part: string) => Ast.tryNumberToEnso(Number(part), tempModule)!)
      return datePattern.instantiateCopied(dateParts)
    }
    case 'Time': {
      const pattern = Pattern.parseExpression('Time_Of_Day.parse (__)')!
      return pattern.instantiateCopied([Ast.TextLiteral.new(value.value, tempModule)])
    }
    case 'Date_Time': {
      const pattern = Pattern.parseExpression('Date_Time.parse (__)')!
      return pattern.instantiateCopied([Ast.TextLiteral.new(value.value, tempModule)])
    }
    case 'Integer':
      return Ast.parseExpression(value.value, tempModule)!
    case 'Char':
      return Ast.TextLiteral.new(value.value)
    case 'Mixed': {
      const items = value.value.map((val: {valueType: ValueTypes; value: string}) => parseSingleArgument(val, tempModule))
      return Ast.Vector.new(tempModule, items)
    }
    default:
      return Ast.parseExpression(value, tempModule)!
  }
}

export const parseArgument = (arg: Argument, tempModule: Ast.MutableModule) => {
  if (Array.isArray(arg)) {
    const itemList = arg.map((i) => {
      return typeof i === 'string' ?
        Ast.parseExpression(i, tempModule)!
        :
      parseSingleArgument(i, tempModule)
  })
    return Ast.Vector.new(tempModule, itemList!)
  }
  return Ast.parseExpression(arg, tempModule)!
}
