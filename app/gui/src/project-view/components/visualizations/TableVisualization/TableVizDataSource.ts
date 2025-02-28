import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'

const parseSingleArgument = (i: any, tempModule: Ast.MutableModule) => {
  switch (i.valueType) {
    case 'Date': {
      const datePattern = Pattern.parseExpression('(Date.new __ __ __)')
      const dateParts = i.value
        .match(/\d+/g)!
        .slice(0, 3)
        .map((part: string) => Ast.tryNumberToEnso(Number(part), tempModule)!)
      return datePattern.instantiateCopied(dateParts)
    }
    case 'Time': {
      const pattern = Pattern.parseExpression('Time_Of_Day.parse (__)')!
      return pattern.instantiateCopied([Ast.TextLiteral.new(i.value, tempModule)])
    }
    case 'Date_Time': {
      const pattern = Pattern.parseExpression('Date_Time.parse (__)')!
      return pattern.instantiateCopied([Ast.TextLiteral.new(i.value, tempModule)])
    }
    case 'Integer':
      return Ast.parseExpression(i.value, tempModule)
    case 'Char':
      return Ast.TextLiteral.new(i.value)
    case 'Mixed': {
      const items = i.value.map((val: any) => parseSingleArgument(val, tempModule))
      return Ast.Vector.new(tempModule, items)
    }
    default:
      return Ast.parseExpression(i, tempModule)
  }
}

export const parseArgument = (arg: any, tempModule: Ast.MutableModule) => {
  if (Array.isArray(arg)) {
    const itemList = arg.map((i) => parseSingleArgument(i, tempModule))
    return Ast.Vector.new(tempModule, itemList!)
  }
  return Ast.parseExpression(arg, tempModule)!
}
