declare const urlStringBrand: unique symbol
export type URLString = string & { [urlStringBrand]: never }

export function isUrlString(value: string): value is URLString {
  return /^\w+[/]*:/.test(value)
}
