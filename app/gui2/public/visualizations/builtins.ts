export interface Vec2 {
  readonly x: number
  readonly y: number
}

export interface RGBA {
  red: number
  green: number
  blue: number
  alpha: number
}

export interface Theme {
  getColorForType(type: string): RGBA
}

export const DEFAULT_THEME: Theme = {
  getColorForType(type) {
    let hash = 0
    for (const c of type) {
      hash = 0 | (hash * 31 + c.charCodeAt(0))
    }
    if (hash < 0) {
      hash += 0x80000000
    }
    const red = (hash >> 24) / 0x180
    const green = ((hash >> 16) & 0xff) / 0x180
    const blue = ((hash >> 8) & 0xff) / 0x180
    return { red, green, blue, alpha: 1 }
  },
}
