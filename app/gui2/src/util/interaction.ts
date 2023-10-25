export abstract class Interaction {
  id: number
  static nextId: number = 0
  constructor() {
    this.id = Interaction.nextId
    Interaction.nextId += 1
  }

  abstract cancel(): void
  click(_e: MouseEvent): boolean {
    return false
  }
}
