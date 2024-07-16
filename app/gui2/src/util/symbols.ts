/**
 * @file This file provides a place to define symbols so that they are not redefined during HMR.
 *
 * If a module needs to expose a symbol to other modules, to enable that symbol to be stable across HMR:
 * - Define the symbol here, named with the `Symbol` suffix.
 * - Import it to the exposing module.
 * - Re-export it without the `Symbol` suffix.
 *   (Using a different name prevents accidentally importing from this module.)
 */

export const CustomDropdownItemsKeySymbol: unique symbol = Symbol('CustomDropdownItems')
