/** Hooks for `react-stripe` mock. */

export const useStripe = () => ({
  confirmCardSetup: () => {},
})

export const useElements = () => ({
  getElement: () => {},
})
