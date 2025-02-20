/**
 * An error that occurs when a network request fails.
 *
 * This error is used to indicate that a network request failed due to a network error,
 * such as a timeout or a connection error.
 */
export class NetworkError extends Error {
  /**
   * Create a new {@link NetworkError} with the specified message.
   * @param message - The message to display when the error is thrown.
   */
  constructor(message: string, options?: ErrorOptions) {
    super(message, options)
    this.name = 'NetworkError'
  }
}

/**
 * An error that occurs when the user is offline.
 *
 * This error is used to indicate that the user is offline, such as when they are
 * not connected to the internet or when they are on an airplane.
 */
export class OfflineError extends Error {
  /**
   * Create a new {@link OfflineError} with the specified message.
   * @param message - The message to display when the error is thrown.
   */
  constructor(message: string = 'User is offline', options?: ErrorOptions) {
    super(message, options)
    this.name = 'OfflineError'
  }
}

/**
 * An error with a display message.
 *
 * This message can be shown to a user.
 */
export class ErrorWithDisplayMessage extends Error {
  readonly displayMessage: string
  /**
   * Create a new {@link ErrorWithDisplayMessage} with the specified message and display message.
   * @param message - The message to display when the error is thrown.
   * @param options - The options to pass to the error.
   */
  constructor(message: string, options: ErrorOptions & { displayMessage: string }) {
    super(message, options)
    this.name = 'ErrorWithDisplayMessage'
    this.displayMessage = options.displayMessage
  }
}
