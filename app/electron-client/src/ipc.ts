/**
 * @file Inter-Process communication configuration of the application. IPC allows the web-view
 * content to exchange information with the Electron application.
 */

// ===============
// === Channel ===
// ===============

/** Channel names used by the IPC protocol. */
export enum Channel {
  /** Channels for passing log messages from renderer to main process. */
  log = 'log',
  info = 'info',
  warn = 'warn',
  error = 'error',
  /** Channel for requesting that a URL be opened by the system browser. */
  openUrlInSystemBrowser = 'open-url-in-system-browser',
  /** Channel for signaling that a deep link to this application was opened. */
  openDeepLink = 'open-deep-link',
  /** Channel for signaling that access token be saved to a credentials file. */
  saveAccessToken = 'save-access-token',
  /** Channel for importing a project or project bundle from the given path. */
  importProjectFromPath = 'import-project-from-path',
  /** Channel for opening project */
  openProject = 'open-project',
  goBack = 'go-back',
  goForward = 'go-forward',
  /** Channel for selecting files and directories using the system file browser. */
  openFileBrowser = 'open-file-browser',
  /** Show a file or folder in the system file browser. */
  showItemInFolder = 'show-item-in-folder',
  /** Download a file using its URL. */
  downloadURL = 'download-url',
  handleMenuItem = 'handle-menu-item',
  /** Run the local Claude agent to generate a User Defined Component body. */
  generateAiComponent = 'generate-ai-component',
  /** Main → renderer: tool call from the in-flight Claude turn; the renderer must echo `requestId`. */
  aiToolCall = 'ai-tool-call',
  /** Renderer → main: reply to a previously-issued {@link Channel.aiToolCall}. */
  aiToolReply = 'ai-tool-reply',
  /**
   * Main → renderer: live progress events while a Claude turn runs. Each carries the originating
   * request's id so the renderer can route to the correct placeholder. Payload shape is
   * {@link AiProgressEvent} from `enso-common/src/ai`.
   */
  aiProgress = 'ai-progress',
  /**
   * Renderer → main: cancel an in-flight or queued AI component request, identified by the
   * `requestId` carried in the original {@link generateAiComponent} payload.
   */
  cancelAiComponent = 'cancel-ai-component',
  /**
   * Renderer → main: query whether the local Claude agent is available — i.e. the `claude`
   * binary was found on PATH and the child process spawned without a synchronous ENOENT.
   * Resolves once the spawn outcome is known; does NOT wait for the priming turn to complete.
   * Returns `false` when `ENSO_AI_DISABLED=1` is set in the main process environment.
   */
  aiIsAvailable = 'ai-is-available',
}
