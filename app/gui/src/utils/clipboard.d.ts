interface Clipboard {
  // Recent addition to the spec: https://github.com/w3c/clipboard-apis/pull/197
  // Currently supported by Chromium: https://developer.chrome.com/docs/web-platform/unsanitized-html-async-clipboard
  read(options?: { unsanitized?: ['text/html'] }): Promise<ClipboardItems>
}
