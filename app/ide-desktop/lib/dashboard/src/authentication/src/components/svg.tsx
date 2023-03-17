/** @file File containing SVG icon definitions. */
/** TODO [NP]: https://github.com/enso-org/cloud-v2/issues/342
 * These should all be regular `.svg` files rather than React components, but React doesn't include
 * the `svg` files when building for Electron. Once the build scripts have been adapted to allow for
 * for this, the contents of this file should be moved back to standalone SVG files. */

// =================
// === Constants ===
// =================

/** Path data for the SVG icons used in app. */
export const PATHS = {
  /** Path data for the `@` icon SVG. */
  at: "M16 12a4 4 0 10-8 0 4 4 0 008 0zm0 0v1.5a2.5 2.5 0 005 0V12a9 9 0 10-9 9m4.5-1.206a8.959 8.959 0 01-4.5 1.207",
  /** Path data for the lock icon SVG. */
  lock: "M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z",
  /** Path data for the "right arrow" icon SVG. */
  rightArrow: "M13 9l3 3m0 0l-3 3m3-3H8m13 0a9 9 0 11-18 0 9 9 0 0118 0z",
  /** Path data for the "create account" icon SVG. */
  createAccount:
    "M18 9v3m0 0v3m0-3h3m-3 0h-3m-2-5a4 4 0 11-8 0 4 4 0 018 0zM3 20a6 6 0 0112 0v1H3v-1z",
  /** Path data for the "go back" icon SVG. */
  goBack:
    "M11 16l-4-4m0 0l4-4m-4 4h14m-5 4v1a3 3 0 01-3 3H6a3 3 0 01-3-3V7a3 3 0 013-3h7a3 3 0 013 3v1",
} as const;

// ===========
// === Svg ===
// ===========

/** Props for the `Svg` component. */
interface Props {
  data: string;
}

/** Component for rendering SVG icons.
 *
 * @param props - Extra props for the SVG path. The `props.data` field in particular contains the
 * SVG path data. */
export function Svg(props: Props) {
  return (
    <svg
      className="h-6 w-6"
      fill="none"
      strokeLinecap="round"
      strokeLinejoin="round"
      strokeWidth="2"
      viewBox="0 0 24 24"
      stroke="currentColor"
    >
      <path d={props.data} />
    </svg>
  );
}
