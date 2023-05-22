/** @file Styled wrapper around SVG images. */

// ===============
// === SvgIcon ===
// ===============

/** Props for a {@link SvgIcon}. */
export interface SvgIconProps {
    svg: JSX.Element
}

/** A fixed-size container for a SVG image. */
function SvgIcon(props: SvgIconProps) {
    return (
        <div
            className={
                'inline-flex items-center justify-center absolute left-0 top-0 h-full w-10 ' +
                'text-gray-400'
            }
        >
            <span>{props.svg}</span>
        </div>
    )
}

export default SvgIcon
