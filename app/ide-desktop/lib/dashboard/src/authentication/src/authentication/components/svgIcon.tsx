/** @file Styled wrapper around {@link Svg} icons. */

// ===============
// === SvgIcon ===
// ===============

interface SvgIconProps {
    svg: JSX.Element
}

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
