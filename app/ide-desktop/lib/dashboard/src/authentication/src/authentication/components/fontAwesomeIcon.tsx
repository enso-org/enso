/** @file Styled wrapper around FontAwesome icons. */
import * as fontawesome from '@fortawesome/react-fontawesome'
import * as fontawesomeIcons from '@fortawesome/free-brands-svg-icons'

// =======================
// === FontAwesomeIcon ===
// =======================

export interface FontAwesomeIconProps {
    icon: fontawesomeIcons.IconDefinition
}

function FontAwesomeIcon(props: FontAwesomeIconProps) {
    return (
        <span
            className={
                'absolute left-0 top-0 flex items-center justify-center h-full w-10 ' +
                'text-blue-500'
            }
        >
            <fontawesome.FontAwesomeIcon icon={props.icon} />
        </span>
    )
}

export default FontAwesomeIcon
