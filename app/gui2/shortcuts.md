## General Assumptions

#### The <kbd>Meta</kbd> key.

The <kbd>Meta</kbd> key was introduced to make the shortcuts consistent across
platforms. It is defined as <kbd>Command ⌘</kbd> on macOS, and as <kbd>Ctrl</kbd>
on Windows and Linux.

#### Mouse Buttons

Shortcuts are designed to work well with both the mouse and the touchpad.

- <kbd>LMB</kbd> corresponds to Left Mouse Button
- <kbd>MMB</kbd> corresponds to Middle Mouse Button
- <kbd>RMB</kbd> corresponds to Right Mouse Button

## Graph Editor

#### General Shortcuts

| Shortcut                                                                          | Action                         |
| --------------------------------------------------------------------------------- | ------------------------------ |
| <kbd>Escape</kbd>                                                                 | Cancel current interaction     |
| <kbd>Meta</kbd>+<kbd>`</kbd>                                                      | Show/hide Code Editor          |
| <kbd>Meta</kbd>+<kbd>D</kbd>                                                      | Show/hide Documentation Editor |
| <kbd>Meta</kbd>+<kbd>,</kbd>                                                      | Show Settings                  |
| <kbd>Meta</kbd>+<kbd>/</kbd>                                                      | Show About Window              |
| <kbd>Meta</kbd>+<kbd>Z</kbd>                                                      | Undo last action               |
| <kbd>Meta</kbd>+<kbd>Y</kbd> or <kbd>Meta</kbd> + <kbd>Shift</kbd> + <kbd>Z</kbd> | Redo last undone action        |

#### Navigation

| Shortcut                                          | Action                               |
| ------------------------------------------------- | ------------------------------------ |
| Drag gesture (two fingers)                        | Pan the scene.                       |
| Pinch gesture (two fingers)                       | Zoom the scene.                      |
| <kbd>MMB</kbd> drag                               | Pan the scene.                       |
| <kbd>RMB</kbd> drag                               | Zoom the scene.                      |
| <kbd>LMB</kbd> double press component             | Step into the component.             |
| <kbd>LMB</kbd> double press background            | Step out of the current component.   |
| <kbd>Meta</kbd>+<kbd>E</kbd>                      | Step in the last selected component. |
| <kbd>Meta</kbd>+<kbd>Shift</kbd>+<kbd>E</kbd>     | Step out of the current component.   |
| <kbd>Meta</kbd> + <kbd>Shift</kbd> + <kbd>A</kbd> | Zoom to selected components.         |

#### Component Layout

| Shortcut                                   | Action                                                                 |
| ------------------------------------------ | ---------------------------------------------------------------------- |
| <kbd>LMB</kbd> drag non-selected component | Move the component to new position (dragging do not modify selection). |
| <kbd>LMB</kbd> drag selected component     | Move all selected components the component to new positions.           |

#### Component Selection

| Shortcut                                                                             | Action                                                      |
| ------------------------------------------------------------------------------------ | ----------------------------------------------------------- |
| <kbd>LMB</kbd> click component                                                       | Deselect all components. Select the target component.       |
| <kbd>LMB</kbd> click background                                                      | Deselect all components.                                    |
| <kbd>LMB</kbd> drag background                                                       | Select components using selection-box.                      |
| <kbd>Shift</kbd> + <kbd>LMB</kbd> click component                                    | Add / remove component to the selection group.              |
| <kbd>Shift</kbd> + <kbd>LMB</kbd> drag background                                    | Add / remove components to the selection group.             |
| <kbd>Meta</kbd> + <kbd>A</kbd>                                                       | Select all components.                                      |
| <kbd>Escape</kbd>                                                                    | Deselect all components (if not in a mode, like edit mode). |
| <kbd>Meta</kbd> + <kbd>Shift</kbd> + <kbd>LMB</kbd> click component                  | Add component to the selection group.                       |
| <kbd>Meta</kbd> + <kbd>Shift</kbd> + <kbd>LMB</kbd> drag background                  | Add components to the selection group.                      |
| <kbd>Shift</kbd> + <kbd>Alt</kbd> + <kbd>LMB</kbd> click component                   | Remove component to the selection group.                    |
| <kbd>Shift</kbd> + <kbd>Alt</kbd> + <kbd>LMB</kbd> drag background                   | Remove components to the selection group.                   |
| <kbd>Meta</kbd> + <kbd>Shift</kbd> + <kbd>Alt</kbd> + <kbd>LMB</kbd> click component | Inverse component selection.                                |
| <kbd>Meta</kbd> + <kbd>Shift</kbd> + <kbd>Alt</kbd> + <kbd>LMB</kbd> drag background | Inverse components selection.                               |

#### Component Browser

| Shortcut                                         | Action                                                                        |
| ------------------------------------------------ | ----------------------------------------------------------------------------- |
| <kbd>Enter</kbd>                                 | Open component browser                                                        |
| <kbd>Tab</kbd>                                   | Apply currently selected suggestion (insert it without finishing the editing) |
| <kbd>Enter</kbd> or <kbd>LMB</kbd> on suggestion | Accept currently selected suggestion (insert it and finish editing)           |
| <kbd>Meta</kbd> + <kbd>Enter</kbd>               | Accept raw input and finish editing                                           |
| <kbd>↑</kbd>                                     | Move selection up                                                             |
| <kbd>↓</kbd>                                     | Move selection down                                                           |

#### Component Editing

| Shortcut                                          | Action                                |
| ------------------------------------------------- | ------------------------------------- |
| <kbd>Meta</kbd> + <kbd>C</kbd>                    | Copy selected components.             |
| <kbd>Meta</kbd> + <kbd>V</kbd>                    | Paste copied components.              |
| <kbd>BackSpace</kbd> or <kbd>Delete</kbd>         | Remove selected components.           |
| <kbd>Meta</kbd>+<kbd>G</kbd>                      | Collapse (group) selected components. |
| <kbd>Meta</kbd>+<kbd>LMB</kbd>                    | Start editing component expression.   |
| <kbd>Meta</kbd> + <kbd>Shift</kbd> + <kbd>C</kbd> | Change color of selected components.  |

#### Visualization

| Shortcut                            | Action                                                            |
| ----------------------------------- | ----------------------------------------------------------------- |
| <kbd>Space</kbd>                    | Toggle visualization visibility of the selected component.        |
| <kbd>Meta</kbd> hold                | Preview visualization of the hovered component (hide on release). |
| <kbd>Shift</kbd> + <kbd>Space</kbd> | Toggle visualization fullscreen mode.                             |
| <kbd>Escape</kbd>                   | Exit visualization fullscreen mode.                               |
| <kbd>Meta</kbd> + <kbd>Space</kbd>  | Cycle visualizations of the selected component.                   |
| <kbd>F1</kbd>                       | Open documentation view                                           |
