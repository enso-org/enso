---
layout: developer-doc
title: Shortcuts
category: product
tags: [product,ui]
---

## General Assumptions

#### The <kbd>meta</kbd> key.
The <kbd>meta</kbd> key was introduced to make the shortcuts consistent across platforms. 
It is defined as <kbd>command</kbd> on macOS, and as <kbd>ctrl</kbd> on Windows and Linux.

#### Keyboard-only Workflow
The GUI and all shortcuts were designed in a way to allow both efficient mouse-only as well as
keyboard-only workflows. In most cases, there is a relation between mouse and keyboard shortcuts,
namely, the `left-mouse-button` corresponds to `enter`. For example, stepping into a node is done
by either double clicking the node, or just pressing the enter key.

#### Missing / not working shortcuts
Some of the shortcuts presented below are marked with the :warning: icon, which means, that they are
planned, but not yet implemented. Feel free to contribute and help us implement them!

Shortcuts marked with the :bangbang: icon should work, but are reported to be broken and require
further investigation.



## Graph Editor

#### General Shortcuts
| Shortcut | Action |
| -------- | ------ |
| <kbd>cmd</kbd>+<kbd>alt</kbd>+<kbd>shift</kbd>+<kbd>t</kbd> | Toggle light/dark application style. Currently doesn't work properly, as the Theme Switcher is not created yet. (https://github.com/enso-org/ide/issues/795)|
| <kbd>ctrl</kbd>+<kbd>`</kbd>                                | Show Code Editor |
| <kbd>meta</kbd>+<kbd>s</kbd>                                | Save module |


#### Navigation
| Shortcut | Action |
| -------- | ------ |
| Drag gesture (two fingers)             | Pan the scene.
| Pinch gesture (two fingers)            | Zoom the scene.
| <kbd>MMB</kbd> drag                    | Pan the scene.
| <kbd>RMB</kbd> drag                    | Zoom the scene.
| <kbd>LMB</kbd> double press node name  | Step into the node.
| :warning: <kbd>LMB</kbd> double press background | Step out of the current node.
| <kbd>enter</kbd>                       | Step in the last selected node.
| <kbd>alt</kbd>+<kbd>enter</kbd>        | Step out of the current node.


#### Node Layout
| Shortcut | Action |
| -------- | ------ |
| <kbd>LMB</kbd> drag non-selected node name | Move the node to new position (dragging do not modify selection). |
| <kbd>LMB</kbd> drag selected node name     | Move all selected nodes the node to new positions. |


#### Node Selection
| Shortcut | Action |
| --- | --- |
| <kbd>LMB</kbd> click node name                                                       | Deselect all nodes. Select the target node. |
| <kbd>LMB</kbd> click background                                                      | Deselect all nodes. |
| :warning: <kbd>LMB</kbd> drag background                                             | Select nodes using selection-box. |
| <kbd>shift</kbd> + <kbd>LMB</kbd> click node name                                    | Add / remove node to the selection group. |
| :warning: <kbd>shift</kbd> + <kbd>LMB</kbd> drag background                          | Add / remove nodes to the selection group. |
| :warning: <kbd>*-arrow</kbd>                                                         | Select node on the right side of the newest selected node. |
| :warning: <kbd>cmd</kbd> + <kbd>a</kbd>                                              | Select all nodes. |
| :warning: <kbd>escape</kbd>                                                          | Deselect all nodes (if not in a mode, like edit mode). |
| <kbd>shift</kbd> + <kbd>ctrl</kbd> + <kbd>LMB</kbd> click node name                  | Add node to the selection group. |
| :warning: <kbd>shift</kbd> + <kbd>ctrl</kbd> + <kbd>LMB</kbd> drag background        | Add nodes to the selection group. |
| <kbd>shift</kbd> + <kbd>alt</kbd> + <kbd>LMB</kbd> click node name                   | Remove node to the selection group. |
| :warning: <kbd>shift</kbd> + <kbd>alt</kbd> + <kbd>LMB</kbd> drag background         | Remove nodes to the selection group. |
| <kbd>shift</kbd> + <kbd>ctrl</kbd> + <kbd>alt</kbd> + <kbd>LMB</kbd> click node name | Inverse node selection. |
| :warning: <kbd>shift</kbd> + <kbd>ctrl</kbd> + <kbd>alt</kbd> + <kbd>LMB</kbd> drag background | Inverse nodes selection. |


#### Node Editing
| Shortcut | Action |
| -------- | ------ |
| <kbd>tab</kbd>                                   | Show / hide node searcher. |
| <kbd>backspace</kbd>                             | Remove selected nodes. |
| <kbd>cmd</kbd>+<kbd>g</kbd>                      | Collapse (group) selected nodes. |
| <kbd>meta</kbd>+<kbd>LMB</kbd>                   | Start editing node expression. |
| <kbd>meta</kbd>+<kbd>enter</kbd>                 | Start editing node expression. |
| <kbd>enter</kbd> or <kbd>LMB</kbd> on suggestion | Pick selected suggestion and commit editing. |


#### Visualization
| Shortcut | Action |
| -------- | ------ |
| <kbd>space</kbd>                         | Toggle visualization visibility of the selected node. |
| <kbd>space</kbd> hold                    | Preview visualization of the selected node (hide on release). |
| <kbd>space</kbd> double press            | Toggle visualization fullscreen mode (with improper depth sorting now). |
| <kbd>ctrl</kbd> + <kbd>space</kbd>       | Cycle visualizations of the selected node. |
| :bangbang: <kbd>cmd</kbd> + <kbd>\</kbd> | Toggle documentation view visibility |


#### Debug
| Shortcut | Action |
| -------- | ------ |
| <kbd>ctrl</kbd> + <kbd>d</kbd>                           | Send test data to the selected node. |
| <kbd>ctrl</kbd> + <kbd>shift</kbd> + <kbd>enter</kbd>    | Push a hardcoded breadcrumb without navigating. |
| <kbd>ctrl</kbd> + <kbd>shift</kbd> + <kbd>arrow up</kbd> | Pop a breadcrumb without navigating. |
