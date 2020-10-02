---
layout: developer-doc
title: Shortcuts
category: product
tags: [product,ui]
---

### The <kbd>meta</kbd> key.
The <kbd>meta</kbd> key was introduced to make the shortcuts consistent across platforms. It is defined as <kbd>command</kbd> on macOS, and as <kbd>ctlr</kbd> on Windows and Linux.

### Graph Editor
| Shortcut | Action |
| -------- | ------ |
| <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>s</kbd> | Toggles light/dark application style. Currently doesn't work properly, as the Theme Switcher is not created yet. (https://github.com/enso-org/ide/issues/795)|

#### Node Editing
| Shortcut | Action |
| -------- | ------ |
| <kbd>shift</kbd>+<kbd>tab</kbd> | Show searcher for adding new node. |
| <kbd>ctrl</kbd>+<kbd>backspace</kbd> | Removes the selected nodes. |
| <kbd>ctrl</kbd>+<kbd>g</kbd> | Collapses the selected nodes. |
| <kbd>meta</kbd>+<kbd>left-mouse-button</kbd> | Start editing node expression. |
| <kbd>enter</kbd> when suggestion is selected, or <kbd>LMB</kbd> on suggestion | Pick selected suggestion and commit editing. |
| <kbd>enter</kbd> *when suggestion is not selected* | Commit editing with the current input. |

#### Navigation

| Shortcut | Action |
| -------- | ------ |
| <kbd>ctrl</kbd>+<kbd>enter</kbd> | Step into the last selected node.
| <kbd>ctrl</kbd>+<kbd>up</kbd> | Step out of the current node.


#### Node Selection

| Shortcut | Action |
| --- | --- |
| <kbd>left-mouse-button</kbd> click node name                          | Deselect all nodes. Select the target node. |
| <kbd>left-mouse-button</kbd> click background                         | Deselect all nodes. |
| <kbd>left-mouse-button</kbd> drag background                          | Deselect all nodes. Select nodes using selection-box. |
| <kbd>shift</kbd> + <kbd>left-mouse-button</kbd> click node name       | Add node to the selection group. |
| <kbd>shift</kbd> + <kbd>left-mouse-button</kbd> drag background       | Add nodes to the selection group. |
| <kbd>option</kbd> + <kbd>left-mouse-button</kbd> click node name      | Remove node from the selection group. |
| <kbd>option</kbd> + <kbd>left-mouse-button</kbd> drag background      | Remove nodes from the selection group. |
| <kbd>option</kbd> + <kbd>shift</kbd> + <kbd>LMB</kbd> click node name | Inverse node selection. |
| <kbd>option</kbd> + <kbd>shift</kbd> + <kbd>LMB</kbd> drag background | Inverse nodes selection. |



### Node dragging

| Shortcut | Action |
| -------- | ------ |
| left-mouse-button drag non-selected node name | Move the node to new position. |
| left-mouse-button drag selected node name     | Move all selected nodes the node to new positions. |



### Visualization

| Shortcut | Action |
| -------- | ------ |
| <kbd>cmd</kbd> / <kbd>ctrl</kbd> + <kbd>space</kbd> | Toggle visualization visibility of the selected node. |
| <kbd>cmd</kbd> / <kbd>ctrl</kbd> + <kbd>f</kbd>     | Cycle visualizations of the selected node. |
| <kbd>cmd</kbd> / <kbd>ctrl</kbd> + <kbd>\</kbd>     | Toggle documentation view visibility |

### Debug
| Shortcut | Action |
| -------- | ------ |
| <kbd>cmd</kbd> / <kbd>ctrl</kbd> + <kbd>d</kbd>     | Send test data to the selected node. |
| <kbd>cmd</kbd> / <kbd>ctrl</kbd> + <kbd>shift</kbd> + <kbd>enter</kbd> | Push a hardcoded breadcrumb without navigating. |
| <kbd>cmd</kbd> / <kbd>ctrl</kbd> + <kbd>shift</kbd> + <kbd>arrow up</kbd> | Pop a breadcrumb without navigating. |
