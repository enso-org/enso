//! Shortcuts used in the graph editor.

use ensogl::application::shortcut::ActionType::*;



// =======================================
// === Shortcuts for the graph editor. ===
// =======================================

/// The list of all shortcuts used in the graph editor.
pub const SHORTCUTS: &[(ensogl::application::shortcut::ActionType, &str, &str, &str)] = &[
    (Press, "!node_editing & !read_only", "tab", "start_node_creation"),
    (Press, "!node_editing & !read_only", "enter", "start_node_creation"),
    // === Drag ===
    (Press, "", "left-mouse-button", "node_press"),
    (Release, "", "left-mouse-button", "node_release"),
    (Press, "!node_editing & !read_only", "backspace", "remove_selected_nodes"),
    (Press, "!node_editing & !read_only", "delete", "remove_selected_nodes"),
    (Press, "has_detached_edge", "escape", "drop_dragged_edge"),
    (Press, "!read_only", "cmd g", "collapse_selected_nodes"),
    // === Visualization ===
    (Press, "!node_editing", "space", "press_visualization_visibility"),
    (DoublePress, "!node_editing", "space", "double_press_visualization_visibility"),
    (Release, "!node_editing", "space", "release_visualization_visibility"),
    (Press, "", "cmd i", "reload_visualization_registry"),
    (Press, "is_fs_visualization_displayed", "space", "close_fullscreen_visualization"),
    (Press, "", "cmd", "enable_quick_visualization_preview"),
    (Release, "", "cmd", "disable_quick_visualization_preview"),
    // === Selection ===
    (Press, "", "shift", "enable_node_multi_select"),
    (Press, "", "shift left-mouse-button", "enable_node_multi_select"),
    (Release, "", "shift", "disable_node_multi_select"),
    (Release, "", "shift left-mouse-button", "disable_node_multi_select"),
    (Press, "", "shift ctrl", "toggle_node_merge_select"),
    (Release, "", "shift ctrl", "toggle_node_merge_select"),
    (Press, "", "shift alt", "toggle_node_subtract_select"),
    (Release, "", "shift alt", "toggle_node_subtract_select"),
    (Press, "", "shift ctrl alt", "toggle_node_inverse_select"),
    (Release, "", "shift ctrl alt", "toggle_node_inverse_select"),
    // === Navigation ===
    (
        Press,
        "!is_fs_visualization_displayed",
        "ctrl space",
        "cycle_visualization_for_selected_node",
    ),
    (DoublePress, "!read_only", "left-mouse-button", "enter_hovered_node"),
    (DoublePress, "!read_only", "left-mouse-button", "start_node_creation_from_port"),
    (Press, "!read_only", "right-mouse-button", "start_node_creation_from_port"),
    (Press, "!node_editing & !read_only", "cmd enter", "enter_selected_node"),
    (Press, "!read_only", "alt enter", "exit_node"),
    // === Node Editing ===
    (Press, "!read_only", "cmd", "edit_mode_on"),
    (Release, "!read_only", "cmd", "edit_mode_off"),
    (Press, "!read_only", "cmd left-mouse-button", "edit_mode_on"),
    (Release, "!read_only", "cmd left-mouse-button", "edit_mode_off"),
    (Press, "node_editing & !read_only", "cmd enter", "stop_editing"),
    // === Profiling Mode ===
    (Press, "", "cmd p", "toggle_profiling_mode"),
    // === Debug ===
    (Press, "debug_mode", "ctrl d", "debug_set_test_visualization_data_for_selected_node"),
    (Press, "debug_mode", "ctrl shift enter", "debug_push_breadcrumb"),
    (Press, "debug_mode", "ctrl shift up", "debug_pop_breadcrumb"),
    (Press, "debug_mode", "ctrl n", "add_node_at_cursor"),
    // === Execution Mode ===
    (Press, "", "shift ctrl e", "toggle_execution_environment"),
];
