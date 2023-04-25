//! # Display Objects
//! Display objects are essential structures used to build elements visible on the screen. They are
//! used to build objects hierarchy, computing elements transformations within this hierarchy
//! (position, rotation, and scale), passing events trough that hierarchy, and layouting the
//! elements on the screen (e.g. with horizontal or vertical layout). The implementation is very
//! performance-oriented, it tracks the transformation changes and updates only the needed
//! subset of the display object tree on demand.
//!
//! ## Lazy updates of display objects
//! Some operations on display objects are very expensive. For example, after moving the root object
//! of a hierarchy, the matrix transformations of all its children, their children, etc. need to be
//! updated. That's why these operations are performed in a lazy way. After an element is
//! transformed, or when the hierarchy is modified, the change information is propagated up to the
//! root of the hierarchy and is updated once per frame, after the [`update`] function is called
//! (usually, it is called by the [`Scene`]). Emitting events is not done in a lazy fashion, as they
//! do not require passing the event down the hierarchy. Instead, the event is passed up the
//! hierarchy, from the object the event was emitted on all way to the root of the hierarchy.
//!
//! ## Scene Layers
//! Every display object can be assigned to a [`scene::Layer`]. During object update, the assignment
//! information is passed down the hierarchy. If an object was not assigned to a layer explicitly,
//! it will inherit the assignment from its parent object, if any. This means that adding an object
//! to a layer will also move all of its children there, until they are assigned with a different
//! layer explicitly.
//!
//!
//! ## Terminology and Concepts
//! Before diving into the concepts of Grid it’s important to understand the terminology. Since the
//! terms involved here are all kinda conceptually similar, it’s easy to confuse them with one
//! another if you don’t first memorize their meanings defined by the Grid specification.
//!
//! - **Grid container:** the display object with enabled auto-layout. It is the element that
//!   contains all the grid items (children of the display object).
//!
//! - **Grid item:** the child (i.e. direct descendants) of the grid container. A grid item can be
//!   any display object.
//!
//! - **Grid line:** the dividing lines that make up the structure of the grid. They can be either
//!   vertical (“column grid lines”) or horizontal (“row grid lines”) and reside on either side of a
//!   row or column.
//!
//! - **Grid cell:** the space between two adjacent row and two adjacent column grid lines. It’s a
//!   single “unit” of the grid.
//!
//! - **Grid track:** the space between two adjacent grid lines. You can think of them as the
//!   columns or rows of the grid.
//!
//! - **Grid area:** the total space surrounded by any four grid lines. A grid area may be composed
//!   of any number of grid cells.
//!
//!
//!
//! ## Setting elements sizes.
//! Each display object can be assigned with a horizontal and vertical size. By default, the size is
//! set to 'hug', which means that it will be resolved to the content's size. Alternatively, the
//! size can be set to a fixed value expressed in one of the units: pixels, percent of the parent
//! container size, or the fraction of the leftover space in the container. These units will be
//! covered in the following chapters. For now, let's focus on the 'hug' and fixed pixel sizes.
//!
//! You can set the display object size by using the [`set_size`] function family. Here is an
//! example of a display object with width of 10 pixels and height set to 'hug' it's children (the
//! default value). In case of display objects that do not contain any children, the hug size will
//! be resolved to 0. The hug resizing is indicated by the `▶` and `◀` symbols in the following
//! illustrations.
//!
//! ```
//! // ╭ root ─╮
//! // │       ▼
//! // │       ▲
//! // ╰───────╯
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! root.set_size_x(10.0);
//! ```
//!
//!
//! ## Automatic creation of columns and rows.
//! Each display object can be asked to automatically place its children by using the Grid layout.
//! The layout is divided into columns and rows. Every Grid has at least one column and one row. To
//! enable the Grid layout, use the [`use_auto_layout`] method. Here is an example of a display
//! object with an empty Grid layout:
//!
//! ```
//! // ╔ root ▶ ◀ ═╗
//! // ║ ╭─ ▶ ◀ ─╮ ║
//! // ║ │       ▼ ▼
//! // ║ │       ▲ ▲
//! // ║ ╰───────╯ ║
//! // ╚═══════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! root.use_auto_layout();
//! ```
//!
//! The above illustration contains visualization of both the Grid container and grid cells. The
//! container is drawn with bold, double lines, while the cells use thin, single lines. All the Grid
//! dimensions are set to 'hug' by default, including the width and height of the container, widths
//! of columns and heights of rows. An empty Grid layout with hug resizing will be resolved to a
//! zero-sized object.
//!
//! You do not need to define columns explicitly. They will be added automatically, one for every
//! child. Columns with the 'hug' width will inherit some properties of their children, like their
//! grow factors. The details will be covered in the further sections. The following code defines a
//! new Grid layout with two children. The children sizes are set manually, while the container,
//! columns, and rows sizes are set to 'hug' (default):
//!
//! ```
//! // ╔ root ═══════ ▶ ◀ ═════════════╗
//! // ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────╮ ║
//! // ║ │             ┆  ╭ node2 ╮  │ ║
//! // ║ │  ╭ node1 ╮  ┆  │       │  ▼ ▼
//! // ║ │  │       │  ┆  │       │  ▲ ▲
//! // ║ │  ╰───────╯  ┆  ╰───────╯  │ ║
//! // ║ ╰─────────────┴─────────────╯ ║
//! // ╚═══════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! root.use_auto_layout();
//! node1.set_size((10.0, 10.0));
//! node2.set_size((10.0, 15.0));
//! ```
//!
//! By default, the Grid layout will create as many columns as there are children. You can limit the
//! maximum number of used columns with the [`set_column_count`] method. You can also define the
//! properties of every column. This topic will be covered in the upcoming sections. The following
//! code defines a new Grid layout with a maximum number of 2 columns:
//!
//! ```
//! // ╔ root ═══════ ▶ ◀ ═════════════╗
//! // ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────╮ ║
//! // ║ │  ╭ node3 ╮  ┆             │ ║
//! // ║ │  │       │  ┆             ▼ ║
//! // ║ │  │       │  ┆             ▲ ║
//! // ║ │  ╰───────╯  ┆             │ ▼
//! // ║ ├╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌┤ ▲
//! // ║ │  ╭ node1 ╮  ┆  ╭ node2 ╮  │ ║
//! // ║ │  │       │  ┆  │       │  ▼ ║
//! // ║ │  │       │  ┆  │       │  ▲ ║
//! // ║ │  ╰───────╯  ┆  ╰───────╯  │ ║
//! // ║ ╰─────────────┴─────────────╯ ║
//! // ╚═══════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! root.use_auto_layout().set_column_count(2);
//! node1.set_size((2.0, 2.0));
//! node2.set_size((2.0, 2.0));
//! node3.set_size((2.0, 2.0));
//! ```
//!
//!
//! ## Growing and shrinking objects.
//! Beside a size, each display object can be assigned with a growth/shrink factor and a
//! maximum/minimum size. These properties describe how the object should be stretched by the parent
//! container if there is either a free space left or there is not enough space. You can use the
//! following methods to control these properties: [`set_grow_factor`], [`set_shrink_factor`],
//! [`set_max_size`], and [`set_min_size`]. Please note, that if an object with a hug resizing can
//! grow and there is enough of free space, it will. Growing is performed after hugging the content.
//!
//! If there are several objects that can grow, the leftover space will be distributed to them by
//! weight computed as `item_grow_factor / sum_of_all_grow_factors`. Analogously, if there are
//! several objects that can shrink, their sizes will be decreased by weight computed as
//! `item_shrink_factor / sum_of_all_shrink_factors`.
//!
//! For convenience, there are also shortcut methods defined. The [`allow_grow`] and
//! [`allow_shrink`] set the grow and shrink factors to 1.0, respectively. The following code
//! constructs a display object with 10 pixels width and height. The height shrink factor is set to
//! 1, width grow factor set to 1, and the maximum width set to 15 pixels:
//!
//! ```
//! // ╭ root ───┬───▷┤
//! // │         │
//! // │         │ 10
//! // │         △
//! // ╰─────────╯
//! //   10
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! root.set_size((10.0, 10.0));
//! root.allow_shrink_y();
//! root.allow_grow_x();
//! root.set_max_size_x(15.0);
//! ```
//!
//! Please note that setting the Grid container size does not influence the children placement if
//! the columns are set to 'hug':
//!
//! ```
//! // ╔ root ══════════════════════════════════════╗
//! // ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────╮              ║
//! // ║ │             ┆  ╭ node2 ╮  │              ║
//! // ║ │  ╭ node1 ╮  ┆  │       │  ▼              ▼
//! // ║ │  │       │  ┆  │       │  ▲              ▲
//! // ║ │  ╰───────╯  ┆  ╰───────╯  │              ║
//! // ║ ╰─────────────┴─────────────╯              ║
//! // ╚════════════════════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! root.use_auto_layout();
//! root.set_size_x(30.0);
//! node1.set_size((10.0, 10.0));
//! node2.set_size((10.0, 15.0));
//! ```
//!
//! However, if the child size is allowed to grow or shrink, the column will inherit this property:
//!
//! ```
//! // ╔ root ═════════════════════════════════════╗
//! // ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ───────────────╮  ║
//! // ║ │             ┆  ╭ node2 ───────────┬▷ │  ║
//! // ║ │  ╭ node1 ╮  ┆  │                  │  ▼  ▼
//! // ║ │  │       │  ┆  │                  │  ▲  ▲
//! // ║ │  ╰───────╯  ┆  ╰──────────────────╯  │  ║
//! // ║ ╰─────────────┴────────────────────────╯  ║
//! // ╚═══════════════════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! root.use_auto_layout();
//! root.set_size_x(30.0);
//! node1.set_size((10.0, 10.0));
//! node2.set_size((10.0, 15.0));
//! node2.allow_grow_x();
//! ```
//!
//!
//!
//! ## Manual creation of columns and rows.
//! While automatic creation of columns and rows is convenient, manual creation provides much more
//! flexibility and allows for more complex layout definitions. You can add a new column/row with
//! the [`add_column`] and [`add_row`] methods. You can access a column/row by index with the
//! [`column`] and [`row`] methods, respectively. Also, as the first column and row always exist,
//! there are [`first_column`] and [`first_row`] methods provided. All these functions return a
//! column/row reference which allows changing the following properties:
//!
//! - **Size:** the width of the column and height of the row. The size can be set to either 'hug'
//!   or a fixed value. The value can be expressed in pixels, percentage of the parent container
//!   size, or by using fractional units. All of these units will be described later.
//!
//! - **Minimum size:** the minimum size of the column/row. If the column/row size does not fit the
//!   parent container, the layout system will try to shrink it, but will not shrink it more than
//!   expressed by this value.
//!
//! - **Maximum size:** the maximum size of the column/row. If the column/row grow factor was set a
//!   positive value, the object will grow, but not more than expressed by this value.
//!
//! - **Grow factor:** a number describing how much the column/row should grow if there is extra
//!   space after first layout step. The space will be distributed among all elements that can grow
//!   by using the grow factors as the distribution weights.
//!
//! - **Shrink factor:** a number describing how much the column/row should shrink if there is not
//!   enough space after first layout step. All elements that can shrink will be resized by using
//!   the shrink factors as the resizing weights.
//!
//! ```
//! // ╔ root ═══════ ▶ ◀ ══════════════════════════╗
//! // ║ ╭──── ▶ ◀ ────┬────────── ▶ ◀ ──────────┬▷ ║
//! // ║ │             ┆  ╭ node2 ╮              │  ║
//! // ║ │  ╭ node1 ╮  ┆  │       │              ▼  ▼
//! // ║ │  │       │  ┆  │       │              ▲  ▲
//! // ║ │  ╰───────╯  ┆  ╰───────╯              │  ║
//! // ║ ╰─────────────┴─────────────────────────╯  ║
//! // ╚════════════════════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! root.use_auto_layout();
//! root.add_column().allow_grow();
//! root.set_size_x(30.0);
//! node1.set_size((10.0, 10.0));
//! node2.set_size((10.0, 15.0));
//! ```
//!
//!
//! ## Cycling columns/rows definitions.
//! In case there are more columns/rows needed than defined, the defined columns/rows will be used
//! in a loop. For example, you can define a layout where every second column is bigger:
//!
//! ```
//! // ╔ root ════════════════════════════ ▶ ◀ ══════════════════════════════════╗
//! // ║ ╭─────────────┬────────────────────┬─────────────┬────────────────────╮ ║
//! // ║ │             ┆  ╭ node2 ───────┬▷ ┆             ┆  ╭ node4 ───────┬▷ │ ║
//! // ║ │  ╭ node1 ┬▷ ┆  │              │  ┆  ╭ node3 ┬▷ ┆  │              │  ▼ ▼
//! // ║ │  │       │  ┆  │              │  ┆  │       │  ┆  │              │  ▲ ▲
//! // ║ │  ╰───────╯  ┆  ╰──────────────╯  ┆  ╰───────╯  ┆  ╰──────────────╯  │ ║
//! // ║ ╰─────────────┴────────────────────┴─────────────┴────────────────────╯ ║
//! // ╚═════════════════════════════════════════════════════════════════════════╝
//! //          2                4                 2                4             
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! let node4 = root.new_child();
//! root.use_auto_layout();
//! root.first_column().set_size(2.0);
//! root.add_column().set_size(4.0);
//! node1.set_size_y(2.0).allow_grow_x();
//! node2.set_size_y(3.0).allow_grow_x();
//! node3.set_size_y(2.0).allow_grow_x();
//! node4.set_size_y(3.0).allow_grow_x();
//! ```
//!
//!
//! ## Item alignment.
//! The item alignment settings allow aligning grid items along both the column and row axis. You
//! can set the default alignment of all children by using the [`set_children_alignment`] on the
//! container, or override it per child using the [`set_alignment`] method.
//!
//! ```
//! // ╔ root ════════════ ▶ ◀ ══════════════════╗
//! // ║ ╭──────────────────┬──────────────────╮ ║
//! // ║ │  ╭ node3 ╮       ┆                  │ ║
//! // ║ │  │       │       ┆                  ▼ ║
//! // ║ │  │       │       ┆                  ▲ ║
//! // ║ │  ╰───────╯       ┆                  │ ▼
//! // ║ ├╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┤ ▲
//! // ║ │       ╭ node1 ╮  ┆       ╭ node2 ╮  │ ║
//! // ║ │       │       │  ┆       │       │  ▼ ║
//! // ║ │       │       │  ┆       │       │  ▲ ║
//! // ║ │       ╰───────╯  ┆       ╰───────╯  │ ║
//! // ║ ╰──────────────────┴──────────────────╯ ║
//! // ╚═════════════════════════════════════════╝
//! //           10                 10         
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! root
//!     .use_auto_layout()
//!     .set_column_count(2)
//!     .set_children_alignment_right_bottom();
//! root.first_column().set_size(10.0);
//! node1.set_size((2.0, 2.0));
//! node2.set_size((2.0, 2.0));
//! node3.set_size((2.0, 2.0));
//! node3.set_alignment_left();
//! ```
//!
//! Please note, that alignment works only if there is a space left in a column. In the case of
//! columns that simply hug the children and the children in the given column have the same size,
//! the alignment would not make any effect even if the container size is larger than columns:
//!
//! ```
//! // ╔ root ════════════════ ▶ ◀ ═══════════════════╗
//! // ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────╮                ║
//! // ║ │  ╭ node3 ╮  ┆             │                ║
//! // ║ │  │       │  ┆             ▼                ║
//! // ║ │  │       │  ┆             ▲                ║
//! // ║ │  ╰───────╯  ┆             │                ▼
//! // ║ ├╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌┤                ▲
//! // ║ │  ╭ node1 ╮  ┆  ╭ node2 ╮  │                ║
//! // ║ │  │       │  ┆  │       │  ▼                ║
//! // ║ │  │       │  ┆  │       │  ▲                ║
//! // ║ │  ╰───────╯  ┆  ╰───────╯  │                ║
//! // ║ ╰─────────────┴─────────────╯                ║
//! // ╚══════════════════════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! root.set_size_x(20.0);
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! root
//!     .use_auto_layout()
//!     .set_column_count(2)
//!     .set_children_alignment_right_bottom();
//! node1.set_size((2.0, 2.0));
//! node2.set_size((2.0, 2.0));
//! node3.set_size((2.0, 2.0));
//! node3.set_alignment_left();
//! ```
//!
//!
//! ## Reversing the column/row item order.
//! It is possible to reverse the columns and rows order by using the [`reverse_columns`] and
//! [`reverse_rows`] functions. By default, items are placed along the axes, from left to right and
//! from bottom to top. The following example places the items from right to left:
//!
//! ```
//! // ╔ root ═══════ ▶ ◀ ═════════════╗
//! // ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────╮ ║
//! // ║ │             ┆  ╭ node3 ╮  │ ║
//! // ║ │             ┆  │       │  ▼ ║
//! // ║ │             ┆  │       │  ▲ ║
//! // ║ │             ┆  ╰───────╯  │ ▼
//! // ║ ├╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌┤ ▲
//! // ║ │  ╭ node2 ╮  ┆  ╭ node1 ╮  │ ║
//! // ║ │  │       │  ┆  │       │  ▼ ║
//! // ║ │  │       │  ┆  │       │  ▲ ║
//! // ║ │  ╰───────╯  ┆  ╰───────╯  │ ║
//! // ║ ╰─────────────┴─────────────╯ ║
//! // ╚═══════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! root
//!     .use_auto_layout()
//!     .set_column_count(2)
//!     .reverse_columns();
//! node1.set_size((2.0, 2.0));
//! node2.set_size((2.0, 2.0));
//! node3.set_size((2.0, 2.0));
//! ```
//!
//!
//! ## Grid flow.
//! The grid flow defines how the next item is placed in context of the previous one. The default
//! flow is 'row', which means that the next item will be placed in the same row as the previous one
//! if this is possible, e.g. if it was not limited by the [`set_column_count`] method. There are
//! other flow modes available:
//!
//! - **Row:** The default flow. Items are placed in a row as long as there is space before moving
//!   to the next row.
//! - **Column:** Items are placed in a column as long as there is space before moving to the next
//!   column.
//! - **Dense:** (not implemented yet): tells the auto-placement algorithm to attempt to fill in
//!   holes earlier in the grid if smaller items come up later. See the CSS grid spec for more
//!   info of how this mode will work: https://css-tricks.com/snippets/css/complete-guide-grid.
//!
//! For example, the following code will place items in a column and then move to the next column
//! after the maximum column count was reached:
//!
//! ```
//! // ╔ root ═══════ ▶ ◀ ═════════════╗
//! // ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────╮ ║
//! // ║ │  ╭ node3 ╮  ┆             │ ║
//! // ║ │  │       │  ┆             ▼ ║
//! // ║ │  │       │  ┆             ▲ ║
//! // ║ │  ╰───────╯  ┆             │ ║
//! // ║ ├╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌┤ ║
//! // ║ │  ╭ node2 ╮  ┆             │ ║
//! // ║ │  │       │  ┆             ▼ ▼
//! // ║ │  │       │  ┆             ▲ ▲
//! // ║ │  ╰───────╯  ┆             │ ║
//! // ║ ├╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌┤ ║
//! // ║ │  ╭ node1 ╮  ┆  ╭ node4 ╮  │ ║
//! // ║ │  │       │  ┆  │       │  ▼ ║
//! // ║ │  │       │  ┆  │       │  ▲ ║
//! // ║ │  ╰───────╯  ┆  ╰───────╯  │ ║
//! // ║ ╰─────────────┴─────────────╯ ║
//! // ╚═══════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! let node4 = root.new_child();
//! let node1 = node1.set_size((2.0, 2.0));
//! let node2 = node2.set_size((2.0, 2.0));
//! let node3 = node3.set_size((2.0, 2.0));
//! let node4 = node4.set_size((2.0, 2.0));
//! root.use_auto_layout().set_row_count(3).set_column_flow();
//! ```
//!
//!
//! ## The column/row gap.
//! The gap between columns/rows specifies the size of the grid lines. You can think of it like
//! setting the width of the gutters between the columns/rows. The gutters are only created between
//! the columns/rows, not on the outer edges. For example, the following code sets the gap of two
//! pixels between all the elements:
//!
//! ```
//! // ╔ root ═════════ ▶ ◀ ═══════════════╗
//! // ║ ╭───── ▶ ◀ ─────┬───── ▶ ◀ ─────╮ ║
//! // ║ │  ╭ node3 ╮  ╱╱┆╱╱             │ ║
//! // ║ │  │       │  ╱╱┆╱╱             ▼ ║
//! // ║ │  │       │  ╱╱┆╱╱             ▲ ║
//! // ║ │  ╰───────╯  ╱╱┆╱╱             │ ║
//! // ║ │╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱┆╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱│ ▼
//! // ║ ├╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┤ ▲
//! // ║ │╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱┆╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱│ ║
//! // ║ │  ╭ node1 ╮  ╱╱┆╱╱  ╭ node2 ╮  │ ║
//! // ║ │  │       │  ╱╱┆╱╱  │       │  ▼ ║
//! // ║ │  │       │  ╱╱┆╱╱  │       │  ▲ ║
//! // ║ │  ╰───────╯  ╱╱┆╱╱  ╰───────╯  │ ║
//! // ║ ╰───────────────┴───────────────╯ ║
//! // ╚═══════════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! root
//!     .use_auto_layout()
//!     .set_column_count(2)
//!     .set_gap((2.0, 2.0));
//! node1.set_size((2.0, 2.0));
//! node2.set_size((2.0, 2.0));
//! node3.set_size((2.0, 2.0));
//! ```
//!
//!
//! ## Grid padding.
//! The padding specifies the size inside the container that should be left empty. You can set the
//! padding by using the [`set_padding`] method. The following code displays three objects
//! within the padded container:
//!
//! ```
//! // ╔ root ═══════════════ ▶ ◀ ═════════════════════╗
//! // ║ ╭───── ▶ ◀ ─────┬─── ▶ ◀ ───┬───── ▶ ◀ ─────╮ ║
//! // ║ │ ╱╱╱╱╱╱╱╱╱╱╱╱╱╱┆╱╱╱╱╱╱╱╱╱╱╱┆╱╱╱╱╱╱╱╱╱╱╱╱╱╱ │ ║
//! // ║ │ ╱╱╱           ┆           ┆ ╭ node3 ╮ ╱╱╱ │ ║
//! // ║ │ ╱╱╱           ┆ ╭ node2 ╮ ┆ │       │ ╱╱╱ │ ║
//! // ║ │ ╱╱╱ ╭ node1 ╮ ┆ │       │ ┆ │       │ ╱╱╱ ▼ ▼
//! // ║ │ ╱╱╱ │       │ ┆ │       │ ┆ │       │ ╱╱╱ ▲ ▲
//! // ║ │ ╱╱╱ ╰───────╯ ┆ ╰───────╯ ┆ ╰───────╯ ╱╱╱ │ ║
//! // ║ │ ╱╱╱╱╱╱╱╱╱╱╱╱╱╱┆╱╱╱╱╱╱╱╱╱╱╱┆╱╱╱╱╱╱╱╱╱╱╱╱╱╱ │ ║
//! // ║ ╰───────────────┴───────────┴───────────────╯ ║
//! // ╚═══════════════════════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! node1.set_size((2.0, 2.0));
//! node2.set_size((2.0, 3.0));
//! node3.set_size((2.0, 4.0));
//! root.use_auto_layout().set_padding_all(10.0);
//! ```
//!
//!
//! ## Item's margin.
//! The margin specifies the size outside an item that should be left empty. You can set the margin
//! by using the [`set_margin`] method. The following code displays three objects where the second
//! object uses margin:
//!
//! ```
//! // ╔ root ═════════════════ ▶ ◀ ══════════════════════╗
//! // ║ ╭──── ▶ ◀ ───┬──────── ▶ ◀ ────────┬─── ▶ ◀ ───╮ ║
//! // ║ │            ┆ ╱╱╱╱╱╱           ╱╱ ┆ ╭ node3 ╮ │ ║
//! // ║ │            ┆ ╱╱╱╱╱╱ ╭ node2 ╮ ╱╱ ┆ │       │ │ ║
//! // ║ │  ╭ node1 ╮ ┆ ╱╱╱╱╱╱ │       │ ╱╱ ┆ │       │ ▼ ▼
//! // ║ │  │       │ ┆ ╱╱╱╱╱╱ │       │ ╱╱ ┆ │       │ ▲ ▲
//! // ║ │  │       │ ┆ ╱╱╱╱╱╱ │       │ ╱╱ ┆ │       │ │ ║
//! // ║ │  ╰───────╯ ┆ ╱╱╱╱╱╱ ╰───────╯ ╱╱ ┆ ╰───────╯ │ ║
//! // ║ ╰────────────┴─────────────────────┴───────────╯ ║
//! // ╚══════════════════════════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! node1.set_size((2.0, 2.0));
//! node2.set_size((2.0, 3.0));
//! node3.set_size((2.0, 4.0));
//! node2.set_margin_left(2.0);
//! node2.set_margin_right(1.0);
//! root.use_auto_layout();
//! ```
//!
//!
//! ## Percentage size units.
//! Sizes of all dimensions (element sizes, margins, paddings, column widths, row heights, etc.) can
//! be expressed as a percentage value of the parent container's size. Numbers are equipped with a
//! `.pc()` method which converts them to a percentage value. For example, the following code
//! creates a container with two children. The first grows as much as possible, while the
//! second occupies 30% of the parent container. Given the container width of 10 pixels, the width
//! of the first child will be resolved to 7 pixels, while the second will be resolved to 3 pixels:
//!
//! ```
//! // ╔ root ══════════════════════════════╗
//! // ║ ╭─────── ▶ ◀ ────────┬─── ▶ ◀ ───╮ ║
//! // ║ │                    ┆ ╭ node2 ╮ │ ║
//! // ║ │  ╭ node1 ───────┬▷ ┆ │       │ ▼ ▼
//! // ║ │  │              │  ┆ │       │ ▲ ▲
//! // ║ │  │      0       │  ┆ │  30%  │ │ ║
//! // ║ │  ╰──────────────╯  ┆ ╰───────╯ │ ║
//! // ║ ╰────────────────────┴───────────╯ ║
//! // ╚════════════════════════════════════╝
//! //                  10
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! node1.set_size((0.0, 2.0));
//! node2.set_size((30.pc(), 3.0));
//! root.use_auto_layout();
//! ```
//!
//!
//! ## Fractional size units (flexible length).
//! Sizes of all dimensions (element sizes, margins, paddings, column widths, row heights, etc.) can
//! be expressed as a fraction of the leftover space in the parent container. Numbers are equipped
//! with an `.fr()` method which converts them to a fractional value.
//!
//! The distribution of leftover space occurs after all non-flexible track sizing functions have
//! reached their maximum. The total size of such rows or columns is subtracted from the available
//! space, yielding the leftover space, which is then divided among the flex-sized rows and columns
//! in proportion to their fractional size. Each column or row’s share of the leftover space can be
//! computed as the column or row’s `fractional_size * leftover_space /
//! sum_of_all_fractional_sizes`.
//!
//! The following code creates a container with three children. The second child will occupy 10% of
//! the parent container, while the first child will be 2 times wider than the third one. Given the
//! parent container width of 20 pixels, the second child size will be resolved to `10% *  20px =
//! 2px`. The leftover space is `20px - 2px = 18px`. The sum of all fractional sizes is 3. The first
//! child size will be resolved to `2fr/3fr * 18px = 12px`, while the last child size will be
//! resolved to `1fr/3fr * 18px = 6px`.
//!
//! ```
//! // ╔ root ═══════════════════════ ▶ ◀ ═════════════════════════════╗
//! // ║ ╭───────────── ▶ ◀ ────────────┬─── ▶ ◀ ───┬────── ▶ ◀ ─────╮ ║
//! // ║ │                              ┆ ╭ node2 ╮ ┆ ╭ node3 ─────╮ │ ║
//! // ║ │  ╭ node1 ─────────────────╮  ┆ │       │ ┆ │            │ ▼ ▼
//! // ║ │  │                        │  ┆ │       │ ┆ │            │ ▲ ▲
//! // ║ │  │          2fr           │  ┆ │  10%  │ ┆ │    1fr     │ │ ║
//! // ║ │  ╰────────────────────────╯  ┆ ╰───────╯ ┆ ╰────────────╯ │ ║
//! // ║ ╰──────────────────────────────┴───────────┴────────────────╯ ║
//! // ╚═══════════════════════════════════════════════════════════════╝
//! //                                20
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! node1.set_size((2.fr(), 2.0));
//! node2.set_size((10.pc(), 3.0));
//! node3.set_size((1.fr(), 3.0));
//! root.use_auto_layout();
//! ```
//!
//!
//! ## Content justification.
//! Fractional units allow expressing elements spacing easily. For example, the following code
//! places its children with the same spacing between them and half of the surrounding spacing. This
//! is commonly referred to as "content justification":
//!
//!
//! ```
//! // ╔ root ════════════════════ ▶ ◀ ══════════════════════════╗
//! // ║ ╭─────────────────┬─────────────────┬─────────────────╮ ║
//! // ║ │╱╱             ╱╱┆╱╱             ╱╱┆╱╱  ╭ node3 ╮  ╱╱│ ║
//! // ║ │╱╱             ╱╱┆╱╱  ╭ node2 ╮  ╱╱┆╱╱  │       │  ╱╱▼ ▼
//! // ║ │╱╱  ╭ node1 ╮  ╱╱┆╱╱  │       │  ╱╱┆╱╱  │       │  ╱╱▲ ▲
//! // ║ │╱╱  │   2   │  ╱╱┆╱╱  │   2   │  ╱╱┆╱╱  │   2   │  ╱╱│ ║
//! // ║ │.5fr╰───────╯   1fr   ╰───────╯   1fr   ╰───────╯.5fr│ ║
//! // ║ ╰─────────────────┴─────────────────┴─────────────────╯ ║
//! // ╚═════════════════════════════════════════════════════════╝
//! //                             12
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! root
//!     .use_auto_layout()
//!     .set_size_x(12.0)
//!     .set_padding_left(0.5.fr())
//!     .set_padding_right(0.5.fr())
//!     .set_gap_x(1.fr());
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! node1.set_size((2.0, 1.0));
//! node2.set_size((2.0, 2.0));
//! node3.set_size((2.0, 3.0));
//! ```
//!
//! For convenience, there are several content justification functions defined. To get the same
//! result as above, you can simply use the [`justify_content_space_around_x`] function instead.
//! There are also other helper functions defined:
//!
//! ### Justify content left.
//! ```
//! // ╔ root ═════════════════════════════════════════════════════════════╗
//! // ║ ╭──── ▶ ◀ ───┬──── ▶ ◀ ────┬──── ▶ ◀ ────╮  ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱║
//! // ║ │ ╭ node1 ╮  ┆  ╭ node2 ╮  ┆  ╭ node3 ╮  ▼  ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱▼
//! // ║ │ ╰───────╯  ┆  ╰───────╯  ┆  ╰───────╯  ▲  ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱▲
//! // ║ ╰────────────┴─────────────┴─────────────╯           1fr          ║
//! // ╚═══════════════════════════════════════════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! node1.set_size((2.0, 1.0));
//! node2.set_size((2.0, 1.0));
//! node3.set_size((2.0, 1.0));
//! root.use_auto_layout().set_size_x(8.0).justify_content_left();
//! ```
//!
//! ### Justify content center.
//! ```
//! // ╔ root ═══════════════════════════════════════════════════════════════╗
//! // ║╱╱╱╱╱╱╱╱╱╱╱  ╭──── ▶ ◀ ────┬──── ▶ ◀ ────┬──── ▶ ◀ ────╮  ╱╱╱╱╱╱╱╱╱╱╱║
//! // ║╱╱╱╱╱╱╱╱╱╱╱  │  ╭ node1 ╮  ┆  ╭ node2 ╮  ┆  ╭ node3 ╮  ▼  ╱╱╱╱╱╱╱╱╱╱╱▼
//! // ║╱╱╱╱╱╱╱╱╱╱╱  │  ╰───────╯  ┆  ╰───────╯  ┆  ╰───────╯  ▲  ╱╱╱╱╱╱╱╱╱╱╱▲
//! // ║    1fr      ╰─────────────┴─────────────┴─────────────╯      1fr    ║
//! // ╚═════════════════════════════════════════════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! node1.set_size((2.0, 1.0));
//! node2.set_size((2.0, 1.0));
//! node3.set_size((2.0, 1.0));
//! root.use_auto_layout().set_size_x(12.0).justify_content_center_x();
//! ```
//!
//! ### Justify content right.
//! ```
//! // ╔ root ═══════════════════════════════════════════════════════════════╗
//! // ║╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱  ╭──── ▶ ◀ ────┬──── ▶ ◀ ────┬──── ▶ ◀ ────╮  ║
//! // ║╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱  │  ╭ node1 ╮  ┆  ╭ node2 ╮  ┆  ╭ node3 ╮  ▼  ▼
//! // ║╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱  │  ╰───────╯  ┆  ╰───────╯  ┆  ╰───────╯  ▲  ▲
//! // ║         1fr            ╰─────────────┴─────────────┴─────────────╯  ║
//! // ╚═════════════════════════════════════════════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! node1.set_size((2.0, 1.0));
//! node2.set_size((2.0, 1.0));
//! node3.set_size((2.0, 1.0));
//! root.use_auto_layout().set_size_x(12.0).justify_content_right();
//! ```
//!
//! ### Justify content space between.
//! ```
//! // ╔ root ═══════════════════════════════════════════════════════════════╗     
//! // ║ ╭─────── ▶ ◀ ───────┬────────── ▶ ◀ ──────────┬─────── ▶ ◀ ───────╮ ║
//! // ║ │  ╭ node1 ╮  ╱╱╱╱╱╱┆╱╱╱╱╱╱  ╭ node2 ╮  ╱╱╱╱╱╱┆╱╱╱╱╱╱  ╭ node3 ╮  ▼ ▼
//! // ║ │  ╰───────╯  ╱╱╱╱ 1fr ╱╱╱╱  ╰───────╯  ╱╱╱╱ 1fr ╱╱╱╱  ╰───────╯  ▲ ▲
//! // ║ ╰───────────────────┴─────────────────────────┴───────────────────╯ ║
//! // ╚═════════════════════════════════════════════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! node1.set_size((2.0, 1.0));
//! node2.set_size((2.0, 1.0));
//! node3.set_size((2.0, 1.0));
//! root.use_auto_layout().justify_content_space_between_x();
//! ```
//!
//! ### Justify content space around.
//! ```
//! // ╔ root ═══════════════════════════════════════════════════════════════╗     
//! // ║ ╭──────── ▶ ◀ ────────┬──────── ▶ ◀ ────────┬──────── ▶ ◀ ────────╮ ║
//! // ║ │╱╱╱╱  ╭ node1 ╮  ╱╱╱╱┆╱╱╱╱  ╭ node2 ╮  ╱╱╱╱┆╱╱╱╱  ╭ node3 ╮  ╱╱╱╱▼ ▼
//! // ║ │.5fr  ╰───────╯  ╱╱ 1fr ╱╱  ╰───────╯  ╱╱ 1fr ╱╱  ╰───────╯  .5fr▲ ▲
//! // ║ ╰─────────────────────┴─────────────────────┴─────────────────────╯ ║
//! // ╚═════════════════════════════════════════════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! node1.set_size((2.0, 1.0));
//! node2.set_size((2.0, 1.0));
//! node3.set_size((2.0, 1.0));
//! root.use_auto_layout().justify_content_space_around_x();
//! ```
//!
//! ### Justify content space evenly.
//! ```
//! // ╔ root ═══════════════════════════════════════════════════════════════╗     
//! // ║ ╭────────── ▶ ◀ ───────┬─────── ▶ ◀ ───────┬─────── ▶ ◀ ──────────╮ ║
//! // ║ │╱╱╱╱╱╱  ╭ node1 ╮  ╱╱╱┆╱╱╱  ╭ node2 ╮  ╱╱╱┆╱╱╱  ╭ node3 ╮  ╱╱╱╱╱╱▼ ▼
//! // ║ │ 1fr    ╰───────╯    1fr    ╰───────╯    1fr    ╰───────╯    1fr │ ║
//! // ║ ╰──────────────────────┴───────────────────┴──────────────────────╯ ║
//! // ╚═════════════════════════════════════════════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! node1.set_size((2.0, 1.0));
//! node2.set_size((2.0, 1.0));
//! node3.set_size((2.0, 1.0));
//! root.use_auto_layout().justify_content_space_evenly_x();
//! ```
//!
//!
//! ## Properties inheritance.
//! Column and row size is set to 'hug' by default. Other properties will be inherited from children
//! if not set explicitly. The following rules will apply:
//!
//! - The `min_size` will be set to the maximum of all children's `min_size`.
//! - The `max_size` will be set to the minimum of all children's `max_size`.
//! - The `grow_factor` will be set to the average of all children's `grow_factor`.
//! - The `shrink_factor` will be set to the average of all children's `shrink_factor`.
//! - In case the column/row size was set to 'hug' and the child size used fractional units to
//!   express its size, the column/row size will be resolved to a maximum of children sizes,
//!   including the fractional sizes.
//!
//!
//! ## Overlapping items
//! Sometimes, it is impossible to fit all children within the parent container. In such a case, the
//! children can overflow the parent container and can overlap themselves. This can happen when you
//! use negative margins, or when the columns can't grow enough to get all the needed space. For
//! example, the following code creates a three-column layout, where both columns and children are
//! of fixed sizes and are not allowed to grow nor shrink:
//!
//! ```
//! // ╔═════════════════════════ ▶ ◀ ═════════════════════╗
//! // ║ ╭─────────────┬─────────────┬─────────────╮       ║
//! // ║ │ root        ┆             ┆  ╭ node3 ───┼─────╮ ║
//! // ║ │             ┆  ╭ node2 ───┼──┼─╮        │     │ ║
//! // ║ │  ╭ node1 ╮  ┆  │          ┆  │ │        │     │ ▼
//! // ║ │  │       │  ┆  │          ┆  │ │        │     │ ▲
//! // ║ │  ╰───────╯  ┆  ╰──────────┼──┴─╯────────┼─────╯ ║
//! // ║ ╰─────────────┴─────────────┴─────────────╯       ║
//! // ╚═══════════════════════════════════════════════════╝
//! //        2.0           2.0           2.0              
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! node1.set_size((1.0, 2.0));
//! node2.set_size((2.0, 3.0));
//! node3.set_size((3.0, 4.0));
//! root.use_auto_layout().justify_content_space_evenly_x();
//! ```
//!
//! ## Future Grid layout extensions.
//!
//! ### Placing items across multiple rows/columns.
//! The CSS Grid allows placing items across multiple rows/columns. This mode is not supported by
//! the Grid layout yet. See the `grid-template-areas` property to learn more about it:
//! https://css-tricks.com/snippets/css/complete-guide-grid/#aa-grid-template-areas.
//!
//! ### The masonry layout.
//! The masonry layout is a layout where rows or columns are not aligned and can be shifted in one
//! direction. This mode is useful when creating "image walls" or lists of tags, where items have
//! different lengths. For example, it will allow to place items in the following layout:
//!
//! ╔════════════════════════════════════════════════════════════════════════╗
//! ║ ╭───────────── ▶ ◀ ────────────┬─── ▶ ◀ ───┬─────────── ▶ ◀ ─────────╮ ║
//! ║ │  ╭────────────────────────╮  ┆ ╭───────╮ ┆ ╭─────────────────────╮ ▼ ║
//! ║ │  ╰────────────────────────╯  ┆ ╰───────╯ ┆ ╰─────────────────────╯ ▲ ▼
//! ║ ├╌╌╌╌╌╌╌╌╌╌╌╌┬╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┴╌╌╌╌┬╌╌╌╌╌╌┴╌╌╌╌╌╌╌┬╌╌╌╌╌╌╌╌┬╌╌╌╌╌╌╌╌╯ ▲
//! ║ │  ╭──────╮  ┆ ╭──────────────────╮ ┆ ╭──────────╮ ┆ ╭────╮ ▼          ║
//! ║ │  ╰──────╯  ┆ ╰──────────────────╯ ┆ ╰──────────╯ ┆ ╰────╯ ▲          ║
//! ║ ╰─── ▶ ◀ ────┴───────── ▶ ◀ ────────┴──── ▶ ◀ ─────┴─ ▶ ◀ ──╯          ║
//! ╚════════════════════════════════════════════════════════════════════════╝
//!
//! To learn more about the masonry layout, see the following article:
//! https://css-tricks.com/snippets/css/complete-guide-grid/#aa-masonry.
//!
//!
//!
//!
//! # The manual layout.
//! The manual layout is a layout where the user can freely place the children. It is the default
//! layout of display objects. Although the children are not automatically placed, the layout still
//! computes some of their properties, including their size.
//!
//! Let's start with the size. In the case of fixed pixel and percentage of parent size units, the
//! size will be computed just as we've seen previously. In case of non-zero relative units, the
//! size will always be resolved to 100% of the parent container size. If the child was allowed to
//! grow, it will always grow to the size of the parent container. For example, the following code
//! creates a manual layout with two overlapping children that inherit their sizes from the parent
//! container:
//! ```
//! // ╭─────────────────────╮
//! // │ root             △  │
//! // │  ╭───────────────┼▷ │
//! // │  │ node1 & node2 │  │
//! // │  │               │  │
//! // │  │               │  │
//! // │  │               │  │
//! // │  │               │  │
//! // │  ╰───────────────╯  │
//! // ╰─────────────────────╯
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! root.set_size((10.0, 10.0));
//! node1.allow_grow();
//! node2.allow_grow();
//! ```
//!
//! ## Children at negative coordinates.
//! In the parent container size is set to hug, the container size will be automatically increased
//! to fit the children in the top or right direction. However, the children will not be moved, so
//! they can still overflow the parent container. Elements that overflow into negative coordinates
//! will never influence their container's size. For example, the following code creates a manual
//! layout with three children, where two of those overflow the parent container into negative
//! coordinates:
//! ```
//! //     ╭──────── ▶ ◀ ───────────────╮
//! //     │ root            ╭────────╮ │
//! // ╭───┼────╮            │ node3  │ │
//! // │ node1  │            │        │ ▼
//! // │   │    │ ╭────────╮ ╰────────╯ ▲
//! // ╰───┼────╯ │ node2  │            │
//! //     ╰──────┼────────┼────────────╯
//! //            │        │
//! //            ╰────────╯
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! let node3 = root.new_child();
//! node1.set_size((2.0, 2.0));
//! node2.set_size((2.0, 2.0));
//! node3.set_size((2.0, 2.0));
//! node1.set_xy((-1.0, 0.0));
//! node2.set_xy((1.0, -1.0));
//! node3.set_xy((3.0, 1.0));
//! ```
//!
//! The automatic layout manager will only take the elements own size and position into account. If
//! those elements have any overflowing children, that overflow will be ignored by the auto-layout.
//! For example, we can place several manual layouts with children that overflow their parents in an
//! auto-layout. Those children will effectively overflow the auto-layout as well:
//! ```
//! //    ╔═════════════════════ ▶ ◀ ═════════════════════╗
//! //    ║ ╭ root ───────────────┬─────────────────────╮ ║
//! //    ║ │╭ node1 ─ ▶ ◀ ──────╮┆╭ node2 ─ ▶ ◀ ──────╮│ ║
//! //   ╭╫─┼┼─────╮           ╭─┼┼┼─────╮             ││ ║
//! //   │║node1_1 │           │ node2_1 │             ││ ▼
//! //   │║ ││     │           │ │┆│     │             ││ ▲
//! //   │║ ││     │  ╭────────┼╮│┆│     │  ╭─────────╮││ ║
//! //   ╰╫─┼┼─────╯  │ node1_2╰┼┼┼┼─────╯  │ node2_2 │││ ║
//! //    ║ │╰────────┼─────────┼╯┆╰────────┼─────────┼╯│ ║
//! //    ║ ╰─────────┼─────────┼─┴─────────┼─────────┼─╯ ║
//! //    ╚═══════════╰─────────╯═══════════╰─────────╯═══╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//! let root = display::object::Instance::new();
//! let node1 = root.new_child();
//! let node2 = root.new_child();
//! root.use_auto_layout();
//!
//! let node1_1 = node1.new_child();
//! let node1_2 = node1.new_child();
//! node1_1.set_size((2.0, 2.0));
//! node1_2.set_size((2.0, 2.0));
//! node1_1.set_xy((-1.0, 0.0));
//! node1_2.set_xy((1.0, -1.0));
//!
//! let node2_1 = node2.new_child();
//! let node2_2 = node2.new_child();
//! node2_1.set_size((2.0, 2.0));
//! node2_2.set_size((2.0, 2.0));
//! node2_1.set_xy((-1.0, 0.0));
//! node2_2.set_xy((1.0, -1.0));
//! ```
//!
//! # Shape view alignment inside layout objects.
//!
//! The shape views defined using `shape!` can have their own set alignment, which defines how the
//! shape sprite is positioned relative to its display object's position. For example, when the
//! alignment is set to `center`, the shape sprite will be positioned such that it's center aligns
//! with the display objects (0.0, 0.0) origin point. When that shape is a child of an auto-layout,
//! it will visually overflow the parent container.
//!
//! ```text
//!       alignment = center                alignment = bottom_right    
//!            ╭╌view╌╌╌╌╌╌╌╌╮
//!            ┊             ┊            ╭ sprite ─────╮╌view╌╌╌╌╌╌╌╌╮
//!            ┊             ┊            │             │             ┊
//!     ╭ sprite ─────╮      ┊            │             │             ┊
//!     │      ┊      │      ┊            │             │             ┊
//!     │      ┊      │      ┊            │             │             ┊
//!     │      ◎╌╌╌╌╌╌┼╌╌╌╌╌╌╯            │             │             ┊
//!     │             │                   ╰─────────────◎╌╌╌╌╌╌╌╌╌╌╌╌╌╯
//!     │             │
//!     ╰─────────────╯
//! ```
//!
//! To avoid this effect and make sprites compatible with layout's understanding of display objects,
//! you have to use shapes with `alignment` property set to `bottom_left` corner. It is the default
//! shape alignment value when not specified. That way, the sprites will be exactly overlapping the
//! display object's position and size, as set by the layout.
//!
//! ```text
//!    alignment = bottom_left
//!        ╭ view/sprite ╮
//!        │             │
//!        │             │
//!        │             │
//!        │             │
//!        │             │
//!        ◎─────────────╯
//! ```
//!
//! Shape views that are aligned to the bottom left corner can be used as children within the
//! auto-layout and will be positioned as expected.
//!
//! ```
//! // ╔═════════════════════════ ▶ ◀ ═╗
//! // ║ ╭ root ───────┬─────────────╮ ║
//! // ║ │ ╭ shape1 ─╮ ┆ ╭ shape2 ─╮ │ ║
//! // ║ │ │         │ ┆ │         │ │ ║
//! // ║ │ │         │ ┆ │         │ │ ▼
//! // ║ │ │         │ ┆ │         │ │ ▲
//! // ║ │ ◎─────────╯ ┆ ◎─────────╯ │ ║
//! // ║ ╰─────────────┴─────────────╯ ║
//! // ╚═══════════════════════════════╝
//!
//! # use ensogl_core::prelude::*;
//! # use ensogl_core::display;
//!
//! mod rectangle {
//!     use super::*;
//!     ensogl_core::shape! {
//!         alignment = left_bottom; // This is also the default value.
//!         (style: Style) {
//!             let rect = Rect(Var::canvas_size()).corners_radius(5.0.px());
//!             let shape = rect.fill(color::Rgba::new(0.7, 0.5, 0.3, 0.5));
//!             shape.into()
//!         }
//!     }
//! }
//!
//! fn build_layout(root: &display::object::Instance) {
//!     root.use_auto_layout();
//!     let shape1 = rectangle::View::new();
//!     let shape2 = rectangle::View::new();
//!     root.add_child(&shape1);
//!     root.add_child(&shape2);
//!     shape1.set_size((100.0, 100.0));
//!     shape2.set_size((100.0, 100.0));
//! }
//! ```
//!
//! # Size and computed size.
//! Display objects expose two functions to get their size: `size` and `computed_size`. The first
//! one provides the size set by the user. It can be either 'hug' or a fixed value expressed as one
//! of the supported units: pixels, percentage, or fraction. The `computed_size` is always expressed
//! in pixels, however, it is updated during the layout refresh, which happens on every frame (if
//! the layout was modified). It means, that you can not set object size and immediately read the
//! computed size, as it will not give the correct result. However, in case you use the `set_size`
//! function and you set the size to fixed pixel value, the computed size will be updated
//! immediately. This is done only for convenience, as reading the size is a common operation.
//! Please note that this still can provide incorrect value if the object can grow.

use crate::data::dirty::traits::*;
use crate::display::object::layout::*;
use crate::prelude::*;

use crate::display::layout::alignment;
use crate::display::object::event;
use crate::display::object::transformation;
use crate::display::scene::layer::AnySymbolPartition;
use crate::display::scene::layer::Layer;
use crate::display::scene::layer::WeakLayer;
use crate::display::scene::Scene;

use enso_types::Dim;
use nalgebra::Matrix4;
use nalgebra::Vector3;
use transformation::CachedTransformation;
use unit2::Fraction;



// ==========
// === Id ===
// ==========

/// Globally unique identifier of a display object.
#[derive(
    Clone, CloneRef, Copy, Debug, Default, Display, Eq, From, Hash, Into, PartialEq, Ord,
    PartialOrd
)]
pub struct Id(usize);

/// The index of a child of a display object.
#[derive(
    Clone, CloneRef, Copy, Debug, Default, Deref, Display, Eq, From, Hash, Into, PartialEq, Ord,
    PartialOrd
)]
pub struct ChildIndex(usize);



// =============
// === Model ===
// =============

/// The main display object structure. Read the docs of [this module](self) to learn more.
#[derive(Clone, CloneRef, Default, Deref, From)]
#[repr(transparent)]
pub struct Instance {
    def: InstanceDef,
}

/// Internal representation of [`Instance`]. It exists only to make the implementation less
/// error-prone. The [`ObjectOps`] trait defines the public API of display objects, such as the
/// [`add_child`] method, and it is automatically defined for every struct that implements
/// the [`Object`] trait, including the [`Instance`]. Without this struct, the [`add_child`] method
/// would need to be implemented as [`self.display_object().add_child(child)`]. Such an
/// implementation will be very error-prone. After renaming the function in [`Instance`], the
/// [`ObjectOps`] trait would still compile, but its function will call itself infinitely (this is
/// not caught by rustc yet: https://github.com/rust-lang/rust/issues/57965). This struct allows the
/// implementation to be written as [`self.display_object().def.add_child(child)`] instead, which
/// will fail to compile after renaming the function in [`InstanceDef`].
#[derive(Clone, CloneRef, Deref)]
#[repr(transparent)]
pub struct InstanceDef {
    rc: Rc<Model>,
}

/// A display object model. See the documentation of [`Instance`] to learn more.
#[derive(Debug, Deref)]
pub struct Model {
    /// This is the display object's FRP network. Feel free to extend it with new FRP nodes as long
    /// as they are inherently bound with this display object. For example, a sprite, which owns a
    /// display object instance, can extend this network to perform computations. However, you
    /// should not extend it if you don't own the display object, as nodes created in this network
    /// may survive the lifetime of other objects causing memory leaks. See the docs of FRP to
    /// learn more.
    pub network: frp::Network,
    /// A name of this display object. Used for debugging purposes only.
    pub name:    &'static str,
    #[deref]
    hierarchy:   HierarchyModel,
    event:       EventModel,
    layout:      LayoutModel,
}


// === Contructors ===

impl Instance {
    /// Constructor.
    #[profile(Debug)]
    pub fn new() -> Self {
        default()
    }

    /// Constructor of a named display object. The name is used for debugging purposes only.
    pub fn new_named(name: &'static str) -> Self {
        Self { def: InstanceDef::new_named(name) }
    }
}

impl InstanceDef {
    /// Constructor.
    pub fn new() -> Self {
        Self { rc: Rc::new(Model::new()) }.init_events_handling()
    }

    /// Constructor.
    pub fn new_named(name: &'static str) -> Self {
        Self { rc: Rc::new(Model::new_named(name)) }.init_events_handling()
    }

    /// ID getter of this display object.
    pub fn id(&self) -> Id {
        Id(Rc::downgrade(&self.rc).as_ptr() as *const () as usize)
    }
}

impl Model {
    /// Constructor.
    pub fn new() -> Self {
        Self::new_named("UnnamedDisplayObject")
    }

    /// Constructor.
    pub fn new_named(name: &'static str) -> Self {
        let network = frp::Network::new("display_object");
        let hierarchy = HierarchyModel::new(&network);
        let event = EventModel::new(&network);
        let layout = LayoutModel::default();
        Self { network, hierarchy, event, layout, name }
    }
}


// === Impls ===

impl Default for InstanceDef {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for Model {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq for InstanceDef {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.rc, &other.rc)
    }
}

impl PartialEq for Instance {
    fn eq(&self, other: &Self) -> bool {
        self.def.eq(&other.def)
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Instance")
    }
}

impl Display for InstanceDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Instance")
    }
}



// ====================
// === WeakInstance ===
// ====================

/// Weak display object instance.
#[derive(Debug, Clone, CloneRef)]
pub struct WeakInstance {
    weak: Weak<Model>,
}

impl WeakInstance {
    /// Upgrade the weak instance to strong one if it was not yet dropped.
    pub fn upgrade(&self) -> Option<Instance> {
        self.weak.upgrade().map(|rc| InstanceDef { rc }.into())
    }

    /// Checks whether this weak instance still exists (its strong instance was not dropped yet).
    pub fn exists(&self) -> bool {
        self.weak.strong_count() > 0
    }
}

impl InstanceDef {
    /// Create a new weak pointer to this display object instance.
    pub fn downgrade(&self) -> WeakInstance {
        let weak = Rc::downgrade(&self.rc);
        WeakInstance { weak }
    }
}

impl PartialEq for WeakInstance {
    fn eq(&self, other: &Self) -> bool {
        self.weak.ptr_eq(&other.weak)
    }
}

impl Eq for WeakInstance {}

impl Hash for WeakInstance {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.weak.as_ptr().hash(state)
    }
}



// ============
// === Root ===
// ============

/// A root element of a display object hierarchy. Unlike [`Instance`], [`Root`] is visible by
/// default and has explicit methods to hide and show it.
#[derive(Clone, CloneRef, Debug, Deref)]
#[repr(transparent)]
pub struct Root {
    def: Instance,
}

impl Root {
    /// Constructor.
    pub fn new() -> Self {
        let def = default();
        Self { def }.init()
    }

    /// Constructor of a named display object. The name is used for debugging purposes only.
    pub fn new_named(name: &'static str) -> Self {
        let def = Instance::new_named(name);
        Self { def }.init()
    }

    fn init(self) -> Self {
        self.show();
        self
    }

    /// Hide the display object.
    pub fn hide(&self) {
        self.def.hide()
    }

    /// Show the display object.
    pub fn show(&self) {
        self.def.show()
    }
}

impl Default for Root {
    fn default() -> Self {
        Self::new()
    }
}

impl Object for Root {
    fn display_object(&self) -> &Instance {
        &self.def
    }
}



// =================================================================================================
// === Hierarchy ===================================================================================
// =================================================================================================

// ==================
// === ParentBind ===
// ==================

/// A parent-child binding. It contains reference to parent node and information about the child
/// index. When dropped, it removes the child from its parent.
#[derive(Debug)]
pub struct ParentBind {
    /// The parent's child index. If this is a binding stored by [`Instance`], this will be the
    /// instance index in the parent's instance vector.
    child_index: ChildIndex,
    parent:      WeakInstance,
}

impl ParentBind {
    fn parent(&self) -> Option<Instance> {
        self.parent.upgrade()
    }

    // Drop this [`ParentBind`] using provided borrows for its parent and its removed child entry.
    // This allows clearing the parent children in a batch more efficiently.
    fn drop_with_removed_element(
        mut self,
        parent: &InstanceDef,
        removed_children_entry: WeakInstance,
    ) {
        self.notify_on_drop(parent, removed_children_entry);
        // The list is already maintained. Drop the bind without doing it again.
        mem::forget(self);
    }

    fn notify_on_drop(&mut self, parent: &InstanceDef, removed_children_entry: WeakInstance) {
        debug_assert!(parent.downgrade() == self.parent);
        parent.dirty.modified_children.unset(&self.child_index);
        if let Some(child) = removed_children_entry.upgrade() {
            child.dirty.new_parent.set();
        }
        parent.dirty.removed_children.set(removed_children_entry);
    }
}

impl Drop for ParentBind {
    fn drop(&mut self) {
        if let Some(parent) = self.parent() {
            if let Some(weak_child) = parent.children.borrow_mut().remove(&self.child_index) {
                self.notify_on_drop(&parent, weak_child);
            }
        }
    }
}



// ========================
// === SharedParentBind ===
// ========================

/// A shared version of [`Option<ParentBind>`].
#[derive(Clone, CloneRef, Debug, Default)]
pub struct SharedParentBind {
    data: Rc<RefCell<Option<ParentBind>>>,
}

impl SharedParentBind {
    fn is_none(&self) -> bool {
        self.data.borrow().is_none()
    }

    fn is_some(&self) -> bool {
        self.data.borrow().is_some()
    }

    fn set_bind(&self, bind: ParentBind) {
        *self.data.borrow_mut() = Some(bind)
    }

    fn take_bind(&self) -> Option<ParentBind> {
        self.data.borrow_mut().take()
    }

    fn parent(&self) -> Option<Instance> {
        self.data.borrow().as_ref().and_then(|t| t.parent())
    }

    fn parent_and_child_index(&self) -> Option<(Instance, ChildIndex)> {
        self.data.borrow().as_ref().and_then(|t| t.parent().map(|s| (s, t.child_index)))
    }

    fn matches(&self, parent: &WeakInstance, child_index: ChildIndex) -> bool {
        self.data
            .borrow()
            .as_ref()
            .map_or(false, |t| t.child_index == child_index && &t.parent == parent)
    }

    fn child_index(&self) -> Option<ChildIndex> {
        self.data.borrow().as_ref().map(|t| t.child_index)
    }
}



// ===================
// === Dirty Flags ===
// ===================

/// Dirty flags.
pub mod dirty {
    pub use super::*;

    // === Types ===

    type NewParent = crate::data::dirty::RefCellBool<()>;
    type ModifiedChildren = crate::data::dirty::RefCellSet<ChildIndex, OnDirtyCallback>;
    type RemovedChildren = crate::data::dirty::RefCellSet<WeakInstance, OnDirtyCallback>;
    type Transformation = crate::data::dirty::RefCellBool<OnDirtyCallback>;
    type ComputedSize = crate::data::dirty::RefCellBool<OnDirtyCallback>;
    type SceneLayer = crate::data::dirty::RefCellBool<OnDirtyCallback>;


    // === Definition ===

    /// A set of dirty flags encoding which hierarchy-related properties of a display object have
    /// been changed and not yet updated. See the docs of [`Instance`] to learn more about the lazy
    /// update mechanism.
    ///
    /// # Performance
    /// Let's consider a deep tree of objects. To render an object, we need its position in the
    /// world-space (global-space). Thus, when the tree root object moves, all of its children,
    /// their sub-children, etc., need to be updated. As there might be hundreds or thousands of
    /// such sub-children, this might be very costly. Even worse, if the user of this library moves
    /// the root object, and then moves its child, all the sub-children of that child would be
    /// recomputed twice if not updated lazily.
    #[derive(Debug)]
    #[allow(missing_docs)]
    pub struct Flags {
        pub new_parent:        NewParent,
        /// A set of children that were added, transformed, moved to a different layer, or
        /// whose descendants were modified in such a way. Does not contain children that were
        /// removed by themselves. Use `removed_children` flag to handle that case.
        pub modified_children: ModifiedChildren,
        pub removed_children:  RemovedChildren,
        pub transformation:    Transformation,
        pub computed_size:     ComputedSize,
        pub new_layer:         SceneLayer,
    }

    impl Flags {
        /// Constructor.
        pub fn new(parent_bind: &SharedParentBind) -> Self {
            let new_parent = NewParent::new(());
            let modified_children = ModifiedChildren::new(on_dirty_callback(parent_bind));
            let removed_children = RemovedChildren::new(on_dirty_callback(parent_bind));
            let transformation = Transformation::new(on_dirty_callback(parent_bind));
            let computed_size = ComputedSize::new(on_dirty_callback(parent_bind));
            let new_layer = SceneLayer::new(on_dirty_callback(parent_bind));
            Self {
                new_parent,
                modified_children,
                removed_children,
                transformation,
                computed_size,
                new_layer,
            }
        }

        /// Check whether any of the dirty flags is set.
        pub fn check_all(&self) -> bool {
            self.new_parent.check()
                || self.modified_children.check_all()
                || self.removed_children.check_all()
                || self.transformation.check()
                || self.new_layer.check()
        }
    }

    type OnDirtyCallback = impl Fn();
    fn on_dirty_callback(parent_bind: &SharedParentBind) -> OnDirtyCallback {
        let parent_bind = parent_bind.clone_ref();
        move || {
            if let Some((parent, index)) = parent_bind.parent_and_child_index() {
                parent.dirty.modified_children.set(index);
            }
        }
    }
}



// =====================
// === Hierarchy FRP ===
// =====================

/// FRP endpoints relate to display object hierarchy modification.
///
/// # Order of evaluation
/// The events are batched and will be streamed after all display objects have been updated. This
/// prevents a situation where one would like to update the hierarchy of display objects in response
/// to an event. Without batching, the events would be emitted in the middle of the update process.
#[derive(Debug)]
pub struct HierarchyFrp {
    /// Fires when the display object is shown. It will fire during the first scene refresh if this
    /// object was invisible and was added as a child to a visible parent.
    pub on_show:            frp::Stream<(Option<Scene>, Option<WeakLayer>)>,
    /// Fires when the display object is hidden. This can happen for example after detaching it
    /// from a visible parent. It will fire during the first scene refresh if this object was
    /// removed from a visible parent or added to an invisible one.
    pub on_hide:            frp::Stream<Option<Scene>>,
    /// Fires during the first scene refresh if this object was moved between scene layers.
    pub on_layer_change: frp::Stream<(
        Option<Scene>,
        Option<WeakLayer>,
        Option<WeakLayer>,
        Option<AnySymbolPartition>,
    )>,
    /// Fires during the first scene refresh if this object needed an update and the update was
    /// performed.
    pub on_transformed:     frp::Stream<()>,
    /// Fires during the scene refresh if this object was resized due to auto-layout rules.
    pub on_resized:         frp::Sampler<Vector2>,
    on_show_source:         frp::Source<(Option<Scene>, Option<WeakLayer>)>,
    on_hide_source:         frp::Source<Option<Scene>>,
    on_transformed_source:  frp::Source<()>,
    on_resized_source:      frp::Source<Vector2>,
    on_layer_change_source: frp::Source<(
        Option<Scene>,
        Option<WeakLayer>,
        Option<WeakLayer>,
        Option<AnySymbolPartition>,
    )>,
}

impl HierarchyFrp {
    fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            on_show_source <- source();
            on_hide_source <- source();
            on_layer_change_source <- source();
            on_transformed_source <- source();
            on_resized_source <- source();
            on_show <- on_show_source.batch().iter();
            on_hide <- on_hide_source.batch().iter();
            on_layer_change <- on_layer_change_source.batch().iter();
            on_transformed <- on_transformed_source.batch().iter();
            on_resized <- on_resized_source.batch().iter().sampler();
        }
        Self {
            on_show_source,
            on_hide_source,
            on_layer_change_source,
            on_transformed_source,
            on_resized_source,
            on_show,
            on_hide,
            on_layer_change,
            on_transformed,
            on_resized,
        }
    }
}



// =======================
// === Hierarchy Model ===
// =======================

/// The part of display object model related to its hierarchy.
#[derive(Debug, Deref)]
pub struct HierarchyModel {
    #[deref]
    frp:              HierarchyFrp,
    visible:          Cell<bool>,
    transformation:   RefCell<CachedTransformation>,
    parent_bind:      SharedParentBind,
    next_child_index: Cell<ChildIndex>,
    // We are using [`BTreeMap`] here in order to preserve the child insertion order.
    children:         RefCell<BTreeMap<ChildIndex, WeakInstance>>,
    /// Layer the object was explicitly assigned to by the user, if any.
    assigned_layer:   RefCell<Option<LayerAssignment>>,
    /// Layer where the object is displayed. It may be set to by user or inherited from the parent.
    layer:            RefCell<Option<LayerAssignment>>,
    dirty:            dirty::Flags,
}

impl HierarchyModel {
    fn new(network: &frp::Network) -> Self {
        let frp = HierarchyFrp::new(network);
        let visible = default();
        let transformation = default();
        let parent_bind = default();
        let next_child_index = default();
        let children = default();
        let assigned_layer = default();
        let layer = default();
        let dirty = dirty::Flags::new(&parent_bind);
        Self {
            frp,
            visible,
            transformation,
            parent_bind,
            next_child_index,
            children,
            assigned_layer,
            layer,
            dirty,
        }
    }
}



// =======================
// === Hierarchy Logic ===
// =======================

// === Updates and Visibility ===

impl Model {
    /// Get the layer this object is displayed in. May be equal to layer explicitly set by the user
    /// or a layer inherited from the parent.
    fn display_layer(&self) -> Option<Layer> {
        self.layer.borrow().as_ref().and_then(|t| t.layer.upgrade())
    }

    /// Add this object to the provided scene layer. Do not use this method explicitly. Use layers'
    /// methods instead.
    pub(crate) fn add_to_display_layer(&self, layer: &Layer) {
        self.set_display_layer(layer, default())
    }

    /// Add this object to the specified symbol partition of the provided scene layer. Do not use
    /// this method explicitly. Use layers' methods instead.
    pub(crate) fn add_to_display_layer_symbol_partition(
        &self,
        layer: &Layer,
        symbol_partition: AnySymbolPartition,
    ) {
        self.set_display_layer(layer, Some(symbol_partition))
    }

    fn set_display_layer(&self, layer: &Layer, symbol_partition: Option<AnySymbolPartition>) {
        let layer = LayerAssignment { layer: layer.downgrade(), symbol_partition };
        let mut assigned_layer = self.assigned_layer.borrow_mut();
        if assigned_layer.as_ref() != Some(&layer) {
            self.dirty.new_layer.set();
            *assigned_layer = Some(layer);
        }
    }

    /// Remove this object from the provided scene layer. Do not use this method explicitly. Use
    /// layers' methods instead.
    pub(crate) fn remove_from_display_layer(&self, layer: &Layer) {
        let layer = layer.downgrade();
        if self.assigned_layer.borrow().as_ref().map(|assignment| &assignment.layer) == Some(&layer)
        {
            self.dirty.new_layer.set();
            *self.assigned_layer.borrow_mut() = None;
        }
    }
}

impl Model {
    fn children(&self) -> Vec<Instance> {
        self.children.borrow().values().filter_map(|t| t.upgrade()).collect()
    }

    /// Checks whether the object is visible.
    pub fn is_visible(&self) -> bool {
        self.visible.get()
    }

    /// Hide the object. This is a helper API. Used by tests and the [`Root`] object.
    fn hide(&self) {
        self.set_vis_false(None)
    }

    /// Show the object. This is a helper API. Used by tests and the [`Root`] object.
    fn show(&self) {
        self.set_vis_true(None, None)
    }

    fn set_vis_false(&self, scene: Option<&Scene>) {
        if self.visible.get() {
            trace!("Hiding.");
            self.visible.set(false);
            self.on_hide_source.emit(scene.cloned());
            self.children
                .borrow()
                .values()
                .filter_map(|t| t.upgrade())
                .for_each(|t| t.set_vis_false(scene));
        }
    }

    fn set_vis_true(&self, scene: Option<&Scene>, parent_layer: Option<&WeakLayer>) {
        if !self.visible.get() {
            trace!("Showing.");
            self.visible.set(true);
            let assigned_layer_borrow = self.assigned_layer.borrow();
            let assigned_layer = assigned_layer_borrow.as_ref().map(|assignment| &assignment.layer);
            let new_layer = assigned_layer.or(parent_layer);
            self.on_show_source.emit((scene.cloned(), new_layer.cloned()));
            self.children
                .borrow()
                .values()
                .filter_map(|t| t.upgrade())
                .for_each(|t| t.set_vis_true(scene, new_layer));
        }
    }

    /// Checks whether the object is orphan (do not have parent object attached).
    pub fn has_parent(&self) -> bool {
        self.parent_bind.is_some()
    }

    /// Get reference to the parent object if any.
    pub fn parent(&self) -> Option<Instance> {
        self.parent_bind.parent()
    }

    /// The index of this display object in the parent's children list.
    fn my_index(&self) -> Option<ChildIndex> {
        self.parent_bind.child_index()
    }

    fn has_visible_parent(&self) -> bool {
        self.parent_bind.parent().map_or(false, |parent| parent.is_visible())
    }

    /// Number of children of this object.
    pub fn children_count(&self) -> usize {
        self.children.borrow().len()
    }

    /// Removes and returns the parent bind. Please note that the parent is not updated as long as
    /// the parent bind is not dropped.
    fn take_parent_bind(&self) -> Option<ParentBind> {
        let parent_bind = self.parent_bind.take_bind();
        if let Some(parent) = parent_bind.as_ref().and_then(|t| t.parent.upgrade()) {
            let is_focused = self.event.focused_descendant.borrow().is_some();
            if is_focused {
                parent.propagate_up_no_focus_instance();
            }
        }
        parent_bind
    }

    /// Set parent of the object. If the object already has a parent, the parent would be replaced.
    fn set_parent_bind(&self, bind: ParentBind) {
        trace!("Adding new parent bind.");
        if let Some(parent) = bind.parent() {
            self.parent_bind.set_bind(bind);
            self.dirty.new_parent.set();
            if let Some(focus_instance) = &*self.event.focused_descendant.borrow() {
                parent.blur_tree();
                parent.propagate_up_new_focus_instance(focus_instance);
            }
        }
    }

    /// Removes all children of this display object and returns them.
    pub fn remove_all_children(&self) -> Vec<Instance> {
        let children: Vec<Instance> =
            self.children.borrow().values().filter_map(|weak| weak.upgrade()).collect();
        for child in &children {
            child.unset_parent();
        }
        children
    }

    /// Recompute the transformation matrix of the display object tree starting with this object and
    /// traversing all of its dirty children.
    #[profile(Detail)]
    pub fn update(&self, scene: &Scene) {
        self.refresh_layout();
        let parent_origin =
            self.parent().map_or(Matrix4::identity(), |parent| parent.transformation_matrix());
        self.update_with_origin(scene, parent_origin, false, false, None);
    }

    /// Update the display object tree transformations based on the parent object origin. See docs
    /// of [`update`] to learn more.
    ///
    /// # Update Order
    /// Please note that scene layers assignment update is performed before the origin and
    /// visibility one. This is because there are rare cases where it is desirable to modify the
    /// display object hierarchy in the `on_layer_change` callback (when the display object was
    /// moved to another layer). For example, this mechanism is used in the shape system to replace
    /// the sprite instance to a new one on layer change. Please note that updating the display
    /// object hierarchy during its refresh is a very complex operation and an extra care should be
    /// taken when modifying this logic.
    fn update_with_origin(
        &self,
        scene: &Scene,
        parent_origin: Matrix4<f32>,
        parent_origin_changed: bool,
        parent_layers_changed: bool,
        parent_layer: Option<&LayerAssignment>,
    ) {
        // === Scene Layers Update ===

        let has_new_parent = self.dirty.new_parent.check();
        let assigned_layer_ref = self.assigned_layer.borrow();
        let assigned_layer = assigned_layer_ref.as_ref();
        let assigned_layers_changed = self.dirty.new_layer.take().check();
        let has_assigned_layer = assigned_layer.is_some();
        let layer_changed = if assigned_layers_changed {
            // We might as well check here if assigned layers were not removed and accidentally the
            // inherited layers are not the same as previously assigned ones, but this is so rare
            // situation that we are not checking it to optimize the performance of this case.
            true
        } else if has_assigned_layer {
            false
        } else if has_new_parent {
            // Optimization for a common case of switching parent in the same layer.
            self.layer.borrow().as_ref() != parent_layer
        } else {
            parent_layers_changed
        };

        let new_layer_opt = layer_changed.as_some_from(|| {
            if has_assigned_layer {
                assigned_layer
            } else {
                parent_layer
            }
        });
        if let Some(new_layer) = new_layer_opt {
            debug_span!("Scene layer changed.").in_scope(|| {
                let old_layer = mem::replace(&mut *self.layer.borrow_mut(), new_layer.cloned());
                self.on_layer_change_source.emit((
                    Some(scene.clone_ref()),
                    old_layer.map(|assignment| assignment.layer),
                    new_layer.map(|assignment| assignment.layer.clone()),
                    new_layer.and_then(|assignment| assignment.symbol_partition),
                ));
            });
        }

        let current_layer = self.layer.borrow();
        let new_layer = new_layer_opt.unwrap_or(current_layer.as_ref());


        // === Origin & Visibility Update ===

        self.update_visibility(scene, parent_layer.as_ref().map(|assignment| &assignment.layer));
        let is_origin_dirty = has_new_parent || parent_origin_changed || layer_changed;
        let new_parent_origin = is_origin_dirty.as_some(parent_origin);
        let parent_origin_label = if new_parent_origin.is_some() { "new" } else { "old" };
        debug_span!("Update with {} parent origin.", parent_origin_label).in_scope(|| {
            let origin_changed = self.transformation.borrow_mut().update(new_parent_origin);
            let new_origin = self.transformation.borrow().matrix;
            if origin_changed || layer_changed {
                self.dirty.modified_children.unset_all();
                if origin_changed {
                    trace!("Self origin changed.");
                    self.on_transformed_source.emit(());
                } else {
                    trace!("Self origin did not change, but the layers did.");
                }
                if !self.children.borrow().is_empty() {
                    debug_span!("Updating all children.").in_scope(|| {
                        let children = self.children.borrow();
                        children.values().for_each(|weak_child| {
                            weak_child.upgrade().for_each(|child| {
                                child.update_with_origin(
                                    scene,
                                    new_origin,
                                    true,
                                    layer_changed,
                                    new_layer,
                                )
                            });
                        });
                    })
                }
            } else {
                trace!("Self origin and layers did not change.");

                if self.dirty.computed_size.check() {
                    trace!("Computed size changed.");
                    self.on_transformed_source.emit(());
                }

                if self.dirty.modified_children.check_all() {
                    debug_span!("Updating dirty children.").in_scope(|| {
                        self.dirty.modified_children.take().iter().for_each(|ix| {
                            self.children.borrow().get(ix).and_then(|t| t.upgrade()).for_each(
                                |child| {
                                    child.update_with_origin(
                                        scene,
                                        new_origin,
                                        false,
                                        layer_changed,
                                        new_layer,
                                    )
                                },
                            )
                        });
                    })
                }
            }
        });
        self.dirty.transformation.unset();
        if self.dirty.computed_size.check() {
            self.on_resized_source.emit(self.layout.computed_size.get());
        }
        self.dirty.computed_size.unset();
        self.dirty.new_parent.unset();
    }

    /// Hide all removed children and show this display object if it was attached to a new parent.
    fn update_visibility(&self, scene: &Scene, parent_layer: Option<&WeakLayer>) {
        self.take_removed_children_and_update_their_visibility(scene);
        let parent_changed = self.dirty.new_parent.check();
        if parent_changed && self.has_parent() {
            self.set_vis_true(Some(scene), parent_layer)
        }
    }

    fn take_removed_children_and_update_their_visibility(&self, scene: &Scene) {
        if self.dirty.removed_children.check_all() {
            debug_span!("Updating removed children.").in_scope(|| {
                for child in self.dirty.removed_children.take().into_iter() {
                    if let Some(child) = child.upgrade() {
                        if !child.has_visible_parent() {
                            // The child was not attached to another visible parent.
                            child.set_vis_false(Some(scene));
                        }
                        // Even if the child is visible at this point, it does not mean that it
                        // should be visible after the entire update. Therefore, we must ensure that
                        // "removed children" lists in its subtree will be managed.
                        // See also test `visibility_test3`.
                        child.take_removed_children_and_update_their_visibility(scene);
                    }
                }
            })
        }
    }
}

impl InstanceDef {
    /// Checks if the provided object is child of the current one.
    pub fn has_child<T: Object>(&self, child: &T) -> bool {
        self.child_index(child).is_some()
    }

    /// Returns the index of the provided object if it was a child of the current one.
    pub fn child_index<T: Object>(&self, child: &T) -> Option<ChildIndex> {
        let child = child.display_object();
        child.parent_bind.parent_and_child_index().and_then(|(parent, index)| {
            if &parent.def == self {
                Some(index)
            } else {
                None
            }
        })
    }

    /// Replaces the parent binding with a new parent.
    fn set_parent(&self, parent: &InstanceDef) {
        parent.add_child(self);
    }

    /// Removes the current parent binding.
    fn unset_parent(&self) {
        self.take_parent_bind();
    }

    /// Attaches the provided display object as a child to this one.
    fn add_child(&self, child: &InstanceDef) {
        child.unset_parent();
        let child_index = self.register_child(child);
        trace!("Adding a new child at index {child_index}.");
        let parent_bind = ParentBind { parent: self.downgrade(), child_index };
        child.set_parent_bind(parent_bind);
    }

    fn add_children<T: Object>(&self, children: impl IntoIterator<Item = T>) {
        children.into_iter().for_each(|child| self.add_child(child.display_object()));
    }

    /// Replace children with object from the provided list. Objects that are already children of
    /// this instance will be moved to the new position. Objects that are not children of this
    /// instance will change their parent and will be inserted in the right position.
    ///
    /// This method avoids unnecessary dirty flag updates and is more efficient than removing all
    /// children and adding them again. Children that only swapped their position will be marked
    /// as modified, but not as having a new parent. Children that are already under the right index
    /// will not be marked as modified.
    ///
    /// Has no effect if the provided list matches the current children list, as long as the
    /// internal child indices were already sequential starting from 0. If that's not the case,
    /// the children will be marked as updated.
    ///
    /// NOTE: If the list contain duplicated objects (instances that are clones of the same ref),
    /// the behavior is undefined. It will however not cause any memory unsafety and all objects
    /// will remain in some valid state.
    fn replace_children<T: Object>(&self, new_children: &[T]) {
        let this_weak = self.downgrade();
        let mut children_borrow = self.children.borrow_mut();
        let num_children_before = children_borrow.len();

        let mut pushed_out_children = false;
        let mut added_children = 0;
        let mut next_free_index = new_children.len().max(*self.next_child_index.get());
        let starting_free_index = next_free_index;

        // Update child indices of existing children, maintain their dirty flags.
        for (index, child) in new_children.iter().enumerate() {
            let child = child.display_object();
            let new_child_index = ChildIndex(index);

            let mut bind_borrow = child.parent_bind.data.borrow_mut();
            let same_parent_bind = bind_borrow.as_mut().filter(|bind| bind.parent == this_weak);

            let free_index = match same_parent_bind {
                Some(bind) => {
                    // The child is already a child of this parent. Update its index.

                    if bind.child_index == new_child_index {
                        // The child is already at its destination index. No need to update it.
                        continue;
                    }

                    // Move the child to its destination index. In case the newly taken spot was
                    // occupied, use a swap. The occupied entry will later be moved to the spot
                    // freed by this element.
                    let old_index = bind.child_index;
                    bind.child_index = new_child_index;

                    // If the old index was higher than the starting number of children, it means
                    // that this element was previously pushed out by a swap. We are reusing it, but
                    // not cleaning up the space it occupied. The cleanup is instead deferred.
                    pushed_out_children |= *old_index >= starting_free_index;

                    old_index
                }
                None => {
                    added_children += 1;
                    // This was not a child of this instance, so it needs to be added as one. Move
                    // it from its existing parent to this one.
                    drop(bind_borrow);
                    drop(child.take_parent_bind());
                    let new_parent_bind =
                        ParentBind { parent: this_weak.clone(), child_index: new_child_index };
                    child.set_parent_bind(new_parent_bind);
                    self.dirty.removed_children.unset(&child.downgrade());
                    let free_index = ChildIndex(next_free_index);
                    next_free_index += 1;
                    free_index
                }
            };

            // If there already was a child present at the destination index, swap them. That child
            // will be either maintained in future iterations or deleted.
            //
            // Note that we want to always attempt BTreeMap insertions before deletions, so we can
            // avoid unnecessary tree structure manipulations. When inserting to previously occupied
            // element, the tree structure is not modified.
            self.dirty.modified_children.swap(free_index, new_child_index);
            self.dirty.modified_children.set(new_child_index);
            let child_at_dest = children_borrow.insert(new_child_index, child.downgrade());
            if let Some(child_at_dest) = child_at_dest {
                if let Some(strong) = child_at_dest.upgrade() {
                    let mut bind = strong.parent_bind.data.borrow_mut();
                    let bind = bind.as_mut().expect("Child should always have a parent bind.");
                    bind.child_index = free_index;
                    children_borrow.insert(free_index, child_at_dest);
                    // In case we just put a child in its final spot, we have to mark as modified.
                    // If it ends up being deleted, the flag will be cleared anyway.
                    if bind.parent == this_weak {
                        self.dirty.modified_children.set(free_index);
                    }
                }
            }
        }

        // At this point, all children that were in the new list are in the right position. We
        // only need to remove the children that were not in the new list. All of them are still
        // in the children list, and their indices are past the inserted indices.
        let has_stale_indices = pushed_out_children || starting_free_index > new_children.len();
        let retained_children = new_children.len() - added_children;
        let has_elements_to_remove = retained_children < num_children_before;
        let need_cleanup = has_elements_to_remove || has_stale_indices;

        if need_cleanup {
            let mut binds_to_drop = SmallVec::<[(ParentBind, WeakInstance); 8]>::new();

            // Drop the instances that were removed from the children list. Note that the drop may
            // cause the instance to be removed from the children list, so we need to drop the
            // instances without holding to borrows.
            children_borrow.retain(|index, weak_instance| {
                let to_retain = **index < new_children.len();
                if !to_retain {
                    let instance = weak_instance.upgrade();
                    // We do not immediately remove old keys containing pushed-out children when
                    // they have been reinserted to their appropriate position. To avoid treating
                    // them as removed, we have to filter them out. Only children that are at their
                    // correct position should be removed.
                    let instance = instance.filter(|i| i.parent_bind.child_index() == Some(*index));
                    let bind = instance.and_then(|i| i.take_parent_bind());
                    let bind_with_instance = bind.map(|bind| (bind, weak_instance.clone()));
                    binds_to_drop.extend(bind_with_instance);
                }
                to_retain
            });

            drop(children_borrow);

            self.next_child_index.set(ChildIndex(new_children.len()));
            for (bind, weak) in binds_to_drop {
                bind.drop_with_removed_element(self, weak)
            }
        }
    }

    fn register_child(&self, child: &InstanceDef) -> ChildIndex {
        let index = self.next_child_index.get();
        self.next_child_index.set(ChildIndex(*index + 1));
        self.children.borrow_mut().insert(index, child.downgrade());
        self.dirty.removed_children.unset(&child.downgrade());
        self.dirty.modified_children.set(index);
        index
    }

    /// Removes the provided object reference from child list of this object. Does nothing if the
    /// reference was not a child of this object.
    fn remove_child<T: Object>(&self, child: &T) {
        let child = child.display_object();
        if self.has_child(child) {
            child.unset_parent()
        }
    }

    /// Get reversed parent chain of this display object (`[root, child_of root, ..., parent,
    /// self]`). The last item is this object.
    fn rev_parent_chain(&self) -> Vec<Instance> {
        let mut vec = default();
        Self::build_rev_parent_chain(&mut vec, Some(self.clone_ref().into()));
        vec
    }

    fn build_rev_parent_chain(vec: &mut Vec<Instance>, parent: Option<Instance>) {
        if let Some(parent) = parent {
            Self::build_rev_parent_chain(vec, parent.parent());
            vec.push(parent);
        }
    }
}



// ========================
// === Layer assignment ===
// ========================

/// Identifies an assigned layer, including symbol partition information, if any.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LayerAssignment {
    /// The layer.
    pub layer:            WeakLayer,
    /// The symbol partition, if any.
    pub symbol_partition: Option<AnySymbolPartition>,
}



// =======================
// === Transformations ===
// =======================

impl Model {
    /// Position of the object in the global coordinate space.
    fn global_position(&self) -> Vector3<f32> {
        self.transformation.borrow().global_position()
    }

    /// Position of the object in the parent coordinate space.
    fn position(&self) -> Vector3<f32> {
        self.transformation.borrow().position()
    }

    /// Scale of the object in the parent coordinate space.
    fn scale(&self) -> Vector3<f32> {
        self.transformation.borrow().scale()
    }

    /// Rotation of the object in the parent coordinate space.
    fn rotation(&self) -> Vector3<f32> {
        self.transformation.borrow().rotation()
    }

    /// Transformation matrix of the object in the parent coordinate space.
    fn transformation_matrix(&self) -> Matrix4<f32> {
        self.transformation.borrow().matrix()
    }
}


// === Transformation Setters ===

impl Model {
    fn with_mut_borrowed_transformation<F, T>(&self, f: F) -> T
    where F: FnOnce(&mut CachedTransformation) -> T {
        self.dirty.transformation.set();
        f(&mut self.transformation.borrow_mut())
    }

    /// Access the transformation of the object as mutable, but only mark it as dirty if the
    /// provided function returns true.
    fn with_mut_borrowed_transformation_manually_flagged<F>(&self, f: F)
    where F: FnOnce(&mut CachedTransformation) -> bool {
        let modified = f(&mut self.transformation.borrow_mut());
        if modified {
            self.dirty.transformation.set();
        }
    }
}

macro_rules! generate_transformation_getters_and_setters {
    ($($name:ident),*) => { paste! {
        impl Model {$(
            fn [<set_ $name>](&self, v: Vector3<f32>) {
                self.with_mut_borrowed_transformation(|t| t.[<set_ $name>](v));
            }

            fn [<update_ $name>](&self, f: impl FnOnce(Vector3<f32>) -> Vector3<f32>) {
                self.with_mut_borrowed_transformation(|t| t.[<update_ $name>](f));
            }

            fn [<modify_ $name>](&self, f: impl FnOnce(&mut Vector3<f32>)) {
                self.with_mut_borrowed_transformation(|t| t.[<modify_ $name>](f));
            }

            fn [<set_ $name _dim>]<D>(&self, dim: D, value: f32)
            where Vector3<f32>: DimSetter<D> {
                self.with_mut_borrowed_transformation_manually_flagged(|t|
                    t.[<set_ $name _dim_checked>](dim, value)
                );
            }

            fn [<update_ $name _dim>]<D: Copy>(&self, dim: D, f: impl FnOnce(f32) -> f32)
            where Vector3<f32>: DimSetter<D> {
                self.with_mut_borrowed_transformation(|t|
                    t.[<modify_ $name>](|v| v.update_dim(dim, f))
                );
            }

            fn [<modify_ $name _dim>]<D: Copy>(&self, dim: D, f: impl FnOnce(&mut f32))
            where Vector3<f32>: DimSetter<D> {
                self.with_mut_borrowed_transformation(|t|
                    t.[<modify_ $name>](|v| v.modify_dim(dim, f))
                );
            }
        )*}
    }};
}

generate_transformation_getters_and_setters!(position, scale, rotation);



// =================================================================================================
// === Event System ================================================================================
// =================================================================================================

// ======================
// === Events & Focus ===
// ======================
// See the documentation of [`event::Event`] to learn more about events.

/// The part of display object model related to event handling.
#[derive(Debug)]
pub struct EventModel {
    source:             frp::Source<event::SomeEvent>,
    capturing_fan:      frp::Fan,
    bubbling_fan:       frp::Fan,
    focused_descendant: RefCell<Option<WeakInstance>>,
}

impl EventModel {
    fn new(network: &frp::Network) -> Self {
        let capturing_fan = frp::Fan::new(network);
        let bubbling_fan = frp::Fan::new(network);
        let focused_descendant = default();
        frp::extend! { network
            source <- source();
        }
        Self { source, capturing_fan, bubbling_fan, focused_descendant }
    }
}

impl Model {
    /// Get event stream for bubbling events. See docs of [`event::Event`] to learn more.
    fn on_event<T>(&self) -> frp::Stream<event::Event<T>>
    where T: frp::Data {
        self.event.bubbling_fan.output::<event::Event<T>>()
    }

    /// Get event stream for capturing events. You should rather not need this function. Use
    /// [`on_event`] instead. See docs of [`event::Event`] to learn more.
    fn on_event_capturing<T>(&self) -> frp::Stream<event::Event<T>>
    where T: frp::Data {
        self.event.capturing_fan.output::<event::Event<T>>()
    }
}

impl InstanceDef {
    fn init_events_handling(self) -> Self {
        // This implementation is a bit complex because we do not want to clone network to the FRP
        // closure in order to avoid a memory leak.
        let network = &self.network;
        let parent_bind = &self.parent_bind;
        let capturing_event_fan = &self.event.capturing_fan;
        let bubbling_event_fan = &self.event.bubbling_fan;
        frp::extend! { network
            eval self.event.source ([parent_bind, capturing_event_fan, bubbling_event_fan] (event) {
                let parent = parent_bind.parent();
                Self::emit_event_impl(event, parent, &capturing_event_fan, &bubbling_event_fan);
            });
        }
        self
    }

    fn emit_event_impl(
        event: &event::SomeEvent,
        parent: Option<Instance>,
        capturing_event_fan: &frp::Fan,
        bubbling_event_fan: &frp::Fan,
    ) {
        let rev_parent_chain = parent.map(|p| p.rev_parent_chain()).unwrap_or_default();
        if event.captures.get() {
            for object in &rev_parent_chain {
                if !event.is_cancelled() {
                    object.event.capturing_fan.emit(&event.data);
                } else {
                    break;
                }
            }
        }
        if !event.is_cancelled() {
            capturing_event_fan.emit(&event.data);
        }
        if !event.is_cancelled() {
            bubbling_event_fan.emit(&event.data);
        }
        if event.bubbles.get() {
            for object in rev_parent_chain.iter().rev() {
                if !event.is_cancelled() {
                    object.event.bubbling_fan.emit(&event.data);
                } else {
                    break;
                }
            }
        }
    }

    fn new_event<T>(&self, payload: T) -> event::SomeEvent
    where T: 'static {
        event::SomeEvent::new(Some(self.downgrade()), payload)
    }

    fn emit_event<T>(&self, payload: T)
    where T: 'static {
        let event = self.new_event(payload);
        self.event.source.emit(event);
    }

    fn emit_event_without_bubbling<T>(&self, payload: T)
    where T: 'static {
        let event = self.new_event(payload);
        event.set_bubbling(false);
        self.event.source.emit(event);
    }

    fn focused_descendant(&self) -> Option<Instance> {
        self.event.focused_descendant.borrow().as_ref().and_then(|t| t.upgrade())
    }

    fn focused_instance(&self) -> Option<Instance> {
        if let Some(child) = self.focused_descendant() {
            Some(child)
        } else {
            self.parent().and_then(|parent| parent.focused_instance())
        }
    }

    fn is_focused(&self) -> bool {
        self.focused_descendant().as_ref().map(|t| &t.def) == Some(self)
    }

    fn focus(&self) {
        self.blur_tree();
        self.propagate_up_new_focus_instance(&self.downgrade());
        let focus_event = self.new_event(event::Focus);
        let focus_in_event = self.new_event(event::FocusIn);
        focus_event.bubbles.set(false);
        self.event.source.emit(focus_event);
        self.event.source.emit(focus_in_event);
    }

    fn blur(&self) {
        if self.is_focused() {
            self.blur_unchecked();
        }
    }

    /// Blur the display object tree this object belongs to. If any tree node (any node directly or
    /// indirectly connected with each other) was focused, it will be blurred.
    fn blur_tree(&self) {
        if let Some(instance) = self.focused_instance() {
            instance.blur_unchecked();
        }
    }

    /// Blur this object and propagate the information to root. Does not check if this object was
    /// focused. Calling this method on a non-focused object may cause inconsistent state, as parent
    /// objects will erase information about the currently focused object.
    fn blur_unchecked(&self) {
        self.propagate_up_no_focus_instance();
        let blur_event = self.new_event(event::Blur);
        let focus_out_event = self.new_event(event::FocusOut);
        blur_event.bubbles.set(false);
        self.event.source.emit(blur_event);
        self.event.source.emit(focus_out_event);
    }

    /// Clears the focus info in this instance and all parent instances. In order to work properly,
    /// this should be called on the focused instance. Otherwise, it may clear the information
    /// only partially.
    fn propagate_up_no_focus_instance(&self) {
        *self.event.focused_descendant.borrow_mut() = None;
        self.parent().for_each(|parent| parent.propagate_up_no_focus_instance());
    }

    /// Set the focus instance to the provided one here and in all instances on the path to the
    /// root.
    fn propagate_up_new_focus_instance(&self, instance: &WeakInstance) {
        debug_assert!(self.event.focused_descendant.borrow().is_none());
        *self.event.focused_descendant.borrow_mut() = Some(instance.clone());
        self.parent().for_each(|parent| parent.propagate_up_new_focus_instance(instance));
    }
}

impl Debug for InstanceDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DisplayObject")
            .field("name", &self.name)
            .field("position", &self.position().xy().as_slice())
            .field("computed_size", &self.layout.computed_size.get().as_slice())
            .finish()
    }
}

impl Debug for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.def, f)
    }
}



// =================================================================================================
// === Layout and Size =============================================================================
// =================================================================================================

// ===================
// === LayoutModel ===
// ===================

/// Layout properties of this display object. It can be considered similar to the [`AutoLayout`]
/// struct, which describes the layout properties of the object's children.
#[derive(Debug, Derivative)]
#[derivative(Default)]
pub struct LayoutModel {
    /// This struct is kept in an `Option` to make the initialization faster. Auto layout is not
    /// used often. This field can contain [`AutoLayout`] instance even if auto layout is not used.
    /// For example, if someone sets auto layout options (e.g. the `gap`), but the auto-layout
    /// was not enabled, it will be instantiated with the `enabled` field set to `false`.
    auto_layout:   RefCell<Option<AutoLayout>>,
    alignment:     Cell<alignment::OptDim2>,
    margin:        Cell<Vector2<SideSpacing>>,
    padding:       Cell<Vector2<SideSpacing>>,
    #[derivative(Default(
        value = "Cell::new((f32::INFINITY, f32::INFINITY).into_vector_trans())"
    ))]
    max_size:      Cell<Vector2<Unit>>,
    min_size:      Cell<Vector2<Unit>>,
    size:          Cell<Vector2<Size>>,
    grow_factor:   Cell<Vector2<f32>>,
    shrink_factor: Cell<Vector2<f32>>,
    computed_size: Cell<Vector2<f32>>,
}

impl Model {
    fn modify_layout(&self, f: impl FnOnce(&LayoutModel)) {
        self.dirty.transformation.set();
        f(&self.layout);
    }

    fn modify_alignment(&self, f: impl FnOnce(&mut alignment::OptDim2)) {
        self.modify_layout(|layout| {
            layout.alignment.modify(f);
        });
    }

    fn modify_margin(&self, f: impl FnOnce(&mut Vector2<SideSpacing>)) {
        self.modify_layout(|layout| {
            layout.margin.modify(f);
        });
    }

    fn modify_padding(&self, f: impl FnOnce(&mut Vector2<SideSpacing>)) {
        self.modify_layout(|layout| {
            layout.padding.modify(f);
        });
    }

    fn set_layout(&self, layout: impl Into<Option<AutoLayout>>) {
        self.modify_layout(|l| {
            l.auto_layout.replace(layout.into());
        });
    }
}

macro_rules! gen_alignment_setters {
    ([$([$name:ident $x:tt $y:tt])*]) => { paste! { $(
        /// Set the alignment of this object. Alignment positions the object within the free space
        /// around them. For example, if objects are placed in a column, the right alignment will
        /// move them to the right border of the column.
        fn [<set_alignment _ $name>](&self)  -> &Self {
            self.display_object().modify_alignment(|t| t.[<align_ $name>]());
            self
    })*}}
}

macro_rules! gen_margin_or_padding_props {
    ([$tp: ident] [ $([$axis:ident $name:ident $loc:ident])* ]) => { paste! { $(
        /// Modify the margin/padding of the object. Margin is the free space around this object,
        /// while padding is the space inside the object, close to its borders.
        #[enso_shapely::gen(update, set)]
        fn [<modify_ $tp _ $name>](&self, f: impl FnOnce(&mut Unit)) -> &Self {
            self.display_object().[<modify_ $tp>](|t| f(&mut t.$axis.$loc));
            self
        }
    )*}}
}

macro_rules! gen_content_justification {
    ($axis:ident, $name:ident, $start:expr, $end: expr, $gap:expr) => {
        paste! {
            /// Content justification. See docs of this module to learn more and see examples.
            fn [<justify_content_ $name>](&self) -> &Self {
                self.display_object().def.modify_padding(|t| {
                    t.$axis.start = $start.into();
                    t.$axis.end = $end.into();
                });
                self.[<set_gap_ $axis>]($gap);
                self
            }
        }
    };
}

impl<T: Object + ?Sized> LayoutOps for T {}
/// Display object operations related to layout and size.
pub trait LayoutOps: Object {
    crate::with_alignment_opt_dim2_named_matrix_sparse!(gen_alignment_setters);
    crate::with_display_object_side_spacing_matrix!(gen_margin_or_padding_props[margin]);
    crate::with_display_object_side_spacing_matrix!(gen_margin_or_padding_props[padding]);

    gen_content_justification!(x, left, 0.0, 1.fr(), 0.0);
    gen_content_justification!(x, center_x, 1.fr(), 1.fr(), 0.0);
    gen_content_justification!(x, right, 1.fr(), 0.0, 0.0);
    gen_content_justification!(x, space_between_x, 0.0, 0.0, 1.fr());
    gen_content_justification!(x, space_around_x, 0.5.fr(), 0.5.fr(), 1.fr());
    gen_content_justification!(x, space_evenly_x, 1.fr(), 1.fr(), 1.fr());

    gen_content_justification!(y, bottom, 0.0, 1.fr(), 0.0);
    gen_content_justification!(y, center_y, 1.fr(), 1.fr(), 0.0);
    gen_content_justification!(y, top, 1.fr(), 0.0, 0.0);
    gen_content_justification!(y, space_between_y, 0.0, 0.0, 1.fr());
    gen_content_justification!(y, space_around_y, 0.5.fr(), 0.5.fr(), 1.fr());
    gen_content_justification!(y, space_evenly_y, 1.fr(), 1.fr(), 1.fr());

    /// Set the alignment of this object. Alignment positions the object within the free space
    /// around them. For example, if objects are placed in a column, the right alignment will
    /// move them to the right border of the column.
    fn set_alignment(&self, alignment: alignment::OptDim2) {
        self.display_object().def.modify_alignment(|a| *a = alignment);
    }

    /// Content justification. See docs of this module to learn more and see examples.
    fn justify_content_center(&self) -> &Self {
        self.justify_content_center_x().justify_content_center_y()
    }

    /// Content justification. See docs of this module to learn more and see examples.
    fn justify_content_space_between(&self) -> &Self {
        self.justify_content_space_between_x().justify_content_space_between_y()
    }

    /// Content justification. See docs of this module to learn more and see examples.
    fn justify_content_space_around(&self) -> &Self {
        self.justify_content_space_around_x().justify_content_space_around_y()
    }

    /// Content justification. See docs of this module to learn more and see examples.
    fn justify_content_space_evenly(&self) -> &Self {
        self.justify_content_space_evenly_x().justify_content_space_evenly_y()
    }

    /// The computed size of the object, in pixels. This value will be updated during display object
    /// refresh cycle, which happens once per frame.
    fn computed_size(&self) -> Vector2<f32> {
        self.display_object().def.layout.computed_size.get()
    }

    /// Get the size of the object. Please note that this is user-set size, not the computed one. If
    /// you want to know the computed size, use the [`computed_size`] method instead.
    fn size(&self) -> Vector2<Size> {
        self.display_object().def.layout.size.get()
    }

    /// Get the margin of the object. Please note that this is user-set margin, not the computed
    /// one.
    fn margin(&self) -> Vector2<SideSpacing> {
        self.display_object().def.layout.margin.get()
    }

    /// Modify the size of the object. By default, the size is set to hug the children. You can set
    /// the size either to a fixed pixel value, a percentage parent container size, or to a fraction
    /// of the free space left after placing siblings with fixed sizes.
    ///
    /// Please note that the [`computed_size`] will not be updated immediately. It will be updated
    /// during the next display object refresh cycle, which happens once per frame. When the size is
    /// set to a fixed pixel value, the final `computed_size` can still differ from the requested
    /// size, because the layout might apply growing or shrinking to the object if it is configured
    /// to do so.
    ///
    /// If you need to know the final computed size of the object, use the [`on_changed`] stream
    #[enso_shapely::gen(update, set(trait = "IntoVectorTrans2<Size>", fn = "into_vector_trans()"))]
    fn modify_size(&self, f: impl FnOnce(&mut Vector2<Size>)) -> &Self {
        self.display_object().modify_layout(|layout| {
            layout.size.modify(f);
        });
        self
    }

    /// Set the X-axis size of the object. Set the Y-axis size to hug the children.
    fn set_size_x_hug(&self, x: impl Into<Size>) -> &Self {
        self.set_size((x, Size::Hug));
        self
    }

    /// Set the X-axis size to hug the children. Set the Y-axis size of the object.
    fn set_size_hug_y(&self, y: impl Into<Size>) -> &Self {
        self.set_size((Size::Hug, y));
        self
    }

    /// Set the X-axis size to hug the children.
    fn set_size_x_to_hug(&self) -> &Self {
        self.modify_size(|t| t.x = Size::Hug);
        self
    }

    /// Set the Y-axis size to hug the children.
    fn set_size_y_to_hug(&self) -> &Self {
        self.modify_size(|t| t.y = Size::Hug);
        self
    }

    /// Set both the X-axis and Y-axis size to hug the children.
    fn set_size_hug(&self) -> &Self {
        self.set_size((Size::Hug, Size::Hug));
        self
    }

    /// The maximum size of the object. During auto layout, if the object [`grow_factor`] is
    /// non-zero, the object will grow to maximum this size.
    fn max_size(&self) -> Vector2<Unit> {
        self.display_object().layout.max_size.get()
    }

    /// Modify the maximum size of the object. During auto layout, if the object [`grow_factor`] is
    /// non-zero, the object will grow to maximum this size.
    #[enso_shapely::gen(update, set(trait = "IntoVectorTrans2<Unit>", fn = "into_vector_trans()"))]
    fn modify_max_size(&self, f: impl FnOnce(&mut Vector2<Unit>)) -> &Self {
        self.display_object().modify_layout(|l| l.max_size.modify_(f));
        self
    }

    /// Do not limit the max size of this object. During auto layout, if the object [`grow_factor`]
    /// is non-zero, the object will grow as much as possible.
    fn unset_max_size(&self) -> &Self {
        self.set_max_size(Vector2(f32::INFINITY, f32::INFINITY))
    }

    /// Do not limit the maximum size of this object in the X-axis direction. During auto layout, if
    /// the object [`grow_factor`] is non-zero, the object will grow as much as possible.
    fn unset_max_size_x(&self) -> &Self {
        self.set_max_size_x(f32::INFINITY)
    }

    /// Do not limit the maximum size of this object in the Y-axis direction. During auto layout, if
    /// the object [`grow_factor`] is non-zero, the object will grow as much as possible.
    fn unset_max_size_y(&self) -> &Self {
        self.set_max_size_y(f32::INFINITY)
    }

    /// The minimum size of the object. During auto layout, if the object [`shrink_factor`] is
    /// non-zero, the object will shrink to minimum this size.
    fn min_size(&self) -> Vector2<Unit> {
        self.display_object().layout.min_size.get()
    }

    /// Modify the minimum size of the object. During auto layout, if the object [`shrink_factor`]
    /// is non-zero, the object will shrink to minimum this size.
    #[enso_shapely::gen(update, set(trait = "IntoVectorTrans2<Unit>", fn = "into_vector_trans()"))]
    fn modify_min_size(&self, f: impl FnOnce(&mut Vector2<Unit>)) -> &Self {
        self.display_object().modify_layout(|l| l.min_size.modify_(f));
        self
    }

    /// Do not limit the minumum size of this object in the X-axis direction. During auto layout, if
    /// the object [`shrink_factor`] is non-zero, the object will shrink as much as possible.
    fn unset_min_size_x(&self) -> &Self {
        self.set_min_size_x(0.0)
    }

    /// Do not limit the max size of this object in the Y-axis direction. During auto layout, if the
    /// object [`shrink_factor`] is non-zero, the object will shrink as much as possible.
    fn unset_min_size_y(&self) -> &Self {
        self.set_min_size_y(0.0)
    }

    /// Modify the grow factor of the object. During auto layout, if the object [`grow_factor`]
    /// is non-zero, the object will grow to a maximum size of [`max_size`].
    #[enso_shapely::gen(update, set(trait = "IntoVector2<f32>", fn = "into_vector()"))]
    fn modify_grow_factor(&self, f: impl FnOnce(&mut Vector2<f32>)) -> &Self {
        self.display_object().modify_layout(|l| l.grow_factor.modify_(f));
        self
    }

    /// Modify the shrink factor of the object. During auto layout, if the object [`shrink_factor`]
    /// is non-zero, the object will shrink to a minimum size of [`min_size`].
    #[enso_shapely::gen(update, set(trait = "IntoVector2<f32>", fn = "into_vector()"))]
    fn modify_shrink_factor(&self, f: impl FnOnce(&mut Vector2<f32>)) -> &Self {
        self.display_object().modify_layout(|l| l.shrink_factor.modify_(f));
        self
    }

    /// Allow the object to grow in both X-axis and Y-axis directions. This has the same effect as
    /// [`set_grow_factor((1.0, 1.0))`].
    fn allow_grow(&self) -> &Self {
        self.set_grow_factor((1.0, 1.0))
    }

    /// Allow the object to grow in the X-axis directions. This has the same effect as
    /// [`set_grow_factor_x(1.0)`].
    fn allow_grow_x(&self) -> &Self {
        self.set_grow_factor_x(1.0)
    }

    /// Allow the object to grow in the Y-axis directions. This has the same effect as
    /// [`set_grow_factor_y(1.0)`].
    fn allow_grow_y(&self) -> &Self {
        self.set_grow_factor_y(1.0)
    }

    /// Allow the object to shrink in both X-axis and Y-axis directions. This has the same effect as
    /// [`set_shrink_factor((1.0, 1.0))`].
    fn allow_shrink(&self) -> &Self {
        self.set_shrink_factor((1.0, 1.0))
    }

    /// Allow the object to shrink in the X-axis directions. This has the same effect as
    /// [`set_shrink_factor_x(1.0)`].
    fn allow_shrink_x(&self) -> &Self {
        self.set_shrink_factor_x(1.0)
    }

    /// Allow the object to shrink in the Y-axis directions. This has the same effect as
    /// [`set_shrink_factor_y(1.0)`].
    fn allow_shrink_y(&self) -> &Self {
        self.set_shrink_factor_y(1.0)
    }

    /// Set margin of all sides of the object. Margin is the free space around the object.
    fn set_margin_all(&self, value: impl Into<Unit>) {
        let margin = SideSpacing::from(value.into());
        self.display_object().layout.margin.set(Vector2(margin, margin));
    }

    /// Set margin of all sides of the object. Margin is the free space around the object.
    fn set_margin_trbl(
        &self,
        top: impl Into<Unit>,
        right: impl Into<Unit>,
        bottom: impl Into<Unit>,
        left: impl Into<Unit>,
    ) -> &Self {
        let horizontal = SideSpacing::new(left.into(), right.into());
        let vertical = SideSpacing::new(bottom.into(), top.into());
        self.display_object().layout.margin.set(Vector2(horizontal, vertical));
        self
    }

    /// Set padding of all sides of the object. Padding is the free space inside the object.
    fn set_padding_all(&self, value: impl Into<Unit>) -> &Self {
        let padding = SideSpacing::from(value.into());
        self.display_object().layout.padding.set(Vector2(padding, padding));
        self
    }

    /// Set padding of all sides of the object. Padding is the free space inside the object.
    fn set_padding_trbl(
        &self,
        top: impl Into<Unit>,
        right: impl Into<Unit>,
        bottom: impl Into<Unit>,
        left: impl Into<Unit>,
    ) {
        let horizontal = SideSpacing::new(left.into(), right.into());
        let vertical = SideSpacing::new(bottom.into(), top.into());
        self.display_object().layout.padding.set(Vector2(horizontal, vertical));
    }
}



// ======================
// === Column and Row ===
// ======================

/// The auto-layout grid column/row definition.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
#[allow(missing_docs)]
pub struct ColumnOrRow {
    size:          Size,
    min_size:      Option<f32>,
    max_size:      Option<f32>,
    grow_factor:   Option<f32>,
    shrink_factor: Option<f32>,
}

/// The auto-layout column definition.
#[derive(Clone, Copy, Debug, Default, Deref, DerefMut, PartialEq)]
pub struct Column {
    def: ColumnOrRow,
}

/// The auto-layout row definition.
#[derive(Clone, Copy, Debug, Default, Deref, DerefMut, PartialEq)]
pub struct Row {
    def: ColumnOrRow,
}



// ==========================
// === Column and Row Ref ===
// ==========================
// Utilities to modify columns and rows.

/// The auto-layout column/row reference.
#[derive(Debug)]
pub struct ColumnOrRowRef<Dim> {
    instance: Instance,
    index:    usize,
    axis:     PhantomData<Dim>,
}

/// The auto-layout column reference.
pub type ColumnRef = ColumnOrRowRef<X>;

/// The auto-layout row reference.
pub type RowRef = ColumnOrRowRef<Y>;

impl<Dim> ColumnOrRowRef<Dim> {
    fn new(instance: Instance, index: usize) -> Self {
        Self { instance, index, axis: PhantomData }
    }
}

macro_rules! gen_column_or_row_builder_props {
    ($($name:ident : $ty:ty),*) => { paste! { $(
        /// Modify the property of the column/row.
        #[enso_shapely::gen(update, set)]
        pub fn [<modify_ $name>](&self, f: impl FnOnce(&mut $ty)) -> &Self {
            self.modify(|t| f(&mut t.$name))
        }
    )*}};
}

impl<Dim> ColumnOrRowRef<Dim>
where Dim: ColumnOrRowAccessor
{
    fn modify(&self, f: impl FnOnce(&mut ColumnOrRow)) -> &Self {
        self.instance
            .modify_auto_layout(|l| f(l.unchecked_column_or_row_mut(Dim::default(), self.index)));
        self
    }

    gen_column_or_row_builder_props!(
        size: Size,
        min_size: Option<f32>,
        max_size: Option<f32>,
        grow_factor: Option<f32>,
        shrink_factor: Option<f32>
    );

    /// Allow the object to grow. This has the same effect as [`set_grow_factor(1.0)`].
    pub fn allow_grow(&self) -> &Self {
        self.set_grow_factor(1.0)
    }

    /// Allow the object to shrink. This has the same effect as [`set_shrink_factor(1.0)`].
    pub fn allow_shrink(&self) -> &Self {
        self.set_shrink_factor(1.0)
    }
}



// ======================
// === AutoLayoutFlow ===
// ======================

/// The grid flow defines how the next item is placed in context of the previous one. The default
/// flow is 'row', which means that the next item will be placed in the same row as the previous one
/// if this is possible, e.g. if it was not limited by the [`set_column_count`] method.
#[derive(Clone, Copy, Default, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum AutoLayoutFlow {
    #[default]
    Row,
    Column,
}



// ==================
// === AutoLayout ===
// ==================

/// The auto-layout options. The auto-layout is similar to a mix of CSS flexbox and CSS grid with a
/// few additions. Read the docs of [this module](self) to learn more.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct AutoLayout {
    /// Indicates whether the auto-layout is enabled. This struct is kept in an `Option` to make
    /// the initialization faster. Auto layout is not used often. If someone sets auto layout
    /// options (e.g. the `gap`), but the auto-layout was not enabled, this field is set to false.
    pub enabled: bool,
    /// Controls in which direction and order the items are placed in the grid.
    pub flow: AutoLayoutFlow,
    /// The default item alignment in grid cells. This can be overriden per-child.
    pub children_alignment: alignment::Dim2,
    /// The spacing between columns/rows.
    pub gap: Vector2<Unit>,
    /// Indicates whether the columns/rows should be placed in order (left to right, bottom to top)
    /// or in a reversed order.
    pub reversed_columns_and_rows: Vector2<bool>,
    /// Other columns and rows definitions.
    pub columns_and_rows: (NonEmptyVec<ColumnOrRow>, NonEmptyVec<ColumnOrRow>),
    /// The number of columns and rows in the grid. If it's set to [`None`], the columns and rows
    /// will grow on demand.
    pub columns_and_rows_count: Vector2<Option<usize>>,
}

/// A trait alias for accessing columns and rows.
pub trait ColumnOrRowAccessor = Copy + Default
where (NonEmptyVec<ColumnOrRow>, NonEmptyVec<ColumnOrRow>):
        DimMut<Self, Output = NonEmptyVec<ColumnOrRow>>;

impl AutoLayout {
    fn enable(mut self) -> Self {
        self.enabled = true;
        self
    }

    fn unchecked_column_or_row_mut<Dim>(&mut self, dim: Dim, index: usize) -> &mut ColumnOrRow
    where Dim: ColumnOrRowAccessor {
        &mut self.columns_and_rows.get_dim_mut(dim)[index]
    }
}

#[allow(missing_docs)]
impl Model {
    /// Modify the auto layout. It sets the dirty flag, so it should be used as the main function
    /// for any user-facing API.
    fn modify_auto_layout<T>(&self, f: impl FnOnce(&mut AutoLayout) -> T) -> T {
        self.modify_auto_layout_without_setting_dirty_flag(|layout| {
            self.set_layout_dirty_flag();
            f(layout)
        })
    }

    /// Modify the auto layout. It does not set the dirty flag. Use with caution.
    fn modify_auto_layout_without_setting_dirty_flag<T>(
        &self,
        f: impl FnOnce(&mut AutoLayout) -> T,
    ) -> T {
        let mut borrow = self.layout.auto_layout.borrow_mut();
        let layout = borrow.get_or_insert_with(default);
        f(layout)
    }

    fn set_layout_dirty_flag(&self) {
        self.dirty.transformation.set();
    }

    fn set_children_alignment(&self, value: alignment::Dim2) {
        self.modify_auto_layout(|l| l.children_alignment = value);
    }
}

impl InstanceDef {
    fn first_column_or_row<Dim>(&self) -> ColumnOrRowRef<Dim> {
        ColumnOrRowRef::new(self.clone_ref().into(), 0)
    }

    fn column_or_row<Dim>(&self, dim: Dim, index: usize) -> Option<ColumnOrRowRef<Dim>>
    where Dim: ColumnOrRowAccessor {
        self.modify_auto_layout_without_setting_dirty_flag(|l| {
            (l.columns_and_rows.get_dim(dim).len() < index).as_some_from(|| {
                self.set_layout_dirty_flag();
                ColumnOrRowRef::new(self.clone_ref().into(), index)
            })
        })
    }

    fn add_column_or_row<Dim>(&self, dim: Dim) -> ColumnOrRowRef<Dim>
    where Dim: ColumnOrRowAccessor {
        let index = self.modify_auto_layout(|l| {
            l.columns_and_rows.get_dim_mut(dim).push(default());
            l.columns_and_rows.get_dim(dim).len() - 1
        });
        ColumnOrRowRef::new(self.clone_ref().into(), index)
    }
}

macro_rules! gen_layout_object_builder_alignment {
    ([$([$name:ident $x:ident $y:ident])*]) => { paste! { $(
        /// Set the default alignment of the children of this display object.
        fn [<set_children_alignment_ $name>](&self) -> &Self {
            self.display_object().def.set_children_alignment(alignment::Dim2::$name());
            self
        }
    )*}}
}

impl<T: Object + ?Sized> AutoLayoutOps for T {}

/// Options for controllin auto-layout.
pub trait AutoLayoutOps: Object {
    crate::with_alignment_dim2_named_anchors_cartesian!(gen_layout_object_builder_alignment);

    /// Set the size of the space between columns and rows.
    #[enso_shapely::gen(update, set(trait = "IntoVectorTrans2<Unit>", fn = "into_vector_trans()"))]
    fn modify_gap(&self, f: impl FnOnce(&mut Vector2<Unit>)) -> &Self {
        self.display_object().modify_auto_layout(|l| f(&mut l.gap));
        self
    }

    /// Set the number of columns used by the layout.
    #[enso_shapely::gen(update, set)]
    fn modify_column_count(&self, f: impl FnOnce(&mut Option<usize>)) -> &Self {
        self.display_object().modify_auto_layout(|l| f(&mut l.columns_and_rows_count.x));
        self
    }

    /// Set the number of rows used by the layout.
    #[enso_shapely::gen(update, set)]
    fn modify_row_count(&self, f: impl FnOnce(&mut Option<usize>)) -> &Self {
        self.display_object().modify_auto_layout(|l| f(&mut l.columns_and_rows_count.y));
        self
    }

    /// Control whether columns should be displayed in a reversed order.
    fn set_columns_reversed(&self, rev: bool) -> &Self {
        self.display_object().modify_auto_layout(|l| l.reversed_columns_and_rows.set_x(rev));
        self
    }

    /// Control whether rows should be displayed in a reversed order.
    fn set_rows_reversed(&self, rev: bool) -> &Self {
        self.display_object().modify_auto_layout(|l| l.reversed_columns_and_rows.set_y(rev));
        self
    }

    /// Reverse the order of columns.
    fn reverse_columns(&self) -> &Self {
        self.display_object().modify_auto_layout(|l| l.reversed_columns_and_rows.update_x(|t| !t));
        self
    }

    /// Reverse the order of rows.
    fn reverse_rows(&self) -> &Self {
        self.display_object().modify_auto_layout(|l| l.reversed_columns_and_rows.update_y(|t| !t));
        self
    }

    /// First column accessor. It allows to modify the column. First column and row always exists,
    /// and thus this function always succeeds.
    fn first_column(&self) -> ColumnRef {
        self.display_object().def.first_column_or_row()
    }

    /// First row accessor. It allows to modify the row. First column and row always exists,
    /// and thus this function always succeeds.
    fn first_row(&self) -> RowRef {
        self.display_object().def.first_column_or_row()
    }

    /// Column accessor. It allows to modify the column. Returns [`None`] if the column does not
    /// exist.
    fn column(&self, index: usize) -> Option<ColumnRef> {
        self.display_object().def.column_or_row(X, index)
    }

    /// Row accessor. It allows to modify the row. Returns [`None`] if the column does not exist.
    fn row(&self, index: usize) -> Option<RowRef> {
        self.display_object().def.column_or_row(Y, index)
    }

    /// Add a new column. The returned ref allows to modify it after creation.
    fn add_column(&self) -> ColumnRef {
        self.display_object().def.add_column_or_row(X)
    }

    /// Add a new row. The returned ref allows to modify it after creation.
    fn add_row(&self) -> RowRef {
        self.display_object().def.add_column_or_row(Y)
    }

    /// Sets the auto layout flow to 'row'. Items will first fill a row before moving to the next
    /// one.
    fn set_row_flow(&self) -> &Self {
        self.display_object().modify_auto_layout(|l| l.flow = AutoLayoutFlow::Row);
        self
    }

    /// Sets the auto layout flow to 'column'. Items will first fill a column before moving to the
    /// next one.
    fn set_column_flow(&self) -> &Self {
        self.display_object().modify_auto_layout(|l| l.flow = AutoLayoutFlow::Column);
        self
    }
}



// =========================
// === Layout Resolution ===
// =========================

impl Model {
    /// Updates a linear (horizontal or vertical) layout.
    ///
    ///
    /// # Overview of the two-pass layout update algorithm
    ///
    /// The layout update is a two pass algorithm. First, the sizes and positions of elements is
    /// updated in the horizontal direction, then in the vertical direction. To better illustrate
    /// the need of such a solution, consider the following example:
    ///
    /// ```text
    /// ╔ root ═══════════════════════════════════╗
    /// ║ ╭────────────────────┬────────────────╮ ║
    /// ║ │╱╱╱╱╱               ┆             △  │ ║
    /// ║ │╱╱╱╱╱  ╭── ▶ ◀ ──┬▷ ┆  ╭ R ▶ ◀ ───┤  │ ║
    /// ║ │╱╱╱╱╱  │ L       │  ┆  │      △   │  │ ║
    /// ║ │╱╱╱╱╱  │ ╭ L1 ┬▷ │  ┆  │ ╭ R2 ┤   │  │ ║
    /// ║ │╱╱╱╱╱  │ │    │  │  ┆  │ │ 30 │   │  │ ║
    /// ║ │╱╱╱╱╱  │ │    │  ▼  ┆  │ ╰────╯   │  ▼ ║
    /// ║ │╱╱╱╱╱  │ │    │  ▲  ┆  │      △   │  ▲ ║
    /// ║ │╱╱╱╱╱  │ │    │  │  ┆  │ ╭ R1 ┤   │  │ ║
    /// ║ │╱╱╱╱╱  │ │ 10 │  │  ┆  │ │ 20 │   │  │ ║
    /// ║ │╱╱╱╱╱  │ ╰────╯  │  ┆  │ ╰────╯   │  │ ║
    /// ║ │ 1fr   ╰─────────╯  │  ╰──────────╯  │ ║
    /// ║ ╰────────────────────┴────────────────╯ ║
    /// ╚═════════════════════════════════════════╝
    ///                     100
    /// ```
    ///
    /// Please note, that computing sizes could not be realized in a single pass. In order to
    /// compute L object width, we need to compute R object width first, as L will occupy the
    /// leftover space. Analogously, We can't compute R object height without first computing the L
    /// object height, as the parent container height is set to hug and it depends on the L object
    /// height.
    ///
    /// From a high perspective, each algorithm pass, both for X-axis and Y-axis, works as follows
    /// (only X-axis analysis is presented here):
    ///
    ///
    /// 1. We need to compute static sizes of all elements. A static size is a size that does not
    /// depend on siblings nor on free space left after static size resolution.
    ///
    /// 1.a. We are visiting the L object, which X-axis size is set to hug. Then we are visiting the
    /// L1 object, which X-axis size is set to 10, however, both L1 and it's parent are allowed to
    /// grow in the X-axis direction. For now, we assume that the width of both L and L1 is 10.
    ///
    /// 1.b. We are visiting the R object, which X-axis size is set to hug. Then we are visiting the
    /// R1 and R2 objects. Their width is set to 20 and 30 respectively and they can't grow in the
    /// X-axis direction. Thus, the size of R is set to 30, the bigger value.
    ///
    /// 1.c. We computed the static size of all elements. The static sizes of L and R are 10 and 30,
    /// respectively.
    ///
    ///
    /// 2. We need to compute the correct size of all elements taking into consideration the
    /// leftover space. In this case, the leftover space is 100 - 10 - 30 = 60. It needs to be
    /// first distributed among objects that can grow.
    ///
    /// 2.a. The L object can grow in the X-axis direction. The ability to grow is resolved before
    /// fractional units, so the object is resized to occupy all the free space, and thus, it's new
    /// X-axis size is 70.
    ///
    /// 2.b. As the L object was updated, we need to re-visit it's child to update it's size. The L1
    /// X-axis size is changed to 70 as well, as it is allowed to grow in the X-axis direction.
    ///
    ///
    /// 3. Now, we can compute the fractional units. As there is no free space left, the left
    /// padding is set to 0. If the L object was disallowed to grow, or the first column width
    /// was not set to hug, the left padding will be non-zero and will be used to "push L and R"
    /// objects to the right.
    ///
    ///
    /// 4. In reality, the algorithm is much more complex. For example, columns that are set to hug
    /// automatically inherit some of the childrens values. To learn more about it, see the docs of
    /// this module.
    ///
    ///
    ///
    /// # Meaning of the function parameters.
    /// In order to make the code easy to understand, all variables in layout functions were named
    /// as if the code was updating horizontal layout only. In reality, the variable [`x`] can be
    /// set to either [`X`] or [`Y`] to update horizontal and vertical axis, respectively.
    fn refresh_layout(&self) {
        if self.should_refresh_layout() {
            let old_size = self.layout.computed_size.get();
            self.reset_size_to_static_values(X, 0.0);
            self.reset_size_to_static_values(Y, 0.0);
            self.refresh_layout_internal(X, PassConfig::Default);
            self.refresh_layout_internal(Y, PassConfig::Default);
            let new_size = self.layout.computed_size.get();
            if old_size != new_size {
                self.dirty.computed_size.set();
            }
        }
    }
}

/// Pass configuration.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PassConfig {
    Default,
    DoNotHugDirectChildren,
}

trait ResolutionDim = Copy + Debug + ResolutionDimImpl
where
    Vector2<Size>: DimSetter<Self>,
    Vector2<alignment::OptDim1>: DimSetter<Self>,
    Vector2<alignment::Dim1>: DimSetter<Self>,
    Vector2<SideSpacing>: DimSetter<Self>,
    Vector2<bool>: DimSetter<Self>,
    Vector2<f32>: DimSetter<Self>,
    Vector2<Unit>: DimSetter<Self>,
    Vector3<f32>: DimSetter<Self>,
    (NonEmptyVec<ColumnOrRow>, NonEmptyVec<ColumnOrRow>):
        DimRef<Self, Output = NonEmptyVec<ColumnOrRow>>;

trait ResolutionDimImpl {
    fn matches_flow_direction(self, flow: AutoLayoutFlow) -> bool;
    fn last_pass(self) -> bool;
}

impl ResolutionDimImpl for X {
    fn matches_flow_direction(self, flow: AutoLayoutFlow) -> bool {
        matches! { flow, AutoLayoutFlow::Row }
    }
    fn last_pass(self) -> bool {
        false
    }
}

impl ResolutionDimImpl for Y {
    fn matches_flow_direction(self, flow: AutoLayoutFlow) -> bool {
        matches! { flow, AutoLayoutFlow::Column }
    }
    fn last_pass(self) -> bool {
        true
    }
}

impl Model {
    /// Reset size of this display object to values that can be computed statically. This is, use
    /// the size property if it is set to pixels or to percent of the parent size. If the size is
    /// set to hug and will be changed in next layout refresh, the size is set to 0.0. Otherwise,
    /// the size does not change.
    fn reset_size_to_static_values<Dim>(&self, x: Dim, parent_size: f32)
    where Dim: ResolutionDim {
        let size = self.resolve_size_static_values(x, parent_size);
        self.layout.computed_size.set_dim(x, size);
    }

    fn resolve_size_static_values<Dim>(&self, x: Dim, parent_size: f32) -> f32
    where Dim: ResolutionDim {
        match self.layout.size.get_dim(x) {
            Size::Fixed(unit) => unit.resolve_const_and_percent(parent_size).unwrap_or(0.0),
            Size::Hug => 0.0,
        }
    }

    fn should_propagate_parent_layout_refresh<Dim>(&self, x: Dim) -> bool
    where Dim: ResolutionDim {
        self.layout.size.get_dim(x).depends_on_parent_size() || self.should_refresh_layout()
    }

    fn should_refresh_layout(&self) -> bool {
        self.dirty.transformation.check()
            || self.dirty.modified_children.check_all()
            || self.dirty.removed_children.check_all()
    }

    /// The main entry point from the recursive auto-layout algorithm.
    fn refresh_layout_internal<Dim>(&self, x: Dim, pass_cfg: PassConfig)
    where Dim: ResolutionDim {
        // let old_size = self.layout.computed_size.get();
        if let Some(layout) = &*self.layout.auto_layout.borrow() && layout.enabled {
            self.refresh_grid_layout(x, layout);
        } else {
            self.refresh_manual_layout(x, pass_cfg);
        }
        // if x.last_pass() {
        //     let new_size = self.layout.computed_size.get();
        //     if old_size != new_size {
        //         self.on_resized_source.emit(new_size);
        //     }
        // }
    }

    /// # Meaning of the function parameters.
    /// In order to make the code easy to understand, all variables in layout functions were named
    /// as if the code was updating horizontal layout only. In reality, the variable [`x`] can be
    /// set to either [`X`] or [`Y`] to update horizontal and vertical axis, respectively.
    fn refresh_manual_layout<Dim>(&self, x: Dim, pass_cfg: PassConfig)
    where Dim: ResolutionDim {
        let hug_children = pass_cfg != PassConfig::DoNotHugDirectChildren;
        let hug_children = hug_children && self.layout.size.get_dim(x).is_hug();
        let children = self.children();
        let old_child_computed_sizes: Vec<f32> =
            children.iter().map(|child| child.layout.computed_size.get_dim(x)).collect();

        let mut max_x = 0.0f32;
        let mut has_aligned_non_grow_children = false;
        let mut has_grow_children = false;
        for child in &children {
            let to_grow = child.layout.grow_factor.get_dim(x) > 0.0;
            if !to_grow {
                // If the child is not growing, doesn't depend on parent size and by itself was not
                // modified since last update, it is guaranteed to already have correct size.
                if child.should_propagate_parent_layout_refresh(x) {
                    child.reset_size_to_static_values(x, self.layout.computed_size.get_dim(x));
                    child.refresh_layout_internal(x, PassConfig::Default);
                }

                if child.layout.alignment.get().get_dim(x).is_some() {
                    // If the child is aligned, the parent manages its position relative to its own
                    // size and position. That child must not be taken into account when calculating
                    // size for hugging. It will be hugged again after its alignment is resolved.
                    has_aligned_non_grow_children = true;
                } else {
                    let child_pos = child.position().get_dim(x);
                    let child_size = child.computed_size().get_dim(x);
                    let child_margin =
                        child.layout.margin.get_dim(x).end.resolve_pixels_or_default();
                    max_x = max_x.max(child_pos + child_size + child_margin);
                }
            } else {
                has_grow_children = true;
            }
        }

        if hug_children {
            self.layout.computed_size.set_dim(x, max_x);
        }

        // Resolve aligned children and hug them again.
        if has_aligned_non_grow_children {
            let base_size = self.layout.computed_size.get_dim(x);
            for child in &children {
                let to_grow = child.layout.grow_factor.get_dim(x) > 0.0;
                if let Some(alignment) = *child.layout.alignment.get().get_dim(x) && !to_grow {
                    let child_size = child.computed_size().get_dim(x);
                    let child_margin = child.layout.margin.get_dim(x).resolve_pixels_or_default();
                    let remaining_size = base_size - child_size - child_margin.total();
                    let aligned_x = remaining_size * alignment.normalized() + child_margin.start;
                    child.set_position_dim(x, aligned_x);
                    max_x = max_x.max(aligned_x + child_size + child_margin.end);
                }
            }
            if hug_children {
                self.layout.computed_size.set_dim(x, max_x);
            }
        }

        // From this point on, the size of this node is not changing anymore.
        let self_size = self.layout.computed_size.get_dim(x);

        if has_grow_children {
            for child in &children {
                let to_grow = child.layout.grow_factor.get_dim(x) > 0.0;
                if to_grow {
                    let can_shrink = child.layout.shrink_factor.get_dim(x) > 0.0;
                    let child_size = child.computed_size().get_dim(x);
                    let child_margin = child.layout.margin.get_dim(x).resolve_pixels_or_default();
                    let mut desired_child_size = self_size - child_margin.total();

                    if !can_shrink {
                        let child_static_size = child.resolve_size_static_values(x, self_size);
                        desired_child_size = desired_child_size.max(child_static_size);
                    }

                    if desired_child_size != child_size || child.should_refresh_layout() {
                        child.layout.computed_size.set_dim(x, desired_child_size);
                        child.refresh_layout_internal(x, PassConfig::DoNotHugDirectChildren);
                    }

                    if let Some(alignment) = *child.layout.alignment.get().get_dim(x) {
                        let remaining_size = self_size - desired_child_size - child_margin.total();
                        let aligned_x =
                            remaining_size * alignment.normalized() + child_margin.start;
                        child.set_position_dim(x, aligned_x);
                    }
                }
            }
        }

        for (child, old_size) in children.iter().zip(old_child_computed_sizes) {
            if child.layout.computed_size.get_dim(x) != old_size {
                child.dirty.computed_size.set();
            }
        }
    }
}


#[derive(Debug, Deref, DerefMut)]
struct UnresolvedColumn {
    #[deref]
    #[deref_mut]
    axis:     ColumnOrRow,
    children: Vec<Instance>,
}

#[derive(Debug, Deref, DerefMut)]
struct ResolvedColumn {
    #[deref]
    #[deref_mut]
    axis:     ResolvedAxis,
    children: Vec<Instance>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
struct ResolvedAxis {
    size:              Size,
    min_size:          f32,
    max_size:          f32,
    grow_factor:       f32,
    shrink_factor:     f32,
    computed_size:     f32,
    max_child_fr_size: Fraction,
}

impl Model {
    /// # Meaning of the function parameters.
    /// In order to make the code easy to understand, all variables in layout functions were named
    /// as if the code was updating horizontal layout only. In reality, the variable [`x`] can be
    /// set to either [`X`] or [`Y`] to update horizontal and vertical axis, respectively.
    fn divide_children_to_columns<Dim>(
        &self,
        x: Dim,
        opts: &AutoLayout,
        children: &[Instance],
    ) -> Vec<UnresolvedColumn>
    where
        Dim: ResolutionDim,
    {
        let columns_defs = opts.columns_and_rows.get_dim(x);
        let prim_axis_item_count = match opts.flow {
            AutoLayoutFlow::Row => opts.columns_and_rows_count.get_dim(X),
            AutoLayoutFlow::Column => opts.columns_and_rows_count.get_dim(Y),
        };
        let prim_axis_item_count = prim_axis_item_count.unwrap_or(children.len());
        if x.matches_flow_direction(opts.flow) {
            let column_defs = columns_defs.iter().cycle().enumerate().take(prim_axis_item_count);
            column_defs
                .map(|(i, axis)| {
                    let children_iter = children.iter().skip(i).step_by(prim_axis_item_count);
                    let children = children_iter.cloned().collect_vec();
                    let axis = *axis;
                    UnresolvedColumn { axis, children }
                })
                .collect_vec()
        } else {
            let column_count = children.len().div_ceil(prim_axis_item_count);
            let column_axes = columns_defs.iter().cycle().enumerate().take(column_count);
            column_axes
                .map(|(i, axis)| {
                    let skip_count = i * prim_axis_item_count;
                    let children_iter = children.iter().skip(skip_count).take(prim_axis_item_count);
                    let children = children_iter.cloned().collect_vec();
                    let axis = *axis;
                    UnresolvedColumn { axis, children }
                })
                .collect_vec()
        }
    }

    /// # Meaning of the function parameters.
    /// In order to make the code easy to understand, all variables in layout functions were named
    /// as if the code was updating horizontal layout only. In reality, the variable [`x`] can be
    /// set to either [`X`] or [`Y`] to update horizontal and vertical axis, respectively.
    fn resolve_columns<Dim>(
        &self,
        x: Dim,
        opts: &AutoLayout,
        unresolved_columns: Vec<UnresolvedColumn>,
    ) -> Vec<ResolvedColumn>
    where
        Dim: ResolutionDim,
    {
        let self_const_size = self.layout.size.get_dim(x).resolve_pixels_or_default();
        let columns = unresolved_columns
            .into_iter()
            .map(|column| {
                let children = column.children;
                let mut avg_child_grow = 0.0;
                let mut avg_child_shrink = 0.0;
                let mut max_child_min_size = 0.0;
                let mut min_child_max_size = f32::INFINITY;
                let mut max_child_size = 0.0;
                let mut max_child_fr = Fraction::default();
                for child in &children {
                    let child_grow_factor = child.layout.grow_factor.get_dim(x);
                    let child_shrink_factor = child.layout.shrink_factor.get_dim(x);

                    match child.layout.size.get_dim(x) {
                        Size::Hug => {
                            let child_can_grow_or_shrink =
                                child_grow_factor > 0.0 || child_shrink_factor > 0.0;
                            let refresh_child = child_can_grow_or_shrink
                                || child.should_propagate_parent_layout_refresh(x);
                            if refresh_child {
                                child.reset_size_to_static_values(x, self_const_size);
                                child.refresh_layout_internal(x, PassConfig::Default);
                            }
                        }
                        Size::Fixed(unit) => {
                            max_child_fr = max(max_child_fr, unit.as_fraction_or_default());
                            child.reset_size_to_static_values(x, self_const_size);
                        }
                    };

                    let child_margin = child.layout.margin.get_dim(x).resolve_pixels_or_default();
                    let child_size = child.layout.computed_size.get_dim(x) + child_margin.total();
                    let child_min_size =
                        child.layout.min_size.get_dim(x).resolve_pixels_or_default();
                    let child_max_size = child.layout.max_size.get_dim(x).resolve_pixels();
                    let child_max_size = child_max_size.unwrap_or(f32::INFINITY);
                    avg_child_grow += child_grow_factor;
                    avg_child_shrink += child_shrink_factor;
                    max_child_min_size = f32::max(max_child_min_size, child_min_size);
                    min_child_max_size = f32::min(min_child_max_size, child_max_size);
                    max_child_size = f32::max(max_child_size, child_size);
                }

                let child_count = children.len() as f32;
                let avg_child_grow = avg_child_grow / child_count;
                let avg_child_shrink = avg_child_shrink / child_count;

                let grow_factor = column.axis.grow_factor.unwrap_or(avg_child_grow);
                let shrink_factor = column.axis.shrink_factor.unwrap_or(avg_child_shrink);
                let min_size = column.axis.min_size.unwrap_or(max_child_min_size);
                let max_size = column.axis.max_size.unwrap_or(min_child_max_size);
                let max_child_fr_size = column.axis.size.as_fraction().unwrap_or(max_child_fr);

                let computed_size = match column.axis.size {
                    Size::Fixed(unit) => unit.resolve_pixels_or_default(),
                    Size::Hug => max_child_size,
                };
                let size = column.axis.size;
                let axis = ResolvedAxis {
                    size,
                    grow_factor,
                    shrink_factor,
                    computed_size,
                    min_size,
                    max_size,
                    max_child_fr_size,
                };
                ResolvedColumn { axis, children }
            })
            .collect_vec();

        if opts.reversed_columns_and_rows.get_dim(x) {
            columns.reversed()
        } else {
            columns
        }
    }

    fn shrink_or_grow_coeff(f: impl Fn(f32, f32) -> f32, total_factor: f32, space: f32) -> f32 {
        if total_factor > 0.0 {
            f(0.0, space / total_factor)
        } else {
            0.0
        }
    }

    fn shrink_coeff(total_factor: f32, space: f32) -> f32 {
        Self::shrink_or_grow_coeff(f32::min, total_factor, space)
    }

    fn grow_coeff(total_factor: f32, space: f32) -> f32 {
        Self::shrink_or_grow_coeff(f32::max, total_factor, space)
    }

    /// # Meaning of the function parameters.
    /// In order to make the code easy to understand, all variables in layout functions were named
    /// as if the code was updating horizontal layout only. In reality, the variable [`x`] can be
    /// set to either [`X`] or [`Y`] to update horizontal and vertical axis, respectively.
    fn refresh_grid_layout<Dim>(&self, x: Dim, opts: &AutoLayout)
    where Dim: ResolutionDim {
        let children = self.children();
        if children.is_empty() {
            return;
        }
        let old_child_computed_sizes: Vec<f32> =
            children.iter().map(|child| child.layout.computed_size.get_dim(x)).collect();

        let unresolved_columns = self.divide_children_to_columns(x, opts, &children);
        let mut columns = self.resolve_columns(x, opts, unresolved_columns);


        // === Compute the static size (no grow, shrink, nor fraction yet) ===

        let gap_def = opts.gap.get_dim(x);
        let padding_def = self.layout.padding.get_dim(x);
        let gap_count = (columns.len() - 1) as f32;
        let static_padding = padding_def.resolve_pixels_or_default().total();
        let static_gap = gap_count * gap_def.resolve_pixels_or_default();
        let self_size = self.layout.computed_size.get_dim(x);
        let mut space_left = self_size - static_padding - static_gap;
        let mut total_grow_factor = 0.0;
        let mut total_shrink_factor = 0.0;
        let mut total_fr = Fraction::default();
        for column in &columns {
            space_left -= column.computed_size;
            total_grow_factor += column.grow_factor;
            total_shrink_factor += column.shrink_factor;
            total_fr += column.max_child_fr_size;
        }
        total_fr += padding_def.start.as_fraction_or_default();
        total_fr += padding_def.end.as_fraction_or_default();
        total_fr += gap_def.as_fraction_or_default() * gap_count;
        if self.layout.size.get_dim(x).is_hug() && space_left < 0.0 {
            self.layout.computed_size.update_dim(x, |t| t - space_left);
            space_left = 0.0;
        }


        // === Resolve grow and shrink ===

        let self_size = self.layout.computed_size.get_dim(x);
        let padding_static = padding_def.resolve_pixels_or_default();
        let gap_static = gap_def.resolve_pixels_or_default();
        let total_gap_static = gap_count * gap_static;
        let shrink_coeff = Self::shrink_coeff(total_shrink_factor, space_left);
        space_left -= padding_static.total() + total_gap_static;

        let sorted_columns = columns.iter_mut().sorted_by(|a, b| a.max_size.total_cmp(&b.max_size));
        for column in sorted_columns {
            let grow_coeff = Self::grow_coeff(total_grow_factor, space_left);
            let grow_size = column.grow_factor * grow_coeff;
            let shrink_size = column.shrink_factor * shrink_coeff;
            let column_size = column.computed_size + grow_size + shrink_size;
            let column_size = f32::max(column.min_size, column_size);
            let column_size = f32::min(column.max_size, column_size);
            space_left -= column_size - column.computed_size;
            total_grow_factor -= column.grow_factor;
            column.computed_size = column_size;
        }


        // === Resolve fraction units and position the children ===

        let padding = padding_def.resolve(self_size, space_left, total_fr);
        let gap = gap_def.resolve(self_size, space_left, total_fr);
        let mut pos_x = padding.start;
        for column in &columns {
            let fr_diff = if total_fr > Fraction::from(0.0) {
                space_left * column.max_child_fr_size.unchecked_raw() / total_fr.unchecked_raw()
            } else {
                0.0
            };
            let column_size = column.computed_size + fr_diff;
            let column_size = f32::max(column.min_size, column_size);
            let column_size = f32::min(column.max_size, column_size);
            for (child, previous_size) in column.children.iter().zip(&old_child_computed_sizes) {
                let child_base_size = child.layout.computed_size.get_dim(x);
                let child_unused_space = f32::max(0.0, column_size - child_base_size);
                let unresolved_margin = child.layout.margin.get_dim(x);
                let margin_fr = unresolved_margin.as_fraction_or_default().total();
                let margin = unresolved_margin.resolve(self_size, child_unused_space, margin_fr);
                let column_size_minus_margin = column_size - margin.start - margin.end;

                let child_can_grow = child.layout.grow_factor.get_dim(x) > 0.0;
                let child_can_shrink = child.layout.shrink_factor.get_dim(x) > 0.0;
                if child_can_grow && child_base_size < column_size_minus_margin {
                    let size = f32::min(
                        column_size_minus_margin,
                        child.layout.max_size.get_dim(x).resolve_pixels_or_default(),
                    );
                    child.layout.computed_size.set_dim(x, size);
                }
                if let Some(fr) = child.layout.size.get_dim(x).as_fraction() {
                    if fr > Fraction::from(0.0) {
                        let size = f32::min(
                            column_size_minus_margin,
                            child.layout.max_size.get_dim(x).resolve_pixels_or_default(),
                        );
                        child.layout.computed_size.set_dim(x, size);
                    }
                }
                if child_can_shrink && child_base_size > column_size_minus_margin {
                    let size = f32::max(
                        column_size_minus_margin,
                        child.layout.min_size.get_dim(x).resolve_pixels_or_default(),
                    );
                    child.layout.computed_size.set_dim(x, size);
                }

                let child_size_changed = child_base_size != child.layout.computed_size.get_dim(x);
                let child_not_computed =
                    child.layout.size.get_dim(x).is_fixed() && child.should_refresh_layout();
                if child_size_changed || child_not_computed {
                    // Child size changed. There is one case when this might be a second call to
                    // refresh layout of the same child. If the child size is set to hug, the
                    // child can grow, and the column size is greater than earlier computed hugged
                    // child size, we need to refresh the child layout again.
                    child.refresh_layout_internal(x, PassConfig::DoNotHugDirectChildren);
                }

                let child_width = child.layout.computed_size.get_dim(x);
                let child_unused_space = f32::max(0.0, column_size_minus_margin - child_width);
                let def_alignment = opts.children_alignment.get_dim(x);
                let alignment = child.layout.alignment.get().get_dim(x).unwrap_or(def_alignment);
                let child_offset = child_unused_space * alignment.normalized();
                let child_left = pos_x + child_offset + margin.start;
                child.set_position_dim(x, child_left);
                if *previous_size != child_width {
                    child.dirty.computed_size.set();
                }
            }
            pos_x += column_size + gap;
        }
    }
}



// =================================================================================================
// === Public API ==================================================================================
// =================================================================================================

// ==============
// === Object ===
// ==============

/// The abstraction for any display object. In order to make your struct a display object, store
/// the `display::object::Instance` as a field and define impl of this trait. Every struct which
/// implements it, automatically implements the `display::object::ObjectOps`, and thus gets a lot
/// of methods implemented automatically.
#[allow(missing_docs)]
pub trait Object {
    fn display_object(&self) -> &Instance;
    fn weak_display_object(&self) -> WeakInstance {
        self.display_object().downgrade()
    }

    /// See `Any` description.
    fn into_any(self) -> Any
    where Self: Sized + 'static {
        Any { wrapped: Rc::new(self) }
    }
}

impl Object for Instance {
    fn display_object(&self) -> &Instance {
        self
    }
}

impl<T: Object + ?Sized> Object for &T {
    fn display_object(&self) -> &Instance {
        let t: &T = self;
        t.display_object()
    }
}

impl<T> Object for std::mem::ManuallyDrop<T>
where T: Object
{
    fn display_object(&self) -> &Instance {
        self.deref().display_object()
    }
}


// ==================
// === Any Object ===
// ==================

/// A structure wrapping any `Object` and hiding the exact type.
///
/// You can convert structure into `Any` using `Object::into_any`. Unfortunately it is not possible
/// to make general `From` implementation, because `Any` itself would use it as well, and it clashes
/// with base implementation `From<T> for T`.
#[derive(CloneRef)]
pub struct Any {
    wrapped: Rc<dyn Object>,
}

impl Clone for Any {
    fn clone(&self) -> Self {
        Self { wrapped: self.wrapped.clone() }
    }
}

impl Debug for Any {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "display::object::Any")
    }
}

impl Object for Any {
    fn display_object(&self) -> &Instance {
        self.wrapped.display_object()
    }
}



// =========================
// === UnsetParentOnDrop ===
// =========================

/// Wrapper that unsets parent of a display object when dropped. Please note that [`Instance`]
/// implements [`CloneRef`], so it can still be alive even if this struct is dropped.
#[derive(Debug, NoCloneBecauseOfCustomDrop)]
pub struct UnsetParentOnDrop {
    instance: Instance,
}

impl UnsetParentOnDrop {
    /// Constructor.
    pub fn new(instance: impl Into<Instance>) -> Self {
        let instance = instance.into();
        Self { instance }
    }
}

impl Drop for UnsetParentOnDrop {
    fn drop(&mut self) {
        self.instance.unset_parent()
    }
}



// =================================================================================================
// === Display Object Traits =======================================================================
// =================================================================================================

// =============================
// === Transformation Macros ===
// =============================

/// Generates getters and setters for display object transformations, such as `x()`, `xy()`,
/// `set_x()`, `rotation_z()`, `set_scale_x()`, etc.
macro_rules! gen_object_trans {
    ($trans:ident $(,$tx_name:ident)?) => {
        paste! {
            fn $trans(&self) -> Vector3<f32> {
                self.display_object().def.$trans()
            }

            #[enso_shapely::gen(
                skip_fields,
                update,
                set(trait = "IntoVector3<f32>", fn = "into_vector()"))
            ]
            fn [<modify_ $trans>](&self, f: impl FnOnce(&mut Vector3<f32>)) {
                self.display_object().def.[<modify_ $trans>](f)
            }

            fn [<set_ $trans _dim>]<D>(&self, dim: D, value: f32)
            where Vector3<f32>: DimSetter<D> {
                self.display_object().def.[<set_ $trans _dim>](dim, value)
            }

            fn [<update_ $trans _dim>]<D: Copy>(&self, dim: D, f: impl FnOnce(f32) -> f32)
            where Vector3<f32>: DimSetter<D> {
                self.display_object().def.[<update_ $trans _dim>](dim, f)
            }

            fn [<modify_ $trans _dim>]<D: Copy>(&self, dim: D, f: impl FnOnce(&mut f32))
            where Vector3<f32>: DimSetter<D> {
                self.display_object().def.[<modify_ $trans _dim>](dim, f)
            }
        }
        enso_types::with_swizzling_for_dim!(1, gen_getters, $trans $(,$tx_name)?);
        enso_types::with_swizzling_for_dim!(2, gen_getters, $trans $(,$tx_name)?);
        enso_types::with_swizzling_for_dim!(3, gen_getters, $trans $(,$tx_name)?);

        enso_types::with_swizzling_for_dim_unique!(1, gen_setters, $trans $(,$tx_name)?);
        enso_types::with_swizzling_for_dim_unique!(2, gen_setters, $trans $(,$tx_name)?);
        enso_types::with_swizzling_for_dim_unique!(3, gen_setters, $trans $(,$tx_name)?);
    };
}

macro_rules! gen_getters {
    ([$tx:tt] $_dim:tt $( $name:ident $dim:tt $_dim_ix:tt $_dim_ord:tt )*) => {
        gen_getters! {@ $tx $( $name $name $dim )* }
    };
    ([$tx:tt, $tx_name:tt] $_dim:tt $( $name:ident $dim:tt $_dim_ix:tt $_dim_ord:tt )*) => {
        gen_getters! {@ $tx $( [<$tx_name _ $name>] $name $dim )* }
    };
    (@ $tx:tt $( $fn_name:tt $name:tt $dim:tt )*) => { paste! {
        $( fn $fn_name(&self) -> [<Vector $dim>]<f32> { self.$tx().$name() } )*
    }};
}

macro_rules! gen_setters {
    ([$tx:tt] $_dim:tt $( $name:ident $dim:tt $_dim_ix:tt $_dim_ord:tt )*) => {
        gen_setters! {@ $tx $( [<set_ $name>] [<modify_ $name>] [<update_ $name>] $name $dim )* }
    };
    ([$tx:tt, $tx_name:tt] $_dim:tt $( $name:ident $dim:tt $_dim_ix:tt $_dim_ord:tt )*) => {
        gen_setters! {@ $tx $( [<set_ $tx_name _ $name>] [<modify_ $tx_name _ $name>]
            [<update_ $tx_name _ $name>] $name $dim )* }
    };
    (@ $tx:tt $( $set_name:tt $mod_name:tt $update_name:tt $name:tt $dim:tt )*) => { paste! {
        $(
            fn $set_name(&self, value: impl [<IntoVector $dim>]<f32>) {
                self.[<modify_ $tx>](|p| p.[<set_ $name>](value.into_vector()));
            }

            fn $mod_name<F>(&self, f: F)
            where F: FnOnce(&mut [<Vector $dim>]<f32>) {
                let mut value = self.$name();
                f(&mut value);
                self.$set_name(value);
            }

            fn $update_name<F>(&self, f: F)
            where F: FnOnce([<Vector $dim>]<f32>) -> [<Vector $dim>]<f32> {
                self.$set_name(f(self.$name()));
            }
        )*
    }};
}



// =================
// === ObjectOps ===
// =================

impl<T: Object + ?Sized> ObjectOps for T {}

/// Implementation of operations available for every struct which implements `display::Object`.
/// To learn more about the design, please refer to the documentation of [`Instance`].
#[allow(missing_docs)]
pub trait ObjectOps: Object + AutoLayoutOps + LayoutOps {
    // === Transformations ===

    gen_object_trans!(position);
    gen_object_trans!(rotation, rotation);
    gen_object_trans!(scale, scale);

    fn transformation_matrix(&self) -> Matrix4<f32> {
        self.display_object().def.transformation_matrix()
    }

    fn global_position(&self) -> Vector3<f32> {
        self.display_object().def.global_position()
    }


    // === Information ===

    /// Globally unique identifier of this display object.
    fn id(&self) -> Id {
        self.display_object().def.id()
    }


    // === Hierarchy ===

    /// Get the layer this object is displayed in. May be equal to layer explicitly set by the user
    /// or a layer inherited from the parent.
    fn display_layer(&self) -> Option<Layer> {
        self.display_object().def.display_layer()
    }

    /// Add another display object as a child to this display object. Children will inherit all
    /// transformations of their parents.
    fn add_child<T: Object + ?Sized>(&self, child: &T) {
        self.display_object().def.add_child(child.display_object());
    }

    fn new_child(&self) -> Instance {
        let child = Instance::new();
        self.add_child(&child);
        child
    }

    fn new_child_named(&self, name: &'static str) -> Instance {
        let child = Instance::new_named(name);
        self.add_child(&child);
        child
    }

    fn add_children<T: Object>(&self, children: impl IntoIterator<Item = T>) {
        self.display_object().def.add_children(children);
    }

    fn replace_children<T: Object>(&self, children: &[T]) {
        self.display_object().def.replace_children(children);
    }

    /// Remove the display object from the children list of this display object. Does nothing if
    /// the child was not registered.
    fn remove_child<T: Object>(&self, child: &T) {
        self.display_object().def.remove_child(child.display_object());
    }

    /// Removes this display object from its parent's children list.
    fn unset_parent(&self) {
        self.display_object().def.unset_parent();
    }

    /// Check whether this display object is attached to a parent.
    fn has_parent(&self) -> bool {
        self.display_object().def.has_parent()
    }

    /// Checks whether the object is visible.
    fn is_visible(&self) -> bool {
        self.display_object().def.is_visible()
    }


    // === EventModel ===

    /// Emit a new event. See docs of [`event::Event`] to learn more.
    fn emit_event<T>(&self, event: T)
    where T: 'static {
        self.display_object().def.emit_event(event)
    }

    /// Emit a new event that does not participate in the bubbling propagation phase. See docs of
    /// [`event::Event`] to learn more.
    fn emit_event_without_bubbling<T>(&self, event: T)
    where T: 'static {
        self.display_object().def.emit_event_without_bubbling(event)
    }

    /// Get event stream for bubbling events. See docs of [`event::Event`] to learn more.
    fn on_event<T>(&self) -> frp::Stream<event::Event<T>>
    where T: frp::Data {
        self.display_object().def.on_event()
    }

    /// Get event stream for capturing events. You should rather not need this function. Use
    /// [`on_event`] instead. See docs of [`event::Event`] to learn more.
    fn on_event_capturing<T>(&self) -> frp::Stream<event::Event<T>>
    where T: frp::Data {
        self.display_object().def.on_event_capturing()
    }

    /// Creates a new event with this object set to target.
    fn new_event<T: 'static>(&self, payload: T) -> event::SomeEvent {
        self.display_object().def.new_event(payload)
    }


    // === Focus ===

    /// Check whether this object is focused.
    fn is_focused(&self) -> bool {
        self.display_object().def.is_focused()
    }

    /// Focus this object. See docs of [`Event::Focus`] to learn more.
    fn focus(&self) {
        self.display_object().def.focus()
    }

    /// Blur ("unfocus") this object. See docs of [`Event::Blur`] to learn more.
    fn blur(&self) {
        self.display_object().def.blur()
    }

    /// Blur the display object tree this object belongs to. If any tree node (any node directly or
    /// indirectly connected with each other) was focused, it will be blurred.
    fn blur_tree(&self) {
        self.display_object().def.blur_tree()
    }

    /// Get the currently focused object if any. See docs of [`Event::Focus`] to learn more.
    fn focused_instance(&self) -> Option<Instance> {
        self.display_object().def.focused_instance()
    }


    // === Auto Layout settings ===

    /// Layout children using an auto-layout algorithm.
    fn use_auto_layout(&self) -> &Self {
        let instance = self.display_object();
        instance.def.set_layout(AutoLayout::default().enable());
        self
    }

    /// Do not layout children automatically.
    fn set_layout_manual(&self) {
        self.display_object().def.set_layout(None);
    }
}



// ========================
// === GenericLayoutApi ===
// ========================

/// Trait exposing the generic API for working with layouts. Until you are creating layouts shared
/// by multiple display objects and you need to store their configurations, you'd not need to use
/// it.
pub trait GenericLayoutApi: Object {
    /// Layout setter.
    fn set_layout(&self, layout: impl Into<Option<AutoLayout>>) {
        self.display_object().def.set_layout(layout)
    }

    /// Force layout refresh. You should not need to use it directly. IT can be helpful for testing
    /// and debugging though.
    fn refresh_layout(&self) {
        self.display_object().def.refresh_layout()
    }
}

impl<T: Object + ?Sized> GenericLayoutApi for T {}



// =======================
// === Hierarchy Tests ===
// =======================

#[cfg(test)]
mod hierarchy_tests {
    use super::*;
    use crate::display::world::World;
    use enso_frp::microtasks;
    use std::f32::consts::PI;

    fn update(node: &Instance, scene: &Scene) {
        node.update(scene);
        microtasks::flush_microtasks();
    }

    #[test]
    fn hierarchy_test() {
        let node1 = Instance::new();
        let node2 = Instance::new();
        let node3 = Instance::new();
        node1.add_child(&node2);
        assert_eq!(node2.my_index(), Some(ChildIndex(0)));

        node1.add_child(&node2);
        assert_eq!(node2.my_index(), Some(ChildIndex(1)));

        node1.add_child(&node3);
        assert_eq!(node3.my_index(), Some(ChildIndex(2)));

        node1.add_child(&node2);
        assert_eq!(node2.my_index(), Some(ChildIndex(3)));

        node1.remove_child(&node3);
        assert_eq!(node3.my_index(), None);
    }

    struct ReplaceChildrenTest<const N: usize> {
        root:  Instance,
        nodes: [Instance; N],
    }

    impl<const N: usize> ReplaceChildrenTest<N> {
        fn new() -> (Instance, [Instance; N], Self) {
            let root = Instance::new_named("root");
            let nodes = std::array::from_fn(|n| {
                Instance::new_named(Box::leak(format!("{n}").into_boxed_str()))
            });
            let nodes_clone = std::array::from_fn(|i| nodes[i].clone());
            (root.clone(), nodes_clone, Self { root, nodes })
        }

        fn prepare_clear_flags(&self) {
            self.root.dirty.modified_children.unset_all();
            self.root.dirty.removed_children.unset_all();
            for node in self.nodes.iter() {
                node.dirty.new_parent.unset();
            }
        }

        #[track_caller]
        fn new_node_parents(&self, node_has_new_parent: [bool; N]) {
            let status = std::array::from_fn(|n| self.nodes[n].dirty.new_parent.take().check());
            assert_eq!(status, node_has_new_parent);
        }

        #[track_caller]
        fn children(&self, expected: &[&'static str]) {
            let names = self.root.children().iter().map(|node| node.name).collect_vec();
            assert_eq!(names, expected);
        }

        #[track_caller]
        fn child_indices(&self, expected: &[usize]) {
            let indices = self
                .root
                .children()
                .iter()
                .map(|node| node.my_index().expect("No index").0)
                .collect_vec();
            assert_eq!(indices, expected);
        }

        #[track_caller]
        fn modified_children(&self, indices: &[usize]) {
            let modified = self.root.dirty.modified_children.take().set;
            let mut modified = modified.into_iter().map(|idx| idx.0).collect_vec();
            modified.sort();
            assert_eq!(modified, indices);
        }

        #[track_caller]
        fn removed_children<T: Object>(&self, instances: &[T]) {
            let mut removed = self.root.dirty.removed_children.take().set;
            for instance in instances {
                let instance = instance.display_object();
                let is_removed = removed.remove(&instance.downgrade());
                assert!(is_removed, "Missing removed instance: {:?}", instance.name);
            }
            assert!(
                removed.is_empty(),
                "Unexpected removed children: {:?}",
                removed.iter().map(|i| i.upgrade().map(|i| i.name)).collect_vec()
            );
        }

        #[track_caller]
        fn no_removed_children(&self) {
            self.removed_children::<Instance>(&[]);
        }
    }

    #[test]
    fn replace_children_identical_test() {
        let (root, nodes, assert) = ReplaceChildrenTest::<5>::new();
        root.replace_children(&nodes);
        assert.children(&["0", "1", "2", "3", "4"]);
        assert.child_indices(&[0, 1, 2, 3, 4]);
        assert.modified_children(&[0, 1, 2, 3, 4]);
        assert.removed_children::<Instance>(&[]);
        assert.new_node_parents([true, true, true, true, true]);

        root.replace_children(&nodes);
        assert.children(&["0", "1", "2", "3", "4"]);
        assert.child_indices(&[0, 1, 2, 3, 4]);
        assert.modified_children(&[]);
        assert.removed_children::<Instance>(&[]);
        assert.new_node_parents([false, false, false, false, false]);

        root.replace_children::<Instance>(&[]);
        assert.child_indices(&[]);
        assert.modified_children(&[]);
        assert.removed_children(&nodes);
        assert.new_node_parents([true, true, true, true, true]);
    }

    #[test]
    fn replace_children_subset_test() {
        let (root, nodes, assert) = ReplaceChildrenTest::<5>::new();
        root.replace_children(&nodes);
        assert.prepare_clear_flags();

        root.replace_children(&nodes[0..4]);
        assert.children(&["0", "1", "2", "3"]);
        assert.child_indices(&[0, 1, 2, 3]);
        assert.modified_children(&[]);
        assert.removed_children(&[&nodes[4]]);
        assert.new_node_parents([false, false, false, false, true]);


        root.replace_children(&nodes[1..4]);
        assert.children(&["1", "2", "3"]);
        assert.child_indices(&[0, 1, 2]);
        assert.modified_children(&[0, 1, 2]);
        assert.removed_children(&[&nodes[0]]);
        assert.new_node_parents([true, false, false, false, false]);

        root.replace_children(&nodes[2..5]);
        assert.children(&["2", "3", "4"]);
        assert.child_indices(&[0, 1, 2]);
        assert.modified_children(&[0, 1, 2]);
        assert.removed_children(&[&nodes[1]]);
        assert.new_node_parents([false, true, false, false, true]);

        root.replace_children(&nodes);
        assert.children(&["0", "1", "2", "3", "4"]);
        assert.modified_children(&[0, 1, 2, 3, 4]);
        assert.no_removed_children();
        assert.new_node_parents([true, true, false, false, false]);

        root.replace_children(&[&nodes[0], &nodes[2], &nodes[4]]);
        assert.children(&["0", "2", "4"]);
        assert.child_indices(&[0, 1, 2]);
        assert.modified_children(&[1, 2]);
        assert.removed_children(&[&nodes[1], &nodes[3]]);
        assert.new_node_parents([false, true, false, true, false]);

        root.replace_children(&nodes);
        assert.children(&["0", "1", "2", "3", "4"]);
        assert.modified_children(&[1, 2, 3, 4]);
        assert.no_removed_children();
        assert.new_node_parents([false, true, false, true, false]);
    }

    #[test]
    fn replace_children_shuffle_test() {
        let (root, nodes, assert) = ReplaceChildrenTest::<5>::new();
        root.replace_children(&nodes);
        assert.prepare_clear_flags();

        root.replace_children(&[&nodes[2..=4], &nodes[0..=1]].concat());
        assert.children(&["2", "3", "4", "0", "1"]);
        assert.child_indices(&[0, 1, 2, 3, 4]);
        assert.modified_children(&[0, 1, 2, 3, 4]);
        assert.no_removed_children();
        assert.new_node_parents([false, false, false, false, false]);

        root.replace_children(&nodes[0..=3]);
        assert.children(&["0", "1", "2", "3"]);
        assert.child_indices(&[0, 1, 2, 3]);
        assert.modified_children(&[0, 1, 2, 3]);
        assert.removed_children(&[&nodes[4]]);
        assert.new_node_parents([false, false, false, false, true]);

        root.replace_children(&[&nodes[4..=4], &nodes[1..=3], &nodes[0..=0]].concat());
        assert.children(&["4", "1", "2", "3", "0"]);
        assert.child_indices(&[0, 1, 2, 3, 4]);
        assert.modified_children(&[0, 4]);
        assert.no_removed_children();
        assert.new_node_parents([false, false, false, false, true]);

        root.replace_children(&nodes[1..=3]);
        assert.children(&["1", "2", "3"]);
        assert.child_indices(&[0, 1, 2]);
        assert.modified_children(&[0, 1, 2]);
        assert.removed_children(&[&nodes[0], &nodes[4]]);
        assert.new_node_parents([true, false, false, false, true]);

        root.replace_children(&nodes[1..=4]);
        assert.children(&["1", "2", "3", "4"]);
        assert.child_indices(&[0, 1, 2, 3]);
        assert.modified_children(&[3]);
        assert.no_removed_children();
        assert.new_node_parents([false, false, false, false, true]);
    }

    #[test]
    fn replace_children_keep_flags_test() {
        let (root, nodes, assert) = ReplaceChildrenTest::<5>::new();
        root.replace_children(&nodes);
        assert.prepare_clear_flags();

        assert.children(&["0", "1", "2", "3", "4"]);
        root.dirty.modified_children.set(ChildIndex(1));
        root.replace_children(&[&nodes[0..=2], &nodes[4..=4]].concat());
        assert.children(&["0", "1", "2", "4"]);
        root.replace_children(&nodes);
        assert.children(&["0", "1", "2", "3", "4"]);
        assert.modified_children(&[1, 3, 4]);
        assert.no_removed_children();
        assert.new_node_parents([false, false, false, true, false]);
    }

    fn replace_children_replace_all_test() {
        let (root, nodes, assert) = ReplaceChildrenTest::<5>::new();
        root.replace_children(&nodes);
        assert.prepare_clear_flags();

        let new_nodes: [_; 10] = std::array::from_fn(|_| Instance::new());
        root.replace_children(&new_nodes);
        assert_eq!(root.children(), &new_nodes);
        assert.child_indices(&[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
        assert.modified_children(&[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
        assert.removed_children(&nodes);
        assert.new_node_parents([true, true, true, true, true]);

        new_nodes.iter().enumerate().for_each(|(i, node)| {
            assert_eq!(node.my_index(), Some(ChildIndex(i)));
        });
        nodes.iter().for_each(|node| assert_eq!(node.my_index(), None));

        root.replace_children(&nodes);
        assert.children(&["0", "1", "2", "3", "4"]);
        assert.child_indices(&[0, 1, 2, 3, 4]);
        assert.modified_children(&[0, 1, 2, 3, 4]);
        assert.removed_children(&new_nodes);
        assert.new_node_parents([true, true, true, true, true]);
    }

    #[test]
    fn transformation_test() {
        let world = World::new();
        let scene = &world.default_scene;

        let node1 = Instance::new();
        let node2 = Instance::new();
        let node3 = Instance::new();
        assert_eq!(node1.position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node2.position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node3.position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node1.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(0.0, 0.0, 0.0));

        node1.modify_position(|t| t.x += 7.0);
        node1.add_child(&node2);
        node2.add_child(&node3);
        assert_eq!(node1.position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node3.position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node1.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(0.0, 0.0, 0.0));

        update(&node1, scene);
        assert_eq!(node1.position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node3.position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node1.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(7.0, 0.0, 0.0));

        node2.modify_position(|t| t.y += 5.0);
        update(&node1, scene);
        assert_eq!(node1.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(7.0, 5.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(7.0, 5.0, 0.0));

        node3.modify_position(|t| t.x += 1.0);
        update(&node1, scene);
        assert_eq!(node1.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(7.0, 5.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(8.0, 5.0, 0.0));

        node2.modify_rotation(|t| t.z += PI / 2.0);
        update(&node1, scene);
        assert_eq!(node1.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(7.0, 5.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(7.0, 6.0, 0.0));

        node1.add_child(&node3);
        update(&node1, scene);
        assert_eq!(node3.global_position(), Vector3::new(8.0, 0.0, 0.0));

        node1.remove_child(&node3);
        node3.update(scene);
        assert_eq!(node3.global_position(), Vector3::new(1.0, 0.0, 0.0));

        node2.add_child(&node3);
        update(&node1, scene);
        assert_eq!(node3.global_position(), Vector3::new(7.0, 6.0, 0.0));

        node1.remove_child(&node3);
        update(&node1, scene);
        node2.update(scene);
        node3.update(scene);
        assert_eq!(node3.global_position(), Vector3::new(7.0, 6.0, 0.0));
    }

    #[test]
    fn parent_test() {
        let node1 = Instance::new();
        let node2 = Instance::new();
        let node3 = Instance::new();
        node1.add_child(&node2);
        node1.add_child(&node3);
        node2.unset_parent();
        node3.unset_parent();
        assert_eq!(node1.children_count(), 0);
    }

    /// A utility to test display object instances' visibility.
    #[derive(Clone, CloneRef, Debug, Deref)]
    struct TestedNode {
        #[deref]
        node:         Instance,
        show_counter: Rc<Cell<usize>>,
        hide_counter: Rc<Cell<usize>>,
    }

    impl Object for TestedNode {
        fn display_object(&self) -> &Instance {
            &self.node
        }
    }

    impl TestedNode {
        fn new() -> Self {
            let node = Instance::new();
            let show_counter = Rc::<Cell<usize>>::default();
            let hide_counter = Rc::<Cell<usize>>::default();
            let network = &node.network;
            frp::extend! { network
                eval_ node.on_show(show_counter.set(show_counter.get() + 1));
                eval_ node.on_hide(hide_counter.set(hide_counter.get() + 1));
            }
            Self { node, show_counter, hide_counter }
        }

        fn reset_counters(&self) {
            self.show_counter.set(0);
            self.hide_counter.set(0);
        }

        fn check_if_was_shown(&self) {
            assert!(self.node.is_visible());
            assert_eq!(self.show_counter.get(), 1);
            assert_eq!(self.hide_counter.get(), 0);
            self.reset_counters();
        }

        fn check_if_was_hidden(&self) {
            assert!(!self.node.is_visible());
            assert_eq!(self.show_counter.get(), 0);
            assert_eq!(self.hide_counter.get(), 1);
            self.reset_counters();
        }

        fn check_if_visibility_did_not_changed(&self, expected_visibility: bool) {
            assert_eq!(self.node.is_visible(), expected_visibility);
            assert_eq!(self.show_counter.get(), 0);
            assert_eq!(self.hide_counter.get(), 0);
        }

        fn check_if_still_shown(&self) {
            self.check_if_visibility_did_not_changed(true)
        }
        fn check_if_still_hidden(&self) {
            self.check_if_visibility_did_not_changed(false)
        }
    }

    #[test]
    fn visibility_test() {
        let world = World::new();
        let scene = &world.default_scene;

        let node1 = TestedNode::new();
        let node2 = TestedNode::new();
        let node3 = TestedNode::new();
        node1.show();
        node3.check_if_still_hidden();
        update(&node3, scene);
        node3.check_if_still_hidden();

        node1.add_child(&node2);
        node2.add_child(&node3);
        update(&node1, scene);
        node3.check_if_was_shown();

        node3.unset_parent();
        node3.check_if_still_shown();

        update(&node1, scene);
        node3.check_if_was_hidden();

        node1.add_child(&node3);
        update(&node1, scene);
        node3.check_if_was_shown();

        node2.add_child(&node3);
        update(&node1, scene);
        node3.check_if_still_shown();

        node3.unset_parent();
        update(&node1, scene);
        node3.check_if_was_hidden();

        node2.add_child(&node3);
        update(&node1, scene);
        node3.check_if_was_shown();
    }

    #[test]
    fn visibility_test2() {
        let world = World::new();
        let scene = &world.default_scene;

        let node1 = TestedNode::new();
        let node2 = TestedNode::new();
        node1.check_if_still_hidden();
        update(&node1, scene);
        node1.check_if_still_hidden();
        node1.show();
        update(&node1, scene);
        node1.check_if_was_shown();

        node1.add_child(&node2);
        update(&node1, scene);
        node1.check_if_still_shown();
        node2.check_if_was_shown();
    }

    #[test]
    fn visibility_test3() {
        let world = World::new();
        let scene = &world.default_scene;

        let node1 = TestedNode::new();
        let node2 = TestedNode::new();
        let node3 = TestedNode::new();
        node1.show();
        node1.add_child(&node2);
        node2.add_child(&node3);
        update(&node1, scene);
        node2.check_if_was_shown();
        node3.check_if_was_shown();

        node3.unset_parent();
        node3.add_child(&node2);
        update(&node1, scene);
        node2.check_if_was_hidden();
        node3.check_if_was_hidden();
    }

    #[test]
    fn visibility_test4() {
        let world = World::new();
        let scene = &world.default_scene;

        let node1 = TestedNode::new();
        let node2 = TestedNode::new();
        let node3 = TestedNode::new();
        let node4 = TestedNode::new();
        node1.show();
        node1.add_child(&node2);
        node2.add_child(&node3);
        update(&node1, scene);
        node2.check_if_was_shown();
        node3.check_if_was_shown();
        node4.check_if_still_hidden();

        node2.unset_parent();
        node1.add_child(&node2);
        update(&node1, scene);
        node2.check_if_still_shown();
        node3.check_if_still_shown();
        node4.check_if_still_hidden();

        node1.add_child(&node4);
        node4.add_child(&node3);
        update(&node1, scene);
        node2.check_if_still_shown();
        // TODO[ao]: This assertion fails, see https://github.com/enso-org/ide/issues/1405
        // node3.check_if_still_shown();
        node3.reset_counters();
        node4.check_if_was_shown();

        node4.unset_parent();
        node2.unset_parent();
        update(&node1, scene);
        node2.check_if_was_hidden();
        node3.check_if_was_hidden();
        node4.check_if_was_hidden();

        node2.add_child(&node3);
        update(&node1, scene);
        node2.check_if_still_hidden();
        node3.check_if_still_hidden();
        node4.check_if_still_hidden();
    }


    #[test]
    fn deep_hierarchy_test() {
        // === Init ===
        let world = World::new();
        let scene = &world.default_scene;

        let root = Instance::new();
        let node1 = Instance::new();
        let node2 = Instance::new();
        let node3 = Instance::new();
        let node4 = Instance::new();
        let node5 = Instance::new();
        let node6 = Instance::new();

        root.show();

        root.add_child(&node1);
        node1.add_child(&node2);
        node2.add_child(&node3);
        node3.add_child(&node4);
        node4.add_child(&node5);
        node5.add_child(&node6);

        assert!(!node3.is_visible());
        assert!(!node4.is_visible());
        assert!(!node5.is_visible());
        assert!(!node6.is_visible());


        // === Init Update ===

        root.update(scene);

        assert!(node3.is_visible());
        assert!(node4.is_visible());
        assert!(node5.is_visible());
        assert!(node6.is_visible());

        assert_eq!(node1.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node4.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node5.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node6.global_position(), Vector3::new(0.0, 0.0, 0.0));


        // === Position Modification  ===

        node3.modify_position(|t| t.x += 1.0);
        node4.modify_position(|t| t.x += 3.0);
        node5.modify_position(|t| t.x += 5.0);
        node6.modify_position(|t| t.x += 7.0);

        root.update(scene);

        assert_eq!(node1.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(1.0, 0.0, 0.0));
        assert_eq!(node4.global_position(), Vector3::new(4.0, 0.0, 0.0));
        assert_eq!(node5.global_position(), Vector3::new(9.0, 0.0, 0.0));
        assert_eq!(node6.global_position(), Vector3::new(16.0, 0.0, 0.0));


        // === Visibility Modification  ===

        node4.unset_parent();
        node3.unset_parent();
        root.update(scene);

        assert!(!node3.is_visible());
        assert!(!node4.is_visible());
        assert!(!node5.is_visible());
        assert!(!node6.is_visible());
    }

    #[test]
    fn layers_test() {
        let world = World::new();
        let scene = &world.default_scene;

        let layer1 = Layer::new("0");
        let layer2 = Layer::new("1");
        let node1 = Instance::new();
        let node2 = Instance::new();
        let node3 = Instance::new();
        node1.add_child(&node2);
        node1.add_child(&node3);
        update(&node1, scene);
        assert_eq!(node1.display_layer(), None);
        assert_eq!(node2.display_layer(), None);
        assert_eq!(node3.display_layer(), None);

        node1.add_to_display_layer(&layer1);
        update(&node1, scene);
        assert_eq!(node1.display_layer().as_ref(), Some(&layer1));
        assert_eq!(node2.display_layer().as_ref(), Some(&layer1));
        assert_eq!(node3.display_layer().as_ref(), Some(&layer1));

        node2.add_to_display_layer(&layer2);
        update(&node1, scene);
        assert_eq!(node1.display_layer().as_ref(), Some(&layer1));
        assert_eq!(node2.display_layer().as_ref(), Some(&layer2));
        assert_eq!(node3.display_layer().as_ref(), Some(&layer1));
    }

    #[test]
    fn focus_consistency_test() {
        //         obj_root
        //         /      \
        // obj_left_1     obj_right_1
        //     |               |
        // obj_left_2     obj_right_2
        let obj_root = Instance::new();
        let obj_left_1 = Instance::new();
        let obj_left_2 = Instance::new();
        let obj_right_1 = Instance::new();
        let obj_right_2 = Instance::new();
        obj_root.add_child(&obj_left_1);
        obj_root.add_child(&obj_right_1);
        obj_left_1.add_child(&obj_left_2);
        obj_right_1.add_child(&obj_right_2);

        let check_focus_consistency = |focused: Option<&Instance>| {
            // Check that at most one object is focused and if so, that it is the correct one.
            assert_eq!(obj_root.is_focused(), focused == Some(&obj_root));
            assert_eq!(obj_left_1.is_focused(), focused == Some(&obj_left_1));
            assert_eq!(obj_left_2.is_focused(), focused == Some(&obj_left_2));
            assert_eq!(obj_right_1.is_focused(), focused == Some(&obj_right_1));
            assert_eq!(obj_right_2.is_focused(), focused == Some(&obj_right_2));

            // Check that all nodes contain the valid reference to the focused one.
            assert_eq!(obj_root.focused_instance().as_ref(), focused);
            assert_eq!(obj_left_1.focused_instance().as_ref(), focused);
            assert_eq!(obj_left_2.focused_instance().as_ref(), focused);
            assert_eq!(obj_right_1.focused_instance().as_ref(), focused);
            assert_eq!(obj_right_2.focused_instance().as_ref(), focused);

            // Check that focus information is correctly distributed across the branches.
            if focused == Some(&obj_root) {
                assert_eq!(obj_root.focused_descendant().as_ref(), focused);
                assert_eq!(obj_left_1.focused_descendant().as_ref(), None);
                assert_eq!(obj_left_2.focused_descendant().as_ref(), None);
                assert_eq!(obj_right_1.focused_descendant().as_ref(), None);
                assert_eq!(obj_right_2.focused_descendant().as_ref(), None);
            } else if focused == Some(&obj_left_1) {
                assert_eq!(obj_root.focused_descendant().as_ref(), focused);
                assert_eq!(obj_left_1.focused_descendant().as_ref(), focused);
                assert_eq!(obj_left_2.focused_descendant().as_ref(), None);
                assert_eq!(obj_right_1.focused_descendant().as_ref(), None);
                assert_eq!(obj_right_2.focused_descendant().as_ref(), None);
            } else if focused == Some(&obj_left_2) {
                assert_eq!(obj_root.focused_descendant().as_ref(), focused);
                assert_eq!(obj_left_1.focused_descendant().as_ref(), focused);
                assert_eq!(obj_left_2.focused_descendant().as_ref(), focused);
                assert_eq!(obj_right_1.focused_descendant().as_ref(), None);
                assert_eq!(obj_right_2.focused_descendant().as_ref(), None);
            } else if focused == Some(&obj_right_1) {
                assert_eq!(obj_root.focused_descendant().as_ref(), focused);
                assert_eq!(obj_left_1.focused_descendant().as_ref(), None);
                assert_eq!(obj_left_2.focused_descendant().as_ref(), None);
                assert_eq!(obj_right_1.focused_descendant().as_ref(), focused);
                assert_eq!(obj_right_2.focused_descendant().as_ref(), None);
            } else if focused == Some(&obj_right_2) {
                assert_eq!(obj_root.focused_descendant().as_ref(), focused);
                assert_eq!(obj_left_1.focused_descendant().as_ref(), None);
                assert_eq!(obj_left_2.focused_descendant().as_ref(), None);
                assert_eq!(obj_right_1.focused_descendant().as_ref(), focused);
                assert_eq!(obj_right_2.focused_descendant().as_ref(), focused);
            }
        };

        // === Checking the initial state ===

        check_focus_consistency(None);


        // === Checking if blurring works ===

        obj_left_1.focus();
        check_focus_consistency(Some(&obj_left_1));

        obj_left_2.blur();
        check_focus_consistency(Some(&obj_left_1));

        obj_left_1.blur();
        check_focus_consistency(None);


        // === Checking if focus stealing works ===

        obj_left_1.focus();
        check_focus_consistency(Some(&obj_left_1));

        obj_right_1.focus();
        check_focus_consistency(Some(&obj_right_1));

        obj_left_2.focus();
        check_focus_consistency(Some(&obj_left_2));

        obj_right_2.focus();
        check_focus_consistency(Some(&obj_right_2));

        obj_root.blur_tree();
        check_focus_consistency(None);


        // === Checking if detaching subtree removes focus from parent its parent ===

        obj_left_2.focus();
        check_focus_consistency(Some(&obj_left_2));

        obj_left_1.unset_parent();
        assert!(!obj_root.is_focused());
        assert!(!obj_left_1.is_focused());
        assert!(obj_left_2.is_focused());
        assert!(!obj_right_1.is_focused());
        assert!(!obj_right_2.is_focused());

        assert_eq!(obj_root.focused_instance().as_ref(), None);
        assert_eq!(obj_left_1.focused_instance().as_ref(), Some(&obj_left_2));
        assert_eq!(obj_left_2.focused_instance().as_ref(), Some(&obj_left_2));
        assert_eq!(obj_right_1.focused_instance().as_ref(), None);
        assert_eq!(obj_right_2.focused_instance().as_ref(), None);


        // === Checking if attaching subtree with a focus steals the existing one ===

        obj_right_2.focus();
        obj_root.add_child(&obj_left_1);
        check_focus_consistency(Some(&obj_left_2));
    }

    #[test]
    fn focus_event_propagation_test() {
        let obj_1 = Instance::new();
        let obj_2 = Instance::new();
        let obj_3 = Instance::new();
        obj_1.add_child(&obj_2);
        obj_2.add_child(&obj_3);

        let capturing_1 = obj_1.on_event_capturing::<f32>();
        let capturing_2 = obj_2.on_event_capturing::<f32>();
        let capturing_3 = obj_3.on_event_capturing::<f32>();
        let bubbling_1 = obj_1.on_event::<f32>();
        let bubbling_2 = obj_2.on_event::<f32>();
        let bubbling_3 = obj_3.on_event::<f32>();


        // === Event phases test ===

        let network = frp::Network::new("network");
        let out: Rc<RefCell<Vec<&'static str>>> = default();
        frp::extend! { network
            eval_ capturing_1 (out.borrow_mut().push("capturing_1"));
            eval_ capturing_2 (out.borrow_mut().push("capturing_2"));
            eval_ capturing_3 (out.borrow_mut().push("capturing_3"));
            eval_ bubbling_1 (out.borrow_mut().push("bubbling_1"));
            eval_ bubbling_2 (out.borrow_mut().push("bubbling_2"));
            eval_ bubbling_3 (out.borrow_mut().push("bubbling_3"));
        }

        obj_3.emit_event::<f32>(0.0);
        assert_eq!(&*out.borrow(), &[
            "capturing_1",
            "capturing_2",
            "capturing_3",
            "bubbling_3",
            "bubbling_2",
            "bubbling_1"
        ]);
        drop(network);


        // === Cancelling the event ===

        let network = frp::Network::new("network");
        let out: Rc<RefCell<Vec<&'static str>>> = default();
        frp::extend! { network
            eval_ capturing_1 (out.borrow_mut().push("capturing_1"));
            eval capturing_2 ([out] (e) {
                e.stop_propagation();
                out.borrow_mut().push("capturing_2")
            });
            eval_ capturing_3 (out.borrow_mut().push("capturing_3"));
            eval_ bubbling_1 (out.borrow_mut().push("bubbling_1"));
            eval_ bubbling_2 (out.borrow_mut().push("bubbling_2"));
            eval_ bubbling_3 (out.borrow_mut().push("bubbling_3"));
        }

        obj_3.emit_event::<f32>(0.0);
        assert_eq!(&*out.borrow(), &["capturing_1", "capturing_2",]);
        drop(network);


        // === Manual event creation ===

        let network = frp::Network::new("network");
        let out: Rc<RefCell<Vec<&'static str>>> = default();
        frp::extend! { network
            eval_ capturing_1 (out.borrow_mut().push("capturing_1"));
            eval_ capturing_2 (out.borrow_mut().push("capturing_2"));
            eval_ capturing_3 (out.borrow_mut().push("capturing_3"));
            eval_ bubbling_1 (out.borrow_mut().push("bubbling_1"));
            eval bubbling_2 ([out] (e) {
                e.stop_propagation();
                out.borrow_mut().push("bubbling_2")
            });
            eval_ bubbling_3 (out.borrow_mut().push("bubbling_3"));
        }

        let event = obj_3.new_event::<f32>(0.0);
        obj_3.event.source.emit(&event);
        assert_eq!(&*out.borrow(), &[
            "capturing_1",
            "capturing_2",
            "capturing_3",
            "bubbling_3",
            "bubbling_2"
        ]);
        drop(network);
    }
}



// ====================
// === Layout Tests ===
// ====================

#[cfg(test)]
mod layout_tests {
    use super::*;
    use crate::display::world::World;
    use enso_frp::microtasks;


    // === Utils ===

    /// Struct providing setup and utilities for testing a simple layout of objects – a root, and
    /// the provided number of its children. The following visualizations shows the default layout
    /// for three children:
    ///
    /// ```text
    /// ╔ root ══════════════ ▶ ◀ ════════════════════╗
    /// ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────┬──── ▶ ◀ ────╮ ║
    /// ║ │  ╭ node1 ╮  ┆  ╭ node2 ╮  ┆  ╭ node3 ╮  ▼ ▼
    /// ║ │  ╰───────╯  ┆  ╰───────╯  ┆  ╰───────╯  ▲ ▲
    /// ║ ╰─────────────┴─────────────┴─────────────╯ ║
    /// ╚═════════════════════════════════════════════╝
    /// ```
    macro_rules! gen_test_flat_children {
        ($total:tt [$($num:tt),*]) => { paste! {
            #[derive(Debug)]
            pub struct [<TestFlatChildren $total>] {
                world: World,
                root:  Instance,
                $([<node $num>]: Instance),*
            }

            impl [<TestFlatChildren $total>] {
                fn new() -> Self {
                    let world = World::new();
                    let root = Instance::new_named("root");
                    $(let [<node $num>] = Instance::new_named(stringify!([<node $num>]));)*
                    world.add_child(&root);
                    $(root.add_child(&[<node $num>]);)*
                    Self { world, root, $([<node $num>]),* }
                }

                fn reset_positions(&self) {
                    self.root.set_position(Vector3(0.0, 0.0, 0.0));
                    $(self.[<node $num>].set_position(Vector3(0.0, 0.0, 0.0));)*
                }

                fn run(&self, assertions: impl Fn()) -> &Self {
                    let update = || {
                        self.world.display_object().update(&self.world.default_scene);
                        microtasks::flush_microtasks();
                    };
                    update();
                    assertions();
                    // Nothing should change if nothing happened.
                    update();
                    assertions();
                    // Check also if the world being dirty also does not affect the `root`.
                    self.world.display_object().set_position((0.0, 0.0, 0.0));
                    update();
                    assertions();
                    self
                }

                #[track_caller]
                fn assert_root_position(&self, x:f32, y:f32) -> &Self {
                    assert_eq!(self.root.position().xy(), Vector2(x,y));
                    self
                }

                #[track_caller]
                fn assert_root_computed_size(&self, x:f32, y:f32) -> &Self {
                    assert_eq!(self.root.computed_size(), Vector2(x,y));
                    self
                }

                $(
                    #[track_caller]
                    fn [<assert_node $num _position>](&self, x:f32, y:f32) -> &Self {
                        assert_eq!(self.[<node $num>].position().xy(), Vector2(x,y));
                        self
                    }

                    #[track_caller]
                    fn [<assert_node $num _computed_size>](&self, x:f32, y:f32) -> &Self {
                        assert_eq!(self.[<node $num>].computed_size(), Vector2(x,y));
                        self
                    }
                )*
            }
        }};
    }

    gen_test_flat_children!(1[1]);
    gen_test_flat_children!(2 [1,2]);
    gen_test_flat_children!(3 [1,2,3]);


    // === Tests ===

    /// ```text
    /// ╭─ ▶ ◀ ─╮
    /// │ root  ▼
    /// │       ▲
    /// ╰───────╯
    /// ```
    #[test]
    fn test_empty_auto_layout() {
        let world = World::new();
        let root = Instance::new_named("Root");

        root.set_size((10.0, 10.0));
        root.update(&world.default_scene);
        assert_eq!(root.computed_size(), Vector2(10.0, 10.0));

        root.set_size_hug();
        root.update(&world.default_scene);
        assert_eq!(root.computed_size(), Vector2(0.0, 0.0));
    }


    #[test]
    fn test_layout_double_update() {
        let world = World::new();
        let root = Instance::new_named("Root");
        root.use_auto_layout();
        let child = root.new_child();
        child.set_size((10.0, 10.0));
        root.update(&world.default_scene);
        assert_eq!(root.computed_size(), Vector2(10.0, 10.0));
        root.update(&world.default_scene);
        assert_eq!(root.computed_size(), Vector2(10.0, 10.0));
    }

    /// Input:
    ///
    /// ```text
    /// ╔ root ════════════════════════════╗
    /// ║ ╭──────────────┬───────────────╮ ║
    /// ║ │              ┆            △  │ ║
    /// ║ │ ╭── ▶ ◀ ──┬▷ ┆  ╭ R ▶ ◀ ──┤  │ ║
    /// ║ │ │ L       │  ┆  │      △  │  │ ║
    /// ║ │ │ ╭ L1 ┬▷ │  ┆  │ ╭ R2 ┤  │  │ ║
    /// ║ │ │ │    │  │  ┆  │ │    │  │  │ ║                 
    /// ║ │ │ │    │  ▼  ┆  │ ╰────╯  │  ▼ ║ 10            
    /// ║ │ │ │    │  ▲  ┆  │      △  │  ▲ ║
    /// ║ │ │ │    │  │  ┆  │ ╭ R1 ┤  │  │ ║
    /// ║ │ │ │    │  │  ┆  │ │    │  │  │ ║
    /// ║ │ │ ╰────╯  │  ┆  │ ╰────╯  │  │ ║
    /// ║ │ ╰─────────╯  │  ╰─────────╯  │ ║
    /// ║ ╰──────────────┴───────────────╯ ║
    /// ╚══════════════════════════════════╝
    ///                 10
    /// ```
    #[test]
    fn test_mixed_layouts() {
        let world = World::new();
        let root = Instance::new_named("Root");
        let l = root.new_child_named("L");
        let r = root.new_child_named("R");
        let l1 = l.new_child_named("L1");
        let r1 = r.new_child_named("R1");
        let r2 = r.new_child_named("R2");

        root.use_auto_layout();
        root.set_size((10.0, 10.0));

        l.use_auto_layout().set_alignment_center();
        l.set_size_y_to_hug().allow_grow_x();
        l1.set_size((0.0, 4.0)).allow_grow_x();

        r.use_auto_layout().set_column_count(1);
        r.set_size_x_to_hug().allow_grow_y();
        r1.set_size((2.0, 0.0)).allow_grow_y();
        r2.set_size((3.0, 0.0)).allow_grow_y();

        let check = || {
            root.update(&world.default_scene);

            assert_eq!(root.position().xy(), Vector2(0.0, 0.0));
            assert_eq!(l.position().xy(), Vector2(0.0, 3.0));
            assert_eq!(r.position().xy(), Vector2(7.0, 0.0));
            assert_eq!(l1.position().xy(), Vector2(0.0, 0.0));
            assert_eq!(r1.position().xy(), Vector2(0.0, 0.0));
            assert_eq!(r2.position().xy(), Vector2(0.0, 5.0));

            assert_eq!(root.computed_size(), Vector2(10.0, 10.0));
            assert_eq!(l.computed_size(), Vector2(7.0, 4.0));
            assert_eq!(r.computed_size(), Vector2(3.0, 10.0));
            assert_eq!(r1.computed_size(), Vector2(2.0, 5.0));
            assert_eq!(r2.computed_size(), Vector2(3.0, 5.0));
        };
        check();
        // Check if nothing happens when nothing happens.
        check();
    }

    /// ```text
    /// ╔ root ══════════════ ▶ ◀ ════════════════════╗
    /// ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────┬──── ▶ ◀ ────╮ ║
    /// ║ │             ┆             ┆             │ ║
    /// ║ │             ┆             ┆  ╭ node3 ╮  │ ║
    /// ║ │             ┆  ╭ node2 ╮  ┆  │       │  │ ║
    /// ║ │  ╭ node1 ╮  ┆  │       │  ┆  │       │  ▼ ▼
    /// ║ │  │       │  ┆  │       │  ┆  │       │  ▲ ▲
    /// ║ │  │       │  ┆  │       │  ┆  │       │  │ ║
    /// ║ │  ╰───────╯  ┆  ╰───────╯  ┆  ╰───────╯  │ ║
    /// ║ ╰─────────────┴─────────────┴─────────────╯ ║
    /// ╚═════════════════════════════════════════════╝
    /// ```
    #[test]
    fn test_horizontal_layout_with_fixed_children() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.node1.set_size((1.0, 1.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((3.0, 3.0));
        test.run(|| {
            test.assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(1.0, 0.0)
                .assert_node3_position(3.0, 0.0)
                .assert_root_computed_size(6.0, 3.0)
                .assert_node1_computed_size(1.0, 1.0)
                .assert_node2_computed_size(2.0, 2.0)
                .assert_node3_computed_size(3.0, 3.0);
        });
    }

    /// ```text
    /// ╔ root ══ ▶ ◀ ════════╗
    /// ║ ╭────── ▶ ◀ ──────╮ ║
    /// ║ │  ╭ node3 ────╮  │ ║
    /// ║ │  │           │  ▼ ║
    /// ║ │  │           │  ▲ ║
    /// ║ │  ╰───────────╯  │ ║
    /// ║ ├╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┤ ║
    /// ║ │  ╭ node2 ──╮    │ ║
    /// ║ │  │         │    ▼ ▼
    /// ║ │  │         │    ▲ ▲
    /// ║ │  ╰─────────╯    │ ║
    /// ║ ├╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┤ ║
    /// ║ │  ╭ node1 ╮      │ ║
    /// ║ │  │       │      ▼ ║
    /// ║ │  │       │      ▲ ║
    /// ║ │  ╰───────╯      │ ║
    /// ║ ╰─────────────────╯ ║
    /// ╚═════════════════════╝
    /// ```
    #[test]
    fn test_vertical_layout_with_fixed_children() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_column_count(1);
        test.node1.set_size((1.0, 1.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((3.0, 3.0));
        test.run(|| {
            test.assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(0.0, 1.0)
                .assert_node3_position(0.0, 3.0)
                .assert_root_computed_size(3.0, 6.0)
                .assert_node1_computed_size(1.0, 1.0)
                .assert_node2_computed_size(2.0, 2.0)
                .assert_node3_computed_size(3.0, 3.0);
        });
    }

    /// ```text
    /// ╔ root ══════════════ ▶ ◀ ═════════════════════╗
    /// ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────┬──── ▶ ◀ ────╮  ║
    /// ║ │  ╭ node1 ╮  ┆  ╭ node2 ╮  ┆  ╭ node3 ╮  │  ║
    /// ║ │  │       │  ┆  │       │  ┆  │       ▼  ▼  ▼
    /// ║ │  │       │  ┆  │       │  ┆  │       ▲  ▲  ▲
    /// ║ │  ╰─ ▶ ◀ ─╯  ┆  ╰───────╯  ┆  ╰─ ▶ ◀ ─╯  │  ║
    /// ║ ╰─────────────┴─────────────┴─────────────╯  ║
    /// ╚══════════════════════════════════════════════╝
    /// ```
    #[test]
    fn test_horizontal_layout_with_hug_children() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.node1.set_size_hug_y(1.0);
        test.node2.set_size((2.0, 2.0));
        test.run(|| {
            test.assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(0.0, 0.0)
                .assert_node3_position(2.0, 0.0)
                .assert_root_computed_size(2.0, 2.0)
                .assert_node1_computed_size(0.0, 1.0)
                .assert_node2_computed_size(2.0, 2.0)
                .assert_node3_computed_size(0.0, 0.0);
        });
    }

    /// ```text
    /// ╔ root ═════════════════════════════════════════════╗
    /// ║ ╭──── ▶ ◀ ────┬──────── ▶ ◀ ───────┬──── ▶ ◀ ───╮ ║
    /// ║ │  ╭ node1 ╮  ┆  ╭ node2 ┬────────▷┆  ╭ node3 ╮ │ ║
    /// ║ │  │       │  ┆  │       │         ┆  │       │ ▼ ▼
    /// ║ │  │       │  ┆  │       │         ┆  │       │ ▲ ▲
    /// ║ │  ╰─ ▶ ◀ ─╯  ┆  ╰───────╯         ┆  ╰───────╯ │ ║
    /// ║ ╰─────────────┴────────────────────┴────────────╯ ║
    /// ╚═══════════════════════════════════════════════════╝
    /// ```
    #[test]
    fn test_horizontal_layout_with_children_that_grow() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.root.set_size_x_hug(10.0);
        test.node1.set_size_hug_y(1.0);
        test.node2.set_size((2.0, 2.0)).allow_grow_x();
        test.node3.set_size((3.0, 3.0));
        test.run(|| {
            test.assert_root_computed_size(10.0, 3.0)
                .assert_node1_computed_size(0.0, 1.0)
                .assert_node2_computed_size(7.0, 2.0)
                .assert_node3_computed_size(3.0, 3.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(0.0, 0.0)
                .assert_node3_position(7.0, 0.0);
        });
    }

    /// ```text
    /// ╔ root ═══════════════════════════════════════════╗
    /// ║ ╭──── ▶ ◀ ────┬────── ▶ ◀ ───┬────── ▶ ◀ ─────╮ ║
    /// ║ │  ╭ node1 ╮  ┆  ╭ node2 ┬─▷┤┆  ╭ node3 ╮     │ ║
    /// ║ │  │       │  ┆  │       │   ┆  │       │     ▼ ▼
    /// ║ │  │       │  ┆  │       │   ┆  │       │     ▲ ▲
    /// ║ │  ╰─ ▶ ◀ ─╯  ┆  ╰───────╯   ┆  ╰───────╯     │ ║
    /// ║ ╰─────────────┴──────────────┴────────────────╯ ║
    /// ╚═════════════════════════════════════════════════╝
    /// ```
    #[test]
    fn test_horizontal_layout_with_children_that_grow_to_a_limit() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.root.set_size_x_hug(10.0);
        test.node1.set_size_hug_y(1.0);
        test.node2.set_size((2.0, 2.0)).allow_grow_x().set_max_size_x(4.0);
        test.node3.set_size((3.0, 3.0));
        test.run(|| {
            test.assert_root_computed_size(10.0, 3.0)
                .assert_node1_computed_size(0.0, 1.0)
                .assert_node2_computed_size(4.0, 2.0)
                .assert_node3_computed_size(3.0, 3.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(0.0, 0.0)
                .assert_node3_position(4.0, 0.0);
        });
    }

    /// ```text
    /// ╔ root ═══════════════════════════════════════════╗
    /// ║ ╭──── ▶ ◀ ────┬────── ▶ ◀ ───┬────── ▶ ◀ ─────╮ ║
    /// ║ │  ╭ node1 ╮  ┆  ╭ node2 ┬─▷┤┆  ╭ node3 ┬────▷│ ║
    /// ║ │  │       │  ┆  │       │   ┆  │       │     ▼ ▼
    /// ║ │  │       │  ┆  │       │   ┆  │       │     ▲ ▲
    /// ║ │  ╰─ ▶ ◀ ─╯  ┆  ╰───────╯   ┆  ╰───────╯     │ ║
    /// ║ ╰─────────────┴──────────────┴────────────────╯ ║
    /// ╚═════════════════════════════════════════════════╝
    /// ```
    #[test]
    fn test_horizontal_layout_with_children_with_mixed_grow() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.root.set_size_x_hug(10.0);
        test.node1.set_size_hug_y(1.0);
        test.node2.set_size((2.0, 2.0)).allow_grow_x().set_max_size_x(4.0);
        test.node3.set_size((2.0, 3.0)).allow_grow_x();
        test.run(|| {
            test.assert_root_computed_size(10.0, 3.0)
                .assert_node1_computed_size(0.0, 1.0)
                .assert_node2_computed_size(4.0, 2.0)
                .assert_node3_computed_size(6.0, 3.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(0.0, 0.0)
                .assert_node3_position(4.0, 0.0);
        });
    }

    /// ```text
    /// ╔ root ══════════════════════════════╗
    /// ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────┬▶ ◀ ╮ ║
    /// ║ │  ╭ node1 ╮  ┆  ╭ node2 ╮  ┆╭ node3 ╮
    /// ▼ ▼  │       │  ┆  │       │  ┆│   │ ║ │
    /// ▲ ▲  │       │  ┆  │       │  ┆│   │ ║ │
    /// ║ │  ╰───────╯  ┆  ╰─────┼◁╯  ┆╰───┼─╫─╯
    /// ║ ╰─────────────┴─────────────┴────╯ ║
    /// ╚════════════════════════════════════╝
    /// ```
    #[test]
    fn test_horizontal_layout_with_children_that_shrink_to_a_limit() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.root.set_size_x_hug(4.0);
        test.node1.set_size((1.0, 1.0));
        test.node2.set_size((2.0, 2.0)).allow_shrink_x().set_min_size_x(1.0);
        test.node3.set_size((3.0, 3.0));
        test.run(|| {
            test.assert_root_computed_size(4.0, 3.0)
                .assert_node1_computed_size(1.0, 1.0)
                .assert_node2_computed_size(1.0, 2.0)
                .assert_node3_computed_size(3.0, 3.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(1.0, 0.0)
                .assert_node3_position(2.0, 0.0);
        });
    }

    /// ```text
    /// ╔ root ═══════════════════════════════════════════════╗
    /// ║ ╭──── ▶ ◀ ────┬──────── ▶ ◀ ────────┬──── ▶ ◀ ────╮ ║
    /// ║ │             ┆                  △  ┆             │ ║
    /// ║ ▼             ┆  ╭ node2 ────────┼─▷┆  ╭ node3 ╮  │ ║
    /// ║ ▲  ╭ node1 ╮  ┆  │  ╭ node2_1 ╮  ▼  ┆  │       │  │ ▼
    /// ║ │  │       │  ┆  │  ╰─────────╯  ▲  ┆  │       │  │ ▲
    /// ║ │  ╰───────╯  ┆  ╰───── ▶ ◀ ─────╯  ┆  ╰───────╯  │ ║
    /// ║ ╰─────────────┴─────────────────────┴─────────────╯ ║
    /// ╚═════════════════════════════════════════════════════╝
    /// ```
    #[test]
    fn test_hug_child_that_can_grow_in_a_hug_column() {
        let test = TestFlatChildren3::new();
        let node2_1 = Instance::new_named("node2_1");
        test.node2.add_child(&node2_1);

        test.root.use_auto_layout();
        test.root.set_size_x_hug(10.0);
        test.node1.set_size((1.0, 1.0));
        test.node2.use_auto_layout();
        test.node2.allow_grow_x().allow_grow_y();
        node2_1.set_size((1.0, 1.0));
        test.node3.set_size((3.0, 3.0));
        test.run(|| {
            test.assert_root_computed_size(10.0, 3.0)
                .assert_node1_computed_size(1.0, 1.0)
                .assert_node2_computed_size(6.0, 3.0)
                .assert_node3_computed_size(3.0, 3.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(1.0, 0.0)
                .assert_node3_position(7.0, 0.0);
        });
        assert_eq!(node2_1.computed_size(), Vector2(1.0, 1.0));
        assert_eq!(node2_1.position().xy(), Vector2(0.0, 0.0));
    }

    /// ```text
    /// ╔ root ═════════════════ ▶ ◀ ══════════════════════╗
    /// ║ ╭──── ▶ ◀ ───┬──────── ▶ ◀ ────────┬─── ▶ ◀ ───╮ ║
    /// ║ │            ┆ ╱╱╱╱╱╱           ╱╱ ┆           │ ║
    /// ║ │            ┆ ╱╱╱╱╱╱           ╱╱ ┆ ╭ node3 ╮ │ ║
    /// ║ │            ┆ ╱╱╱╱╱╱ ╭ node2 ╮ ╱╱ ┆ │       │ │ ║
    /// ║ │  ╭ node1 ╮ ┆ ╱╱╱╱╱╱ │       │ ╱╱ ┆ │       │ ▼ ▼
    /// ║ │  │       │ ┆ ╱╱╱╱╱╱ │       │ ╱╱ ┆ │       │ ▲ ▲
    /// ║ │  │       │ ┆ ╱╱╱╱╱╱ │       │ ╱╱ ┆ │       │ │ ║
    /// ║ │  ╰───────╯ ┆ ╱╱╱╱╱╱ ╰───────╯ ╱╱ ┆ ╰───────╯ │ ║
    /// ║ ╰────────────┴─────────────────────┴───────────╯ ║
    /// ╚══════════════════════════════════════════════════╝
    /// ```
    #[test]
    fn test_horizontal_layout_with_fixed_children_and_margin() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.node1.set_size((1.0, 1.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((3.0, 3.0));
        test.node2.set_margin_left(10.0);
        test.node2.set_margin_right(1.0);
        test.run(|| {
            test.assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(11.0, 0.0)
                .assert_node3_position(14.0, 0.0)
                .assert_root_computed_size(17.0, 3.0)
                .assert_node1_computed_size(1.0, 1.0)
                .assert_node2_computed_size(2.0, 2.0)
                .assert_node3_computed_size(3.0, 3.0);
        });
    }

    /// ```text
    /// ╔ root ═══════════════ ▶ ◀ ═════════════════════╗
    /// ║ ╭───── ▶ ◀ ─────┬─── ▶ ◀ ───┬───── ▶ ◀ ─────╮ ║
    /// ║ │ ╱╱╱╱╱╱╱╱╱╱╱╱╱╱┆╱╱╱╱╱╱╱╱╱╱╱┆╱╱╱╱╱╱╱╱╱╱╱╱╱╱ │ ║
    /// ║ │ ╱╱╱           ┆           ┆ ╭ node3 ╮ ╱╱╱ │ ║
    /// ║ │ ╱╱╱           ┆ ╭ node2 ╮ ┆ │       │ ╱╱╱ │ ║
    /// ║ │ ╱╱╱ ╭ node1 ╮ ┆ │       │ ┆ │       │ ╱╱╱ ▼ ▼
    /// ║ │ ╱╱╱ │       │ ┆ │       │ ┆ │       │ ╱╱╱ ▲ ▲
    /// ║ │ ╱╱╱ │       │ ┆ │       │ ┆ │       │ ╱╱╱ │ ║
    /// ║ │ ╱╱╱ ╰───────╯ ┆ ╰───────╯ ┆ ╰───────╯ ╱╱╱ │ ║
    /// ║ │ ╱╱╱╱╱╱╱╱╱╱╱╱╱╱┆╱╱╱╱╱╱╱╱╱╱╱┆╱╱╱╱╱╱╱╱╱╱╱╱╱╱ │ ║
    /// ║ ╰───────────────┴───────────┴───────────────╯ ║
    /// ╚═══════════════════════════════════════════════╝
    /// ```
    #[test]
    fn test_horizontal_layout_with_fixed_children_and_padding() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.root.set_padding_all(10.0);
        test.node1.set_size((1.0, 1.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((3.0, 3.0));
        test.run(|| {
            test.assert_root_position(0.0, 0.0)
                .assert_node1_position(10.0, 10.0)
                .assert_node2_position(11.0, 10.0)
                .assert_node3_position(13.0, 10.0)
                .assert_root_computed_size(26.0, 23.0)
                .assert_node1_computed_size(1.0, 1.0)
                .assert_node2_computed_size(2.0, 2.0)
                .assert_node3_computed_size(3.0, 3.0);
        });
    }

    /// ```text
    /// ╔ root ═══════ ▶ ◀ ═════════════╗
    /// ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────╮ ║
    /// ║ │  ╭ node3 ╮  ┆             │ ║
    /// ║ │  │       │  ┆             ▼ ║
    /// ║ │  │       │  ┆             ▲ ║
    /// ║ │  ╰───────╯  ┆             │ ▼
    /// ║ ├╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌┤ ▲
    /// ║ │  ╭ node1 ╮  ┆  ╭ node2 ╮  │ ║
    /// ║ │  │       │  ┆  │       │  ▼ ║
    /// ║ │  │       │  ┆  │       │  ▲ ║
    /// ║ │  ╰───────╯  ┆  ╰───────╯  │ ║
    /// ║ ╰─────────────┴─────────────╯ ║
    /// ╚═══════════════════════════════╝
    /// ```
    #[test]
    fn test_simple_grid_layout() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_column_count(2);
        test.node1.set_size((2.0, 2.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((2.0, 2.0));
        test.run(|| {
            test.assert_root_computed_size(4.0, 4.0)
                .assert_node1_computed_size(2.0, 2.0)
                .assert_node2_computed_size(2.0, 2.0)
                .assert_node3_computed_size(2.0, 2.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(2.0, 0.0)
                .assert_node3_position(0.0, 2.0);
        });
    }

    /// ```text
    /// ╔ root ═══════ ▶ ◀ ═════════════╗
    /// ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────╮ ║
    /// ║ │  ╭ node2 ╮  ┆             │ ║
    /// ║ │  │       │  ┆             ▼ ║
    /// ║ │  │       │  ┆             ▲ ║
    /// ║ │  ╰───────╯  ┆             │ ▼
    /// ║ ├╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌┤ ▲
    /// ║ │  ╭ node1 ╮  ┆  ╭ node3 ╮  │ ║
    /// ║ │  │       │  ┆  │       │  ▼ ║
    /// ║ │  │       │  ┆  │       │  ▲ ║
    /// ║ │  ╰───────╯  ┆  ╰───────╯  │ ║
    /// ║ ╰─────────────┴─────────────╯ ║
    /// ╚═══════════════════════════════╝
    /// ```
    #[test]
    fn test_simple_grid_layout_with_column_flow() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_row_count(2).set_column_flow();
        test.node1.set_size((2.0, 2.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((2.0, 2.0));
        test.run(|| {
            test.assert_root_computed_size(4.0, 4.0)
                .assert_node1_computed_size(2.0, 2.0)
                .assert_node2_computed_size(2.0, 2.0)
                .assert_node3_computed_size(2.0, 2.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(0.0, 2.0)
                .assert_node3_position(2.0, 0.0);
        });
    }

    /// ```text
    /// ╔ root ═══════ ▶ ◀ ═════════════╗
    /// ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────╮ ║
    /// ║ │             ┆  ╭ node3 ╮  │ ║
    /// ║ │             ┆  │       │  ▼ ║
    /// ║ │             ┆  │       │  ▲ ║
    /// ║ │             ┆  ╰───────╯  │ ▼
    /// ║ ├╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌┤ ▲
    /// ║ │  ╭ node2 ╮  ┆  ╭ node1 ╮  │ ║
    /// ║ │  │       │  ┆  │       │  ▼ ║
    /// ║ │  │       │  ┆  │       │  ▲ ║
    /// ║ │  ╰───────╯  ┆  ╰───────╯  │ ║
    /// ║ ╰─────────────┴─────────────╯ ║
    /// ╚═══════════════════════════════╝
    /// ```
    #[test]
    fn test_simple_grid_layout_reversed_x() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_column_count(2).reverse_columns();
        test.node1.set_size((2.0, 2.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((2.0, 2.0));
        test.run(|| {
            test.assert_root_computed_size(4.0, 4.0)
                .assert_node1_computed_size(2.0, 2.0)
                .assert_node2_computed_size(2.0, 2.0)
                .assert_node3_computed_size(2.0, 2.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(2.0, 0.0)
                .assert_node2_position(0.0, 0.0)
                .assert_node3_position(2.0, 2.0);
        });
    }

    /// ```text
    /// ╔ root ═══════ ▶ ◀ ═════════════╗
    /// ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────╮ ║
    /// ║ │  ╭ node1 ╮  ┆  ╭ node2 ╮  │ ║
    /// ║ │  │       │  ┆  │       │  ▼ ║
    /// ║ │  │       │  ┆  │       │  ▲ ║
    /// ║ │  ╰───────╯  ┆  ╰───────╯  │ ▼
    /// ║ ├╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌┤ ▲
    /// ║ │  ╭ node3 ╮  ┆             │ ║
    /// ║ │  │       │  ┆             ▼ ║
    /// ║ │  │       │  ┆             ▲ ║
    /// ║ │  ╰───────╯  ┆             │ ║
    /// ║ ╰─────────────┴─────────────╯ ║
    /// ╚═══════════════════════════════╝
    /// ```
    #[test]
    fn test_simple_grid_layout_reversed_y() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_column_count(2).reverse_rows();
        test.node1.set_size((2.0, 2.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((2.0, 2.0));
        test.run(|| {
            test.assert_root_computed_size(4.0, 4.0)
                .assert_node1_computed_size(2.0, 2.0)
                .assert_node2_computed_size(2.0, 2.0)
                .assert_node3_computed_size(2.0, 2.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 2.0)
                .assert_node2_position(2.0, 2.0)
                .assert_node3_position(0.0, 0.0);
        });
    }

    /// ```text
    /// ╔ root ════════ ▶ ◀ ═════════════╗
    /// ║ ╭──── ▶ ◀ ─────┬──── ▶ ◀ ────╮ ║
    /// ║ │  ╭ node3 ╮ ╱╱┆╱╱           │ ║
    /// ║ │  │       │ ╱╱┆╱╱           ▼ ║
    /// ║ │  │       │ ╱╱┆╱╱           ▲ ║
    /// ║ │  ╰───────╯ ╱╱┆╱╱           │ ║
    /// ║ │ ╱╱╱╱╱╱╱╱╱╱╱╱╱┆╱╱╱╱╱╱╱╱╱╱╱╱ │ ▼
    /// ║ ├╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌┤ ▲
    /// ║ │ ╱╱╱╱╱╱╱╱╱╱╱╱╱┆╱╱╱╱╱╱╱╱╱╱╱╱ │ ║
    /// ║ │  ╭ node1 ╮ ╱╱┆╱╱ ╭ node2 ╮ │ ║
    /// ║ │  │       │ ╱╱┆╱╱ │       │ ▼ ║
    /// ║ │  │       │ ╱╱┆╱╱ │       │ ▲ ║
    /// ║ │  ╰───────╯ ╱╱┆╱╱ ╰───────╯ │ ║
    /// ║ ╰──────────────┴─────────────╯ ║
    /// ╚════════════════════════════════╝
    /// ```
    #[test]
    fn test_simple_grid_layout_with_gap() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_column_count(2).set_gap((5.0, 3.0));
        test.node1.set_size((2.0, 2.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((2.0, 2.0));
        test.run(|| {
            test.assert_root_computed_size(9.0, 7.0)
                .assert_node1_computed_size(2.0, 2.0)
                .assert_node2_computed_size(2.0, 2.0)
                .assert_node3_computed_size(2.0, 2.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(7.0, 0.0)
                .assert_node3_position(0.0, 5.0);
        });
    }

    /// ```text
    /// ╔ root ═════════════════════════╗
    /// ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────╮ ║
    /// ║ │             ┆  ╭ node2 ╮  │ ║
    /// ║ │  ╭ node1 ╮  ┆  │       │  ▼ ▼
    /// ║ │  │  30%  │  ┆  │  70%  │  ▲ ▲
    /// ║ │  ╰───────╯  ┆  ╰───────╯  │ ║
    /// ║ ╰─────────────┴─────────────╯ ║
    /// ╚═══════════════════════════════╝
    ///                10
    /// ```
    #[test]
    fn test_horizontal_layout_with_percentage_children() {
        let test = TestFlatChildren2::new();
        test.root.use_auto_layout().set_size_x(10.0);
        test.node1.set_size((30.pc(), 1.0));
        test.node2.set_size((70.pc(), 2.0));
        test.run(|| {
            test.assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(3.0, 0.0)
                .assert_root_computed_size(10.0, 2.0)
                .assert_node1_computed_size(3.0, 1.0)
                .assert_node2_computed_size(7.0, 2.0);
        });
        test.root.set_size_x(20.0);
        test.run(|| {
            test.assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(6.0, 0.0)
                .assert_root_computed_size(20.0, 2.0)
                .assert_node1_computed_size(6.0, 1.0)
                .assert_node2_computed_size(14.0, 2.0);
        });
    }

    /// ```text
    /// ╔ root ═══════════════════════════════════════╗
    /// ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────┬──── ▶ ◀ ────╮ ║
    /// ║ │             ┆             ┆  ╭ node3 ╮  │ ║
    /// ║ │             ┆  ╭ node2 ╮  ┆  │       │  │ ║
    /// ║ │  ╭ node1 ╮  ┆  │       │  ┆  │       │  ▼ ▼
    /// ║ │  │       │  ┆  │       │  ┆  │       │  ▲ ▲
    /// ║ │  │  1fr  │  ┆  │  40%  │  ┆  │  2fr  │  │ ║
    /// ║ │  ╰───────╯  ┆  ╰───────╯  ┆  ╰───────╯  │ ║
    /// ║ ╰─────────────┴─────────────┴─────────────╯ ║
    /// ╚═════════════════════════════════════════════╝
    ///                       10
    /// ```
    #[test]
    fn test_horizontal_layout_with_fraction_and_percentage_children() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_size_x(10.0);
        test.node1.set_size((1.fr(), 1.0));
        test.node2.set_size((40.pc(), 2.0));
        test.node3.set_size((2.fr(), 3.0));
        test.run(|| {
            test.assert_root_computed_size(10.0, 3.0)
                .assert_node1_computed_size(2.0, 1.0)
                .assert_node2_computed_size(4.0, 2.0)
                .assert_node3_computed_size(4.0, 3.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(2.0, 0.0)
                .assert_node3_position(6.0, 0.0);
        });
        test.root.set_size_x(20.0);
        test.run(|| {
            test.assert_root_computed_size(20.0, 3.0)
                .assert_node1_computed_size(4.0, 1.0)
                .assert_node2_computed_size(8.0, 2.0)
                .assert_node3_computed_size(8.0, 3.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(4.0, 0.0)
                .assert_node3_position(12.0, 0.0);
        });
    }

    /// ```text
    /// ```text
    /// ╔ root ══════════════ ▶ ◀ ════════════════════╗
    /// ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────┬──── ▶ ◀ ────╮ ║
    /// ║ │             ┆             ┆  ╭ node3 ╮  │ ║
    /// ║ │             ┆  ╭ node2 ╮  ┆  │       │  │ ║
    /// ║ │  ╭ node1 ╮  ┆  │       │  ┆  │       │  ▼ ▼
    /// ║ │  │       │  ┆  │       │  ┆  │       │  ▲ ▲
    /// ║ │  │  1fr  │  ┆  │  40%  │  ┆  │  2fr  │  │ ║
    /// ║ │  ╰───────╯  ┆  ╰───────╯  ┆  ╰───────╯  │ ║
    /// ║ ╰─────────────┴─────────────┴─────────────╯ ║
    /// ╚═════════════════════════════════════════════╝
    ///                       10
    /// ```
    #[test]
    fn test_horizontal_hug_layout_with_fraction_and_percentage_children() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.node1.set_size((1.fr(), 1.0));
        test.node2.set_size((40.pc(), 2.0));
        test.node3.set_size((2.fr(), 3.0));
        test.run(|| {
            test.assert_root_computed_size(0.0, 3.0)
                .assert_node1_computed_size(0.0, 1.0)
                .assert_node2_computed_size(0.0, 2.0)
                .assert_node3_computed_size(0.0, 3.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(0.0, 0.0)
                .assert_node3_position(0.0, 0.0);
        });
    }

    /// ```text
    /// ╔ root ═══════════════════════════════════════╗
    /// ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────┬──── ▶ ◀ ────╮ ║
    /// ║ │             ┆             ┆  ╭ node3 ╮  │ ║
    /// ║ │             ┆  ╭ node2 ╮  ┆  │       │  │ ║
    /// ║ │  ╭ node1 ╮  ┆  │       │  ┆  │       │  ▼ ▼
    /// ║ │  │       │  ┆  │       │  ┆  │       │  ▲ ▲
    /// ║ │  │       │  ┆  │       │  ┆  │       │  │ ║
    /// ║ │  ╰───────╯  ┆  ╰───────╯  ┆  ╰───────╯  │ ║
    /// ║ ╰─────────────┴─────────────┴─────────────╯ ║
    /// ╚═════════════════════════════════════════════╝
    ///         1fr           2fr           3fr
    /// ```
    #[test]
    fn test_fractional_column_layout() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_size_x(12.0);
        test.root.first_column().set_size(1.fr());
        test.root.add_column().set_size(2.fr());
        test.root.add_column().set_size(3.fr());
        test.node1.set_size((1.0, 1.0));
        test.node2.set_size((1.0, 2.0));
        test.node3.set_size((1.0, 3.0));
        test.run(|| {
            test.assert_root_computed_size(12.0, 3.0)
                .assert_node1_computed_size(1.0, 1.0)
                .assert_node2_computed_size(1.0, 2.0)
                .assert_node3_computed_size(1.0, 3.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(2.0, 0.0)
                .assert_node3_position(6.0, 0.0);
        });
    }

    /// ```text
    /// ╔ root ═══════════════════════════════════════╗
    /// ║ ╭──── ▶ ◀ ────┬──── ▶ ◀ ────┬──── ▶ ◀ ────╮ ║
    /// ║ │             ┆             ┆  ╭──────────┼─║────╮
    /// ║ │             ┆  ╭──────────┼──┼─╮ node3  │ ║    │
    /// ║ │  ╭ node1 ╮  ┆  │ node2    ┆  │ │        ▼ ▼    │
    /// ║ │  │       │  ┆  │          ┆  │ │        ▲ ▲    │
    /// ║ │  │       │  ┆  │          ┆  │ │        │ ║    │
    /// ║ │  ╰───────╯  ┆  ╰──────────┼──┴─╯────────┼─║────╯
    /// ║ ╰─────────────┴─────────────┴─────────────╯ ║
    /// ╚═════════════════════════════════════════════╝
    ///         2.0           2.0           2.0
    /// ```
    #[test]
    fn test_fixed_column_layout() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.root.first_column().set_size(2.0);
        test.root.add_column().set_size(2.0);
        test.root.add_column().set_size(2.0);
        test.node1.set_size((1.0, 1.0));
        test.node2.set_size((3.0, 2.0));
        test.node3.set_size((4.0, 3.0));
        test.run(|| {
            test.assert_root_computed_size(6.0, 3.0)
                .assert_node1_computed_size(1.0, 1.0)
                .assert_node2_computed_size(3.0, 2.0)
                .assert_node3_computed_size(4.0, 3.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(2.0, 0.0)
                .assert_node3_position(4.0, 0.0);
        });
    }


    /// ```text
    /// ╔ root ═══════════════════════════════════════════════════╗
    /// ║ ╭─────────────────┬─────────────────┬─────────────────╮ ║
    /// ║ │╱╱             ╱╱┆╱╱             ╱╱┆╱╱  ╭ node3 ╮  ╱╱│ ║
    /// ║ │╱╱             ╱╱┆╱╱  ╭ node2 ╮  ╱╱┆╱╱  │       │  ╱╱│ ║
    /// ║ │╱╱  ╭ node1 ╮  ╱╱┆╱╱  │       │  ╱╱┆╱╱  │       │  ╱╱▼ ▼
    /// ║ │╱╱  │       │  ╱╱┆╱╱  │       │  ╱╱┆╱╱  │       │  ╱╱▲ ▲
    /// ║ │╱╱  │   2   │  ╱╱┆╱╱  │   2   │  ╱╱┆╱╱  │   2   │  ╱╱│ ║
    /// ║ │.5fr╰───────╯   1fr   ╰───────╯   1fr   ╰───────╯.5fr│ ║
    /// ║ ╰─────────────────┴─────────────────┴─────────────────╯ ║
    /// ╚═════════════════════════════════════════════════════════╝
    ///                              12
    /// ```
    #[test]
    fn test_fixed_column_layout_with_fraction_gap() {
        let test = TestFlatChildren3::new();
        test.root
            .use_auto_layout()
            .set_size_x(12.0)
            .set_padding_left(0.5.fr())
            .set_padding_right(0.5.fr())
            .set_gap_x(1.fr());
        test.node1.set_size((2.0, 1.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((2.0, 3.0));
        test.run(|| {
            test.assert_root_position(0.0, 0.0)
                .assert_node1_position(1.0, 0.0)
                .assert_node2_position(5.0, 0.0)
                .assert_node3_position(9.0, 0.0)
                .assert_root_computed_size(12.0, 3.0)
                .assert_node1_computed_size(2.0, 1.0)
                .assert_node2_computed_size(2.0, 2.0)
                .assert_node3_computed_size(2.0, 3.0);
        });
    }

    /// ```text
    /// ╔ root ═══════════════════════════════════════════════════╗
    /// ║ ╭─────────────────┬─────────────────┬─────────────────╮ ║
    /// ║ │╱╱             ╱╱┆╱╱             ╱╱┆╱╱  ╭ node3 ┬▷ ╱╱│ ║
    /// ║ │╱╱             ╱╱┆╱╱  ╭ node2 ┬▷ ╱╱┆╱╱  │       │  ╱╱│ ║
    /// ║ │╱╱  ╭ node1 ┬▷ ╱╱┆╱╱  │       │  ╱╱┆╱╱  │       │  ╱╱▼ ▼
    /// ║ │╱╱  │       │  ╱╱┆╱╱  │       │  ╱╱┆╱╱  │       │  ╱╱▲ ▲
    /// ║ │╱╱  │   0   │  ╱╱┆╱╱  │   0   │  ╱╱┆╱╱  │   0   │  ╱╱│ ║
    /// ║ │.5fr╰───────╯   1fr   ╰───────╯   1fr   ╰───────╯.5fr│ ║
    /// ║ ╰─────────────────┴─────────────────┴─────────────────╯ ║
    /// ╚═════════════════════════════════════════════════════════╝
    ///                             12
    /// ```
    #[test]
    fn test_fixed_column_layout_with_fraction_gap_and_children_tat_grow() {
        let test = TestFlatChildren3::new();
        test.root
            .use_auto_layout()
            .set_size_x(12.0)
            .set_padding_left(0.5.fr())
            .set_padding_right(0.5.fr())
            .set_gap_x(1.fr());
        test.node1.set_size((0.0, 1.0)).allow_grow_x();
        test.node2.set_size((0.0, 2.0)).allow_grow_x();
        test.node3.set_size((0.0, 3.0)).allow_grow_x();
        test.run(|| {
            test.assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(4.0, 0.0)
                .assert_node3_position(8.0, 0.0)
                .assert_root_computed_size(12.0, 3.0)
                .assert_node1_computed_size(4.0, 1.0)
                .assert_node2_computed_size(4.0, 2.0)
                .assert_node3_computed_size(4.0, 3.0);
        });
    }

    /// ```text
    /// ╔ root ═══════════════════════════════════════════════════════════════╗
    /// ║ ╭─────────────────────┬─────────────────────┬─────────────────────╮ ║
    /// ║ │ ╱╱╱             ╱╱╱ ┆ ╱╱╱             ╱╱╱ ┆ ╱╱╱  ╭ node3 ╮  ╱╱╱ │ ║
    /// ║ │ ╱╱╱             ╱╱╱ ┆ ╱╱╱  ╭ node2 ╮  ╱╱╱ ┆ ╱╱╱  │       │  ╱╱╱ │ ║
    /// ║ │ ╱╱╱  ╭ node1 ╮  ╱╱╱ ┆ ╱╱╱  │       │  ╱╱╱ ┆ ╱╱╱  │       │  ╱╱╱ ▼ ║
    /// ║ │ ╱╱╱  │       │  ╱╱╱ ┆ ╱╱╱  │       │  ╱╱╱ ┆ ╱╱╱  │       │  ╱╱╱ ▲ ║
    /// ║ │ ╱╱╱  │   2   │  ╱╱╱ ┆ ╱╱╱  │   2   │  ╱╱╱ ┆ ╱╱╱  │   2   │  ╱╱╱ │ ║
    /// ║ │ 1fr  ╰───────╯  2fr ┆ 3fr  ╰───────╯  4fr ┆ 5fr  ╰───────╯  6fr │ ║
    /// ║ ╰─────────────────────┴─────────────────────┴─────────────────────╯ ║
    /// ╚═════════════════════════════════════════════════════════════════════╝
    ///              5                     9                    13
    /// ```
    #[test]
    fn test_fixed_column_layout_with_fraction_margin() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_size_x(27.0);
        test.root.first_column().set_size(5.0);
        test.root.add_column().set_size(9.0);
        test.root.add_column().set_size(13.0);
        test.node1.set_size((2.0, 1.0)).set_margin_left(1.fr()).set_margin_right(2.fr());
        test.node2.set_size((2.0, 2.0)).set_margin_left(3.fr()).set_margin_right(4.fr());
        test.node3.set_size((2.0, 3.0)).set_margin_left(5.fr()).set_margin_right(6.fr());
        test.run(|| {
            test.assert_root_position(0.0, 0.0)
                .assert_node1_position(1.0, 0.0)
                .assert_node2_position(8.0, 0.0)
                .assert_node3_position(19.0, 0.0)
                .assert_root_computed_size(27.0, 3.0)
                .assert_node1_computed_size(2.0, 1.0)
                .assert_node2_computed_size(2.0, 2.0)
                .assert_node3_computed_size(2.0, 3.0);
        });
    }

    /// ```text
    ///     ╭─────────────────────╮
    ///     │ root            ╭───┼────╮
    /// ╭───┼────╮            │ node3  │
    /// │ node1  │            │   │    │
    /// │   │    │ ╭────────╮ ╰───┼────╯
    /// ╰───┼────╯ │ node2  │     │
    ///     ╰──────┼────────┼─────╯
    ///            │        │
    ///            ╰────────╯
    /// ```
    #[test]
    fn test_layout_manual_fixed() {
        let test = TestFlatChildren3::new();
        test.root.set_size((3.0, 2.0));
        test.node1.set_size((2.0, 2.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((2.0, 2.0));
        test.node1.set_xy((-1.0, 0.0));
        test.node2.set_xy((1.0, -1.0));
        test.node3.set_xy((3.0, 1.0));
        test.run(|| {
            test.assert_root_computed_size(3.0, 2.0)
                .assert_node1_computed_size(2.0, 2.0)
                .assert_node2_computed_size(2.0, 2.0)
                .assert_node3_computed_size(2.0, 2.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(-1.0, 0.0)
                .assert_node2_position(1.0, -1.0)
                .assert_node3_position(3.0, 1.0);
        });
    }

    /// ```text
    ///     ╭──────── ▶ ◀ ──────────────╮
    ///     │ root            ╭────────╮│
    /// ╭───┼────╮            │ node3  ││
    /// │ node1  │            │        │▼
    /// │   │    │ ╭────────╮ ╰────────╯▲
    /// ╰───┼────╯ │ node2  │           │
    ///     ╰──────┼────────┼───────────╯
    ///            │        │
    ///            ╰────────╯
    /// ```
    #[test]
    fn test_layout_manual_hug() {
        let test = TestFlatChildren3::new();
        test.node1.set_size((2.0, 2.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((2.0, 2.0));
        test.node1.set_xy((-1.0, 0.0));
        test.node2.set_xy((1.0, -1.0));
        test.node3.set_xy((3.0, 1.0));
        test.run(|| {
            test.assert_root_computed_size(5.0, 3.0)
                .assert_node1_computed_size(2.0, 2.0)
                .assert_node2_computed_size(2.0, 2.0)
                .assert_node3_computed_size(2.0, 2.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(-1.0, 0.0)
                .assert_node2_position(1.0, -1.0)
                .assert_node3_position(3.0, 1.0);
        });
    }

    /// ```text
    ///    ╔══════════════════════ ▶ ◀ ══════════════════════╗
    ///    ║ ╭ root ────────────────┬──────────────────────╮ ║
    ///    ║ │╭ node1 ─ ▶ ◀ ───────╮┆╭ node2 ─ ▶ ◀ ───────╮│ ║
    ///   ╭╫─┼┼──────╮           ╭─┼┼┼──────╮             ││ ║
    ///   │║node1_1  │           │ node2_1  │             ││ ▼
    ///   │║ ││      │  ╭────────┼╮│┆│      │  ╭─────────╮││ ▲
    ///   ╰╫─┼┼──────╯  │ node1_2╰┼┼┼┼──────╯  │ node2_2 │││ ║
    ///    ║ │╰─────────┼─────────┼╯┆╰─────────┼─────────┼╯│ ║
    ///    ║ ╰──────────┼─────────┼─┴──────────┼─────────┼─╯ ║
    ///    ╚════════════╰─────────╯════════════╰─────────╯═══╝
    /// ```
    #[test]
    fn test_layout_with_children_with_overflow() {
        let test = TestFlatChildren2::new();

        test.root.use_auto_layout();

        let node1_1 = test.node1.new_child_named("node1_1");
        let node1_2 = test.node1.new_child_named("node1_2");
        node1_1.set_size((2.0, 2.0));
        node1_2.set_size((2.0, 2.0));
        node1_1.set_xy((-1.0, 0.0));
        node1_2.set_xy((1.0, -1.0));

        let node2_1 = test.node2.new_child_named("node2_1");
        let node2_2 = test.node2.new_child_named("node2_2");
        node2_1.set_size((2.0, 2.0));
        node2_2.set_size((2.0, 2.0));
        node2_1.set_xy((-1.0, 0.0));
        node2_2.set_xy((1.0, -1.0));

        test.run(|| {
            assert_eq!(node1_1.position().xy(), Vector2(-1.0, 0.0));
            assert_eq!(node1_2.position().xy(), Vector2(1.0, -1.0));
            assert_eq!(node1_1.computed_size(), Vector2(2.0, 2.0));
            assert_eq!(node1_2.computed_size(), Vector2(2.0, 2.0));
            assert_eq!(test.node1.computed_size(), Vector2(3.0, 2.0));

            assert_eq!(node2_1.position().xy(), Vector2(-1.0, 0.0));
            assert_eq!(node2_2.position().xy(), Vector2(1.0, -1.0));
            assert_eq!(node2_1.computed_size(), Vector2(2.0, 2.0));
            assert_eq!(node2_2.computed_size(), Vector2(2.0, 2.0));
            assert_eq!(test.node2.computed_size(), Vector2(3.0, 2.0));

            test.assert_node1_position(0.0, 0.0)
                .assert_node2_position(3.0, 0.0)
                .assert_node1_computed_size(3.0, 2.0)
                .assert_node2_computed_size(3.0, 2.0)
                .assert_root_computed_size(6.0, 2.0)
                .assert_root_position(0.0, 0.0);
        });
    }

    /// ```text
    /// ╭───────────────────╮
    /// │ root              │
    /// │    ╭─────────╮    │
    /// │    │ node1   │    │
    /// │    │         │    │
    /// │    │         │    │
    /// │    ╰─────────╯    │
    /// │                   │
    /// ╰───────────────────╯
    /// ```
    #[test]
    fn test_layout_fraction_padding_all() {
        let test = TestFlatChildren1::new();
        test.root.use_auto_layout().set_size((10.0, 10.0)).set_padding_all(1.fr());
        test.node1.set_size((2.0, 2.0));
        test.run(|| {
            test.assert_root_computed_size(10.0, 10.0)
                .assert_node1_computed_size(2.0, 2.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(4.0, 4.0);
        });
    }

    /// ```text
    /// ╭───────────────────╮
    /// │ root              │
    /// │    ╭─────────╮    │
    /// │    │ node1   │    │
    /// │    │         │    │
    /// │    │         │    │
    /// │    ╰─────────╯    │
    /// │                   │
    /// ╰───────────────────╯
    /// ```
    #[test]
    fn test_manual_layout_alignment_center() {
        let test = TestFlatChildren1::new();
        test.root.set_size((10.0, 10.0));
        test.node1.set_size((5.0, 5.0));
        test.node1.set_alignment_center();
        test.run(|| {
            test.assert_root_computed_size(10.0, 10.0)
                .assert_node1_computed_size(5.0, 5.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(2.5, 2.5);
        });
    }

    #[test]
    fn test_manual_layout_margin_alignment() {
        let test = TestFlatChildren3::new();
        test.root.set_size((10.0, 10.0));
        test.node1.allow_grow().set_margin_trbl(1.0, 2.0, 3.0, 4.0).set_alignment_left_bottom();
        test.node2
            .set_size((3.0, 3.0))
            .set_margin_trbl(1.0, 2.0, 3.0, 4.0)
            .set_alignment_left_center();
        test.node3
            .set_size((3.0, 3.0))
            .set_margin_trbl(1.0, 2.0, 3.0, 4.0)
            .set_alignment_right_bottom();

        test.run(|| {
            test.assert_root_computed_size(10.0, 10.0)
                .assert_node1_computed_size(4.0, 6.0)
                .assert_node2_computed_size(3.0, 3.0)
                .assert_node3_computed_size(3.0, 3.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(4.0, 3.0)
                .assert_node2_position(4.0, 4.5)
                .assert_node3_position(5.0, 3.0);
        });
    }

    /// ```text
    ///        ╭─root─────────────╮
    ///        │╭─node1──╮        │
    ///        ││        │        │
    ///        ││        │        │
    /// ╭─node2┼┼────────┼───────╮│
    /// │      ││        │       ││
    /// │      ││        │       ││
    /// ╰──────┼┼────────┼───────╯│
    ///        ││        │        │
    ///        ││        │        │
    ///        │╰────────╯        │
    ///        ╰──────────────────╯
    /// ```
    #[test]
    fn test_manual_layout_alignment_center_hug() {
        let test = TestFlatChildren2::new();
        test.node1.set_size((5.0, 10.0));
        test.node2.set_size((10.0, 5.0));
        test.node2.set_alignment_center();
        test.run(|| {
            test.assert_root_computed_size(7.5, 10.0)
                .assert_node1_computed_size(5.0, 10.0)
                .assert_node2_computed_size(10.0, 5.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(-2.5, 2.5);
        });
    }

    /// ```text
    /// ╭ root ───────────╮
    /// │╭ node1 ───◀ ▶──╮│
    /// ││               ▲│
    /// ││               ▼│
    /// ││               ││
    /// │╰───────────────╯│
    /// ╰─────────────────╯
    /// ```
    #[test]
    fn test_manual_layout_with_child_with_grow() {
        let test = TestFlatChildren1::new();
        test.root.set_size((10.0, 10.0));
        test.node1.allow_grow();
        test.run(|| {
            test.assert_root_computed_size(10.0, 10.0)
                .assert_node1_computed_size(10.0, 10.0)
                .assert_root_position(0.0, 0.0)
                .assert_node1_position(0.0, 0.0);
        });
    }

    #[test]
    fn test_automatic_layout_update_after_removing_and_adding_children() {
        let test = TestFlatChildren2::new();
        test.root.use_auto_layout();
        test.node1.set_size((10.0, 10.0));
        test.node2.set_size((10.0, 10.0));

        test.run(|| {
            test.assert_root_computed_size(20.0, 10.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(10.0, 0.0);
        });

        test.root.remove_child(&test.node1);
        test.run(|| {
            test.assert_root_computed_size(10.0, 10.0).assert_node2_position(0.0, 0.0);
        });

        test.root.add_child(&test.node1);
        test.run(|| {
            test.assert_root_computed_size(20.0, 10.0)
                .assert_node1_position(10.0, 0.0)
                .assert_node2_position(0.0, 0.0);
        });

        let node3 = Instance::new();
        node3.set_size((12.0, 10.0));
        test.root.add_child(&node3);
        test.run(|| {
            test.assert_root_computed_size(32.0, 10.0)
                .assert_node1_position(10.0, 0.0)
                .assert_node2_position(0.0, 0.0);
        });
        assert_eq!(node3.position().xy(), Vector2(20.0, 0.0));
    }

    #[test]
    fn test_automatic_layout_update_after_resizing_children() {
        let test = TestFlatChildren2::new();
        test.root.use_auto_layout();
        test.node1.set_size((10.0, 10.0));
        test.node2.set_size((10.0, 10.0));

        test.run(|| {
            test.assert_root_computed_size(20.0, 10.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(10.0, 0.0);
        });

        test.node1.set_size((20.0, 10.0));
        test.node2.set_size((10.0, 20.0));
        test.run(|| {
            test.assert_root_computed_size(30.0, 20.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(20.0, 0.0);
        });
    }

    #[test]
    fn test_auto_layout_nested_grow_update() {
        let test = TestFlatChildren2::new();
        test.node1.use_auto_layout().allow_grow();
        let inner = test.node1.new_child_named("inner");
        inner.allow_grow();

        test.node2.set_size((30.0, 10.0));
        test.run(|| {
            test.assert_root_computed_size(30.0, 10.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(0.0, 0.0)
                .assert_node1_computed_size(30.0, 10.0)
                .assert_node2_computed_size(30.0, 10.0);
            assert_eq!(inner.position().xy(), Vector2(0.0, 0.0));
            assert_eq!(inner.computed_size(), Vector2(30.0, 10.0));
        });

        test.node2.set_size((25.0, 20.0));
        test.run(|| {
            test.assert_root_computed_size(25.0, 20.0)
                .assert_node1_position(0.0, 0.0)
                .assert_node2_position(0.0, 0.0)
                .assert_node1_computed_size(25.0, 20.0)
                .assert_node2_computed_size(25.0, 20.0);
            assert_eq!(inner.position().xy(), Vector2(0.0, 0.0));
            assert_eq!(inner.computed_size(), Vector2(25.0, 20.0));
        });
    }

    #[test]
    fn test_size_hug_double_update() {
        let world = World::new();
        let root = Instance::new_named("Root");
        let child = root.new_child();
        child.set_size((10.0, 10.0));
        root.update(&world.default_scene);
        assert_eq!(root.computed_size(), Vector2(10.0, 10.0));
        root.update(&world.default_scene);
        assert_eq!(root.computed_size(), Vector2(10.0, 10.0));
    }
}
