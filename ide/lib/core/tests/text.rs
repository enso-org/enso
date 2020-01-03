//! Test suite for the Web and headless browsers.
#![cfg(target_arch = "wasm32")]

use basegl::display::world::Add;
use basegl::display::world::Workspace;
use basegl::display::world::WorkspaceID;
use basegl::display::world::World;
use basegl::display::world::WorldRef;
use basegl_system_web::create_element;
use basegl_system_web::dyn_into;
use basegl_system_web::Error;
use basegl_system_web::get_element_by_id;
use basegl_system_web::get_webgl2_context;
use basegl_system_web::NodeInserter;
use basegl_system_web::Result;
use basegl_system_web::StyleSetter;
use web_sys::HtmlElement;
use web_sys::HtmlCanvasElement;


// ==================
// === World test ===
// ==================

/// A little framework for doing web tests with worlds
///
/// This should be a temporary solution - until world and htmlscene frameworks will be merged.
pub struct WorldTest {
    pub world_ptr    : WorldRef,
    pub workspace_id : WorkspaceID,
}

impl WorldTest {
    /// Set up new test with World
    ///
    /// This creates `canvas` element and add workspace operating on this canvas. Returns `None`
    /// if webgl context is unavailable (likely because test are run in headless browser - whe
    /// should not fail in that case)
    pub fn new(test_name:&str) -> Option<WorldTest> {
        let workspace_set_up = Self::setup_workspace_canvas(test_name);
        match workspace_set_up {
            Ok(())                         => Some(Self::create_world_with_workspace(test_name)),
            Err(Error::NoWebGL{version:_}) => None,
            other_error                    => {other_error.unwrap(); None}
        }
    }

    fn setup_workspace_canvas(test_name:&str) -> Result<()> {
        let root                               = get_element_by_id(test_name)?;
        let root_html      : HtmlElement       = dyn_into(root.clone())?;
        let canvas_element                     = create_element("canvas")?;
        let canvas_html    : HtmlElement       = dyn_into(canvas_element.clone())?;
        let canvas         : HtmlCanvasElement = dyn_into(canvas_element.clone())?;

        get_webgl2_context(&canvas)?;
        canvas.set_width(640);
        canvas.set_height(640);
        canvas_html.set_id(Self::workspace_name(test_name).as_str());
        root_html.set_property_or_panic("overflow", "scroll");
        root.append_or_panic(&canvas_html);
        Ok(())
    }

    fn create_world_with_workspace(test_name:&str) -> WorldTest {
        let world_ptr    = World::new();
        let workspace    = Workspace::build(Self::workspace_name(test_name));
        let workspace_id = world_ptr.borrow_mut().add(workspace);
        WorldTest {world_ptr,workspace_id}
    }

    fn workspace_name(test_name:&str) -> String {
        "workspace_".to_owned() + test_name
    }
}


#[cfg(test)]
mod tests {
    use web_test::*;
    use basegl::data::dirty::traits::*;

    use super::WorldTest;
    use basegl::Color;
    use basegl::display::world::World;
    use basegl::display::shape::text::content::TextChange;
    use basegl::display::shape::text::content::CharPosition;
    use basegl::display::shape::text::TextComponentBuilder;
    use basegl::display::shape::text::TextComponentProperties;

    use basegl_core_msdf_sys::run_once_initialized;
    use nalgebra::Point2;
    use nalgebra::Vector2;

    web_configure!(run_in_browser);

    const TEST_TEXT : &str = "To be, or not to be, that is the question:\n\
        Whether 'tis nobler in the mind to suffer\n\
        The slings and arrows of outrageous fortune,\n\
        Or to take arms against a sea of troubles\n\
        And by opposing end them.";
    const LONG_TEXT : &str    = include_str!(concat!(env!("OUT_DIR"), "/long.txt"));
    const WIDE_TEXT : &str    = include_str!(concat!(env!("OUT_DIR"), "/wide.txt"));
    const FONTS     : &[&str] = &
        [ "DejaVuSans"
        , "DejaVuSansMono"
        , "DejaVuSansMono-Bold"
        , "DejaVuSerif"
        ];

    #[web_test]
    fn small_font() {
        if let Some(world_test) = WorldTest::new("small_font") {
            run_once_initialized(move || {
                let text         = TEST_TEXT.to_string();
                let size         = 0.025;
                create_test_components_for_each_font(&world_test,text,size);
            });
        }
    }

    #[web_test]
    fn normal_font() {
        if let Some(world_test) = WorldTest::new("normal_font") {
            run_once_initialized(move || {
                let text         = TEST_TEXT.to_string();
                let size         = 0.0375;
                create_test_components_for_each_font(&world_test,text,size);
            });
        }
    }

    #[web_test]
    fn big_font() {
        if let Some(world_test) = WorldTest::new("big_font") {
            run_once_initialized(move || {
                let text         = TEST_TEXT.to_string();
                let size         = 0.125;
                create_test_components_for_each_font(&world_test,text,size);
            });
        }
    }

    #[web_bench]
    fn scrolling_vertical_30(bencher:&mut Bencher) {
        if let Some(world_test) = WorldTest::new("scrolling_vertical_30") {
            let mut bencher_clone = bencher.clone();
            run_once_initialized(move || {
                create_full_sized_text_component(&world_test,LONG_TEXT.to_string());
                bencher_clone.iter(move || {
                    let world : &mut World = &mut world_test.world_ptr.borrow_mut();
                    for _ in 0..30 { //TODO[AO] make target FPS feature in web_bench
                        let workspace      = &mut world.workspaces[world_test.workspace_id];
                        let text_component = &mut workspace.text_components[0];
                        text_component.scroll(Vector2::new(0.0,-1.0));
                        world.workspace_dirty.set(world_test.workspace_id);
                        world.update();
                    }
                });
            });
        }
    }

    #[web_bench]
    fn scrolling_horizontal_20(bencher:&mut Bencher) {
        if let Some(world_test) = WorldTest::new("scrolling_horizontal_20") {
            let mut bencher_clone = bencher.clone();
            run_once_initialized(move || {
                create_full_sized_text_component(&world_test,WIDE_TEXT.to_string());
                bencher_clone.iter(move || {
                    let world : &mut World = &mut world_test.world_ptr.borrow_mut();
                    for _ in 0..20 { //TODO[AO] make target FPS feature in web_bench
                        let workspace      = &mut world.workspaces[world_test.workspace_id];
                        let text_component = &mut workspace.text_components[0];
                        text_component.scroll(Vector2::new(1.0,0.0));
                        world.workspace_dirty.set(world_test.workspace_id);
                        world.update();
                    }
                });
            });
        }
    }

    #[web_bench]
    fn editing_single_long_line_15(bencher:&mut Bencher) {
        if let Some(world_test) = WorldTest::new("editing_single_long_line_15") {
            let mut bencher_clone = bencher.clone();
            run_once_initialized(move || {
                create_full_sized_text_component(&world_test,WIDE_TEXT.to_string());
                bencher_clone.iter(move || {
                    let world : &mut World = &mut world_test.world_ptr.borrow_mut();
                    for _ in 0..15 {
                        let workspace      = &mut world.workspaces[world_test.workspace_id];
                        let text_component = &mut workspace.text_components[0];
                        let replace_from   = CharPosition{line:1, column:2};
                        let replace_to     = CharPosition{line:1, column:3};
                        let replaced_range = replace_from..replace_to;
                        let change         = TextChange::replace(replaced_range, "abc");
                        text_component.content.make_change(change);
                        world.workspace_dirty.set(world_test.workspace_id);
                        world.update();
                    }
                });
            });
        }
    }

    #[web_bench]
    fn inserting_many_lines_in_long_file(bencher:&mut Bencher) {
        if let Some(world_test) = WorldTest::new("inserting_many_lines_in_long_file") {
            let mut bencher_clone = bencher.clone();
            run_once_initialized(move || {
                create_full_sized_text_component(&world_test,LONG_TEXT.to_string());
                bencher_clone.iter(move || {
                    let world : &mut World = &mut world_test.world_ptr.borrow_mut();
                    let workspace      = &mut world.workspaces[world_test.workspace_id];
                    let text_component = &mut workspace.text_components[0];
                    let position       = CharPosition{line:1, column:0};
                    let change         = TextChange::insert(position, TEST_TEXT);
                    text_component.content.make_change(change);
                    world.workspace_dirty.set(world_test.workspace_id);
                    world.update();
                });
            });
        }
    }

    fn create_full_sized_text_component(world_test:&WorldTest, text:String) {
        let workspace_id       = world_test.workspace_id;
        let world : &mut World = &mut world_test.world_ptr.borrow_mut();
        let workspace          = &mut world.workspaces[workspace_id];
        let fonts              = &mut world.fonts;
        let font_name          = FONTS[1];
        let font_id            = fonts.load_embedded_font(font_name).unwrap();

        let properties = TextComponentProperties {
            position  : Point2::new(-1.0, -1.0),
            size      : Vector2::new(2.0, 2.0),
            text_size : 0.03125,
            color     : Color {r: 1.0, g: 1.0, b: 1.0, a: 1.0},
        };
        let builder        = TextComponentBuilder {workspace,fonts,text,font_id,properties};
        let text_component = builder.build();
        workspace.text_components.push(text_component);
        world.workspace_dirty.set(workspace_id); // TODO[AO] Make dirty flags for component
    }

    fn create_test_components_for_each_font(world_test:&WorldTest, text:String, text_size:f64) {
        let workspace_id       = world_test.workspace_id;
        let world : &mut World = &mut world_test.world_ptr.borrow_mut();
        let workspace          = &mut world.workspaces[workspace_id];
        let fonts              = &mut world.fonts;

        for (i, font_name) in FONTS.iter().enumerate() {
            let x         = -1.0 + (i / 2) as f64;
            let y         = -1.0 + (i % 2) as f64;
            let font_id   = fonts.load_embedded_font(font_name).unwrap();

            let properties = TextComponentProperties {text_size,
                position  : Point2::new(x,y),
                size      : Vector2::new(1.0,1.0),
                color     : Color{r:1.0, g:1.0, b:1.0, a:1.0}
            };
            let builder            = TextComponentBuilder{workspace,fonts,font_id,properties,
                text: text.clone()
            };
            let mut text_component = builder.build();
            let cursor_position_1  = CharPosition{line:0, column: 10};
            let cursor_position_2  = CharPosition{line:1, column: 6};
            text_component.cursors.add_cursor(cursor_position_1);
            text_component.cursors.add_cursor(cursor_position_2);
            workspace.text_components.push(text_component);
        }
        world.workspace_dirty.set(workspace_id);
    }
}