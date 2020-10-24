#![allow(missing_docs)]

//! NOTE
//! This file is under a heavy development. It contains commented lines of code and some code may
//! be of poor quality. Expect drastic changes.

use crate::prelude::*;

use crate::graph_editor::GraphEditor;
use crate::graph_editor::Type;
use crate::project;

use enso_frp as frp;
use ensogl::display::navigation::navigator::Navigator;
use ensogl::system::web;
use ensogl::application::Application;
use ensogl::display::object::ObjectOps;
use ensogl_text as text;
use ensogl_theme;
use wasm_bindgen::prelude::*;
use parser::Parser;



const STUB_MODULE:&str = "from Base import all\n\nmain = IO.println \"Hello\"\n";


#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_interface() {
    web::forward_panic_hook_to_console();
    web::set_stdout();
    web::set_stack_trace_limit();
    run_once_initialized(|| {
        let app = Application::new(&web::get_html_element_by_id("root").unwrap());
        init(&app);
        mem::forget(app);
    });
}


fn _fence<T,Out>(network:&frp::Network, trigger:T) -> (frp::Stream,frp::Stream<bool>)
where T:frp::HasOutput<Output=Out>, T:Into<frp::Stream<Out>>, Out:frp::Data {
    let trigger = trigger.into();
    frp::extend! { network
        def trigger_ = trigger.constant(());
        def runner   = source::<()>();
        def switch   = any_mut();
        switch.attach(&trigger_);
        def triggered = trigger.map(f_!(runner.emit(())));
        switch.attach(&triggered);
        def condition = switch.toggle_true();
    }
    let runner = runner.into();
    (runner,condition)
}



// ==================
// === Mock Types ===
// ==================

/// Allows the creation of arbitrary unique `Type`s.
#[derive(Clone,Debug,Default)]
struct DummyTypeGenerator {
    type_counter : u32
}

impl DummyTypeGenerator {
    fn get_dummy_type(&mut self) -> Type {
        self.type_counter += 1;
        Type::from(format!("dummy_type_{}",self.type_counter))
    }
}



// ========================
// === Init Application ===
// ========================

fn init(app:&Application) {

    ensogl_theme::dark::setup(&app);
    // ensogl_theme::light::setup(&app);

    let _bg = app.display.scene().style_sheet.var(ensogl_theme::vars::application::background::color);


    let world     = &app.display;
    let scene     = world.scene();
    let camera    = scene.camera();
    let navigator = Navigator::new(&scene,&camera);

    app.views.register::<project::View>();
    app.views.register::<text::Area>();
    app.views.register::<GraphEditor>();
    let project_view = app.new_view::<project::View>();
    let graph_editor = project_view.graph();
    let code_editor  = project_view.code_editor();
    world.add_child(&project_view);

    code_editor.text_area().set_content(STUB_MODULE.to_owned());

    let node1_id = graph_editor.add_node();
    let node2_id = graph_editor.add_node();

    graph_editor.frp.set_node_position.emit((node1_id,Vector2(-150.0,50.0)));
    graph_editor.frp.set_node_position.emit((node2_id,Vector2(50.0,50.0)));

    let mut dummy_type_generator = DummyTypeGenerator::default();
    let expression_1 = expression_mock();
    graph_editor.frp.set_node_expression.emit((node1_id,expression_1.clone()));
    expression_1.input_span_tree.root_ref().leaf_iter().for_each(|node|{
        if let Some(expr_id) = node.ast_id {
            let dummy_type = Some(dummy_type_generator.get_dummy_type());
            graph_editor.frp.set_expression_type.emit((node1_id,expr_id,dummy_type));
        }
    });
    expression_1.output_span_tree.root_ref().leaf_iter().for_each(|node|{
        if let Some(expr_id) = node.ast_id {
            let dummy_type = Some(dummy_type_generator.get_dummy_type());
            graph_editor.frp.set_expression_type.emit((node1_id,expr_id,dummy_type));
        }
    });

    let expression_2 = expression_mock3();
    graph_editor.frp.set_node_expression.emit((node2_id,expression_2.clone()));
    expression_2.input_span_tree.root_ref().leaf_iter().for_each(|node|{
        if let Some(expr_id) = node.ast_id {
            let dummy_type = Some(dummy_type_generator.get_dummy_type());
            graph_editor.frp.set_expression_type.emit((node2_id,expr_id,dummy_type));
        }
    });
    expression_2.output_span_tree.root_ref().leaf_iter().for_each(|node|{
        if let Some(expr_id) = node.ast_id {
            let dummy_type = Some(dummy_type_generator.get_dummy_type());
            graph_editor.frp.set_expression_type.emit((node2_id,expr_id,dummy_type));
        }
    });

    let mut was_rendered = false;
    let mut loader_hidden = false;
    world.on_frame(move |_| {
        let _keep_alive = &navigator;
        let _keep_alive = &project_view;

        // Temporary code removing the web-loader instance.
        // To be changed in the future.
        if was_rendered && !loader_hidden {
            web::get_element_by_id("loader").map(|t| {
                t.parent_node().map(|p| {
                    p.remove_child(&t).unwrap()
                })
            }).ok();
            loader_hidden = true;
        }
        was_rendered = true;
    }).forget();
    visual_expr(expression_mock2());

}


fn visual_expr(expr:Expression) {
    println!("-----------------");
    println!("{:#?}",expr);
}


// =============
// === Mocks ===
// =============

use crate::graph_editor::component::node::port::Expression;

use ast::crumbs::*;
use ast::crumbs::PatternMatchCrumb::*;
use enso_protocol::prelude::Uuid;
use ensogl_text_msdf_sys::run_once_initialized;
use span_tree::traits::*;



pub fn expression_mock() -> Expression {
    let code             = "open \"data.csv\"".into();
    let output_span_tree = span_tree::SpanTree::default();
    let input_span_tree  = span_tree::builder::TreeBuilder::new(15)
        .add_child(0,4,span_tree::node::Kind::Operation,PrefixCrumb::Func)
        .set_ast_id(Uuid::new_v4())
        .done()
        .add_empty_child(5,span_tree::node::InsertionPointType::BeforeTarget)
        .add_child(5,10,span_tree::node::Kind::this(),PrefixCrumb::Arg)
        .set_ast_id(Uuid::new_v4())
        .done()
        .add_empty_child(15,span_tree::node::InsertionPointType::Append)
        .set_ast_id(Uuid::new_v4())
        .build();
    Expression {code,input_span_tree,output_span_tree}
}

pub fn expression_mock2() -> Expression {
    let pattern_cr       = vec![Seq { right: false }, Or, Or, Build];
    let val              = ast::crumbs::SegmentMatchCrumb::Body {val:pattern_cr};
    let parens_cr        = ast::crumbs::MatchCrumb::Segs {val,index:0};
    let code             = "make_maps size (distribution normal)".into();
    let output_span_tree = span_tree::SpanTree::default();
    let input_span_tree  = span_tree::builder::TreeBuilder::new(36)
        .add_child(0,14,span_tree::node::Kind::Chained,PrefixCrumb::Func)
            .add_child(0,9,span_tree::node::Kind::Operation,PrefixCrumb::Func)
                .set_ast_id(Uuid::new_v4())
                .done()
            .add_empty_child(10,span_tree::node::InsertionPointType::BeforeTarget)
            .add_child(10,4,span_tree::node::Kind::this().removable(),PrefixCrumb::Arg)
                .set_ast_id(Uuid::new_v4())
                .done()
            .add_empty_child(14,span_tree::node::InsertionPointType::Append)
            .set_ast_id(Uuid::new_v4())
            .done()
        .add_child(15,21,span_tree::node::Kind::argument().removable(),PrefixCrumb::Arg)
            .set_ast_id(Uuid::new_v4())
            .add_child(1,19,span_tree::node::Kind::argument(),parens_cr)
                .set_ast_id(Uuid::new_v4())
                .add_child(0,12,span_tree::node::Kind::Operation,PrefixCrumb::Func)
                    .set_ast_id(Uuid::new_v4())
                    .done()
                .add_empty_child(13,span_tree::node::InsertionPointType::BeforeTarget)
                .add_child(13,6,span_tree::node::Kind::this(),PrefixCrumb::Arg)
                    .set_ast_id(Uuid::new_v4())
                    .done()
                .add_empty_child(19,span_tree::node::InsertionPointType::Append)
                .done()
            .done()
        .add_empty_child(36,span_tree::node::InsertionPointType::Append)
        .build();
    Expression {code,input_span_tree,output_span_tree}
}

pub fn expression_mock3() -> Expression {
    let code       = "image.blur 15".to_string();
    let parser     = Parser::new_or_panic();
    let this_param = span_tree::ArgumentInfo {
        name : Some("this".to_owned()),
        tp   : Some("Image".to_owned()),
    };
    let param0 = span_tree::ArgumentInfo {
        name : Some("radius".to_owned()),
        tp   : Some("Number".to_owned()),
    };
    let param1 = span_tree::ArgumentInfo {
        name : Some("area".to_owned()),
        tp   : Some("Vector Int".to_owned()),
    };
    let param2 = span_tree::ArgumentInfo {
        name : Some("matrix".to_owned()),
        tp   : Some("Vector String".to_owned()),
    };
    let parameters       = vec![this_param, param0, param1, param2];
    let ast              = parser.parse_line(&code).unwrap();
    let invocation_info  = span_tree::generate::context::CalledMethodInfo {parameters};
    let ctx              = span_tree::generate::MockContext::new_single(ast.id.unwrap(),invocation_info);
    let output_span_tree = span_tree::SpanTree::default();
    let input_span_tree  = span_tree::SpanTree::new(&ast,&ctx).unwrap();
    Expression {code,input_span_tree,output_span_tree}
}



// TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
// TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
// TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO

// Extract and make use in scene depth sorting.

#[allow(clippy::implicit_hasher)]
pub fn depth_sort(ids:&[usize], elem_above_elems:&HashMap<usize,Vec<usize>>) -> Vec<usize> {

    // === Remove from `elem_above_elems` all ids which are not present in `ids` ===

    let mut elem_above_elems : HashMap<usize,Vec<usize>> = elem_above_elems.clone();
    let mut missing = vec![];
    for (elem,above_elems) in &mut elem_above_elems {
        above_elems.retain(|id| ids.contains(id));
        if above_elems.is_empty() {
            missing.push(*elem);
        }
    }
    for id in &missing {
        elem_above_elems.remove(id);
    }


    // === Generate `elem_below_elems` map ===

    let mut elem_below_elems : HashMap<usize,Vec<usize>> = HashMap::new();
    for (above_id,below_ids) in &elem_above_elems {
        for below_id in below_ids {
            elem_below_elems.entry(*below_id).or_default().push(*above_id);
        }
    }


    // === Sort ids ===

    let mut queue        = HashSet::<usize>::new();
    let mut sorted       = vec![];
    let mut newly_sorted = vec![];

    for id in ids {
        if elem_above_elems.get(id).is_some() {
            queue.insert(*id);
        } else {
            newly_sorted.push(*id);
            while !newly_sorted.is_empty() {
                let id = newly_sorted.pop().unwrap();
                sorted.push(id);
                elem_below_elems.remove(&id).for_each(|above_ids| {
                    for above_id in above_ids {
                        if let Some(lst) = elem_above_elems.get_mut(&above_id) {
                            lst.remove_item(&id);
                            if lst.is_empty() && queue.contains(&above_id) {
                                queue.remove(&above_id);
                                newly_sorted.push(above_id);
                            }
                            if lst.is_empty() {
                                elem_above_elems.remove(&above_id);
                            }
                        }
                    }
                })
            }
        }
    }
    sorted
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identity_with_no_rules() {
        assert_eq!( depth_sort(&vec![]      , &default()) , Vec::<usize>::new() );
        assert_eq!( depth_sort(&vec![1]     , &default()) , vec![1] );
        assert_eq!( depth_sort(&vec![1,3]   , &default()) , vec![1,3] );
        assert_eq!( depth_sort(&vec![1,2,3] , &default()) , vec![1,2,3] );
    }


    #[test]
    fn chained_rules() {
        let mut rules = HashMap::<usize,Vec<usize>>::new();
        rules.insert(1,vec![2]);
        rules.insert(2,vec![3]);
        assert_eq!( depth_sort(&vec![]      , &rules) , Vec::<usize>::new() );
        assert_eq!( depth_sort(&vec![1]     , &rules) , vec![1] );
        assert_eq!( depth_sort(&vec![1,2]   , &rules) , vec![2,1] );
        assert_eq!( depth_sort(&vec![1,2,3] , &rules) , vec![3,2,1] );
    }

    #[test]
    fn order_preserving() {
        let mut rules = HashMap::<usize,Vec<usize>>::new();
        rules.insert(1,vec![2]);
        rules.insert(2,vec![3]);
        assert_eq!( depth_sort(&vec![10,11,12]          , &rules) , vec![10,11,12] );
        assert_eq!( depth_sort(&vec![10,1,11,12]        , &rules) , vec![10,1,11,12] );
        assert_eq!( depth_sort(&vec![10,1,11,2,12]      , &rules) , vec![10,11,2,1,12] );
        assert_eq!( depth_sort(&vec![10,1,11,2,12,3,13] , &rules) , vec![10,11,12,3,2,1,13] );
    }
}
