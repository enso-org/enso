#![allow(missing_docs)]

//! NOTE
//! This file is under a heavy development. It contains commented lines of code and some code may
//! be of poor quality. Expect drastic changes.

use ensogl::prelude::*;

use ensogl::display::navigation::navigator::Navigator;
use ensogl::system::web;
use ensogl::application::Application;
use graph_editor::{GraphEditor, EdgeTarget};
use wasm_bindgen::prelude::*;
use ensogl::display::object::ObjectOps;
use ensogl_core_msdf_sys::run_once_initialized;
use ensogl::display::style::theme;
use ensogl::data::color;
use enso_frp::Position;
use enso_frp as frp;


#[wasm_bindgen]
#[allow(dead_code)]
pub fn run_example_shapes() {
    web::forward_panic_hook_to_console();
    web::set_stdout();
    web::set_stack_trace_limit();
    run_once_initialized(|| {
        let app = Application::new(&web::get_html_element_by_id("root").unwrap());
        init(&app);
        mem::forget(app);
    });
}


fn fence<T,Out>(network:&frp::Network, trigger:T) -> (frp::Stream,frp::Stream<bool>)
where T:frp::HasOutput<Output=Out>, T:Into<frp::Stream<Out>>, Out:frp::Data {
    let trigger = trigger.into();
    frp::extend! { network
        def trigger_ = trigger.constant(());
        def runner   = source::<()>();
        def switch   = gather();
        switch.attach(&trigger_);
        def triggered = trigger.map(f_!(runner.emit(())));
        switch.attach(&triggered);
        def condition = switch.toggle_true();
    }
    let runner = runner.into();
    (runner,condition)
}

//fn fenced_gate<F,T,X>(network:&frp::Network, target:&frp::Stream<X>, f:F) -> frp::Stream<X>
//where F:'static+Fn()->T, X:frp::Data {
//    let target = target.clone_ref();
//    frp::extend! { network
//        let (trigger,runner,condition) = fence(&network);
//        def _eval = runner.map(move |_| {f();});
//        def out   = target.gate(&condition);
//    }
//    out
//}

fn init(app:&Application) {

    let mut dark = theme::Theme::new();
    dark.insert("application.background.color", color::Lcha::new(0.13,0.013,0.18,1.0));
    dark.insert("graph_editor.node.background.color", color::Lcha::new(0.2,0.013,0.18,1.0));
    dark.insert("graph_editor.node.selection.color", color::Lcha::new(0.72,0.5,0.22,1.0));
    dark.insert("graph_editor.node.selection.size", 7.0);
//    dark.insert("graph_editor.node.selection.color", color::Lcha::new(0.7,0.59,0.18,1.0));
    dark.insert("animation.duration", 0.5);
    dark.insert("graph.node.shadow.color", 5.0);
    dark.insert("graph.node.shadow.size", 5.0);
    dark.insert("mouse.pointer.color", color::Rgba::new(0.3,0.3,0.3,1.0));

    app.themes.register("dark",dark);
    app.themes.set_enabled(&["dark"]);

    let _bg = app.display.scene().style_sheet.var("application.background.color");

//    println!("{:?}",bg.value());
//    println!("{:?}",app.display.scene().style_sheet.debug_sheet_nodes_count());

//    let t1 : color::Hsla = color::Hsla::new(0.0,0.0,0.03,1.0);
//    let t2 : color::Lcha = t1.into();
//    let t4 : color::Rgba = color::Rgba::from(t2);
//    println!("{:?}", t2);
//    println!("{:?}", color::Rgba::from(t1));
//    println!("{:?}", t4);
//    println!("{:?}", color::Hsla::from(color::LinearRgba::new(0.2,0.3,0.4,1.0)));
//
//    let x = color::Hsla::from(color::Rgba::new(0.031,0.031,0.031,1.0));
//    let y = color::Rgba::from(x);
//    println!("{:?}", y);
//    let xyz = color::Xyz::from(color::Rgb::new(0.2,0.4,0.6));
//    let lab = color::Lab::from(color::Rgb::new(0.2,0.4,0.6));
//    let lch = color::Lch::from(color::Rgb::new(0.2,0.4,0.6));
//    let lch = color::Lch::from(color::Rgb::new(1.0,0.0,0.0));
//    println!("{:?}", xyz);
//    println!("{:?}", lab);
//    println!("{:?}", lch);
//    println!("-----------");
//    println!("{:?}", color::Rgb::from(xyz));
//    println!("{:?}", color::Rgb::from(lab));
//    println!("{:?}", color::Rgb::from(lch));
//    println!("{:?}", color::Lab::from(color::Xyz::new(0.1,0.2,0.3)));

//    println!("{:?}", palette::Xyz::from(palette::Srgb::new(0.2,0.4,0.6)));
//    println!("{:?}", palette::Lab::from(palette::LinSrgb::new(0.2,0.4,0.6)));
//    println!("{:?}", palette::Lab::from(palette::Xyz::new(0.1,0.2,0.3)));


//    color::test();

    let world     = &app.display;
    let scene     = world.scene();
    let camera    = scene.camera();
    let navigator = Navigator::new(&scene,&camera);

    app.views.register::<GraphEditor>();
    let graph_editor = app.views.new::<GraphEditor>();
    world.add_child(&graph_editor);


    let node1_id = graph_editor.add_node();
    let node2_id = graph_editor.add_node();

    graph_editor.frp.set_node_position.emit((node1_id,Position::new(100.0 , 250.0)));
    graph_editor.frp.set_node_position.emit((node2_id,Position::new(200.0 ,  50.0)));

    graph_editor.frp.set_node_expression.emit((node1_id,expression_mock()));
    graph_editor.frp.set_node_expression.emit((node2_id,expression_mock()));

    frp::new_network! { network
        def trigger = source::<()>();
        let (runner,condition) = fence(&network,&trigger);
        def _eval = runner.map(f_!( {
            graph_editor.frp.connect_nodes.emit((EdgeTarget::new(node1_id,default()),EdgeTarget::new(node2_id,vec![1,0,2])));
        }));
        def _debug = graph_editor.frp.outputs.edge_added.map2(&condition, |id,cond| {
            let owner = if *cond { "GUI" } else { "ME" };
            println!("Edge {:?} added by {}!",id,owner)
        });

    }

    trigger.emit(());


    let mut was_rendered = false;
    let mut loader_hidden = false;
    world.on_frame(move |_| {
        let _keep_alive = &navigator;
        let _keep_alive = &graph_editor;
        let _keep_alive = &network;

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
}



// =============
// === Mocks ===
// =============

use ast::crumbs::PatternMatchCrumb::*;
use ast::crumbs::*;
use span_tree::traits::*;
use graph_editor::component::node::port::Expression;


pub fn expression_mock() -> Expression {
    let pattern_cr       = vec![Seq { right: false }, Or, Or, Build];
    let val              = ast::crumbs::SegmentMatchCrumb::Body {val:pattern_cr};
    let parens_cr        = ast::crumbs::MatchCrumb::Segs {val,index:0};
    let code             = "draw_maps size (distribution normal)".into();
    let output_span_tree = default();
    let input_span_tree  = span_tree::builder::TreeBuilder::new(36)
        .add_child(0,14,span_tree::node::Kind::Chained,PrefixCrumb::Func)
        .add_leaf(0,9,span_tree::node::Kind::Operation,PrefixCrumb::Func)
        .add_empty_child(10,span_tree::node::InsertType::BeforeTarget)
        .add_leaf(10,4,span_tree::node::Kind::Target {is_removable:true},PrefixCrumb::Arg)
        .add_empty_child(14,span_tree::node::InsertType::Append)
        .done()
        .add_child(15,21,span_tree::node::Kind::Argument {is_removable:true},PrefixCrumb::Arg)
        .add_child(1,19,span_tree::node::Kind::Argument {is_removable:false},parens_cr)
        .add_leaf(0,12,span_tree::node::Kind::Operation,PrefixCrumb::Func)
        .add_empty_child(13,span_tree::node::InsertType::BeforeTarget)
        .add_leaf(13,6,span_tree::node::Kind::Target {is_removable:false},PrefixCrumb::Arg)
        .add_empty_child(19,span_tree::node::InsertType::Append)
        .done()
        .done()
        .add_empty_child(36,span_tree::node::InsertType::Append)
        .build();
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
