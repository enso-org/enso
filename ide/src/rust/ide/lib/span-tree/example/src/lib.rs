use enso_prelude::*;
use wasm_bindgen::prelude::*;

use ensogl_system_web as web;

use enso_logger::AnyLogger;
use enso_logger::disabled::Logger;

use span_tree::*;


use ast::crumbs::*;
use ast::crumbs::PatternMatchCrumb::*;
use uuid::Uuid;
use span_tree::builder::Builder;
use span_tree::node::Kind;
use span_tree::node::InsertType;


#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_span_tree() {
    web::forward_panic_hook_to_console();
    web::set_stdout();
    web::set_stack_trace_limit();
    main();
}

pub fn main() {
    println!("hello");
    let pattern_cr       = vec![Seq { right: false }, Or, Or, Build];
    let val              = ast::crumbs::SegmentMatchCrumb::Body {val:pattern_cr};
    let parens_cr1       = ast::crumbs::MatchCrumb::Segs {val:val.clone(),index:0};
    let parens_cr        = ast::crumbs::MatchCrumb::Segs {val,index:0};
    let input_span_tree  = builder::TreeBuilder::new(36)
        .add_child(0,14,Kind::Chained,PrefixCrumb::Func)
            .add_child(0,9,Kind::Operation,PrefixCrumb::Func)
                .set_expression_id(Uuid::new_v4())
                .done()
            .add_empty_child(10,InsertType::BeforeTarget)
            .add_child(10,4,Kind::target(true),PrefixCrumb::Arg)
                .set_expression_id(Uuid::new_v4())
                .done()
            .add_empty_child(14,InsertType::Append)
            .set_expression_id(Uuid::new_v4())
            .done()
        .add_child(15,21,Kind::Argument {is_removable:true},PrefixCrumb::Arg)
            .set_expression_id(Uuid::new_v4())
            .add_child(1,19,Kind::Argument {is_removable:false},parens_cr1)
                .set_expression_id(Uuid::new_v4())
                .add_child(0,12,Kind::Operation,PrefixCrumb::Func)
                    .set_expression_id(Uuid::new_v4())
                    .done()
                .add_empty_child(13,InsertType::BeforeTarget)
                .add_child(13,6,Kind::Target {is_removable:false},PrefixCrumb::Arg)
                    .set_expression_id(Uuid::new_v4())
                    .done()
                .add_empty_child(19,InsertType::Append)
                .done()
            .done()
        .add_empty_child(36,InsertType::Append)
        .build();

    let input_span_tree2 = Node::new()
        .new_child(|t|t
            .new_id()
            .kind(Kind::Chained)
            .crumbs(PrefixCrumb::Func)
            .new_child(|t|t.size(9).kind(Kind::Operation).crumbs(PrefixCrumb::Func).new_id())
            .new_child(|t|t.size(1))
            .new_child(|t|t.size(4).kind(Kind::target(true)).crumbs(PrefixCrumb::Arg).new_id())
            .new_child(|t|t.size(1)))
        .new_child(|t|t
            .new_id()
            .kind(Kind::argument(true))
            .crumbs(PrefixCrumb::Arg)
            .new_child(|t|t
                .new_id()
                .offset(1)
                .kind(Kind::argument(true))
                .crumbs(parens_cr)
                .new_child(|t|t.size(12).kind(Kind::Operation).crumbs(PrefixCrumb::Func).new_id())
                .new_child(|t|t.size(1))
                .new_child(|t|t.size(6).kind(Kind::target(true)).crumbs(PrefixCrumb::Arg).new_id())
                .new_child(|t|t.size(1))
            ))
        .new_child(|t|t.size(1));

    println!("{:#?}",input_span_tree2);
}
