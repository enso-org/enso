use enso_prelude::*;
use wasm_bindgen::prelude::*;

use enso_web as web;

use ast::crumbs::PatternMatchCrumb::*;
use ast::crumbs::*;
use span_tree::builder::Builder;
use span_tree::node;
use span_tree::node::InsertionPointType;
use span_tree::*;
use uuid::Uuid;


#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_span_tree() {
    web::forward_panic_hook_to_console();
    web::set_stack_trace_limit();
    main();
}

pub fn main() {
    let pattern_cr = vec![Seq { right: false }, Or, Or, Build];
    let val = ast::crumbs::SegmentMatchCrumb::Body { val: pattern_cr };
    let parens_cr1 = ast::crumbs::MatchCrumb::Segs { val: val.clone(), index: 0 };
    let parens_cr = ast::crumbs::MatchCrumb::Segs { val, index: 0 };
    let _input_span_tree = builder::TreeBuilder::<()>::new(36)
        .add_child(0, 14, node::Kind::Chained, PrefixCrumb::Func)
        .add_child(0, 9, node::Kind::Operation, PrefixCrumb::Func)
        .set_ast_id(Uuid::new_v4())
        .done()
        .add_empty_child(10, InsertionPointType::BeforeTarget)
        .add_child(10, 4, node::Kind::this().removable(), PrefixCrumb::Arg)
        .set_ast_id(Uuid::new_v4())
        .done()
        .add_empty_child(14, InsertionPointType::Append)
        .set_ast_id(Uuid::new_v4())
        .done()
        .add_child(15, 21, node::Kind::argument().removable(), PrefixCrumb::Arg)
        .set_ast_id(Uuid::new_v4())
        .add_child(1, 19, node::Kind::argument(), parens_cr1)
        .set_ast_id(Uuid::new_v4())
        .add_child(0, 12, node::Kind::Operation, PrefixCrumb::Func)
        .set_ast_id(Uuid::new_v4())
        .done()
        .add_empty_child(13, InsertionPointType::BeforeTarget)
        .add_child(13, 6, node::Kind::this(), PrefixCrumb::Arg)
        .set_ast_id(Uuid::new_v4())
        .done()
        .add_empty_child(19, InsertionPointType::Append)
        .done()
        .done()
        .add_empty_child(36, InsertionPointType::Append)
        .build();

    let input_span_tree2 = Node::<()>::new()
        .new_child(|t| {
            t.new_ast_id()
                .kind(node::Kind::Chained)
                .crumbs(PrefixCrumb::Func)
                .new_child(|t| {
                    t.size(9).kind(node::Kind::Operation).crumbs(PrefixCrumb::Func).new_ast_id()
                })
                .new_child(|t| t.size(1))
                .new_child(|t| {
                    t.size(4)
                        .kind(node::Kind::this().removable())
                        .crumbs(PrefixCrumb::Arg)
                        .new_ast_id()
                })
                .new_child(|t| t.size(1))
        })
        .new_child(|t| {
            t.new_ast_id()
                .kind(node::Kind::argument().removable())
                .crumbs(PrefixCrumb::Arg)
                .new_child(|t| {
                    t.new_ast_id()
                        .offset(1)
                        .kind(node::Kind::argument().removable())
                        .crumbs(parens_cr)
                        .new_child(|t| {
                            t.size(12)
                                .kind(node::Kind::Operation)
                                .crumbs(PrefixCrumb::Func)
                                .new_ast_id()
                        })
                        .new_child(|t| t.size(1))
                        .new_child(|t| {
                            t.size(6)
                                .kind(node::Kind::this().removable())
                                .crumbs(PrefixCrumb::Arg)
                                .new_ast_id()
                        })
                        .new_child(|t| t.size(1))
                })
        })
        .new_child(|t| t.size(1));

    DEBUG!("{input_span_tree2:#?}");
}
