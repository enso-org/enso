use std::cell::Cell;
use std::rc::Rc;

use frp::f;
use frp::f_;

#[test]
fn lifetime_management() {
    frp::new_network! { network1
        def source = source::<()>();
    }
    frp::new_network! { network2
        def count   = source.count();
        def sampler = count.sampler();
    }
    assert_eq!(sampler.try_value(), Some(0));
    source.emit(());
    assert_eq!(sampler.try_value(), Some(1));
    source.emit(());
    assert_eq!(sampler.try_value(), Some(2));
    drop(network1);
    source.emit(());
    assert_eq!(sampler.try_value(), Some(2));
    drop(network2);
    source.emit(());
    assert_eq!(sampler.try_value(), None);
}

// #[test]
// fn weak_memory_management() {
//     frp::new_dynamic_network! {
//         def source = source::<()>();
//     }
//     let weak_source = source.downgrade();
//     assert!(weak_source.upgrade().is_some());
//     drop(source);
//     assert!(weak_source.upgrade().is_none());
// }

// #[test]
// fn lifetime_management_1() {
//     frp::new_dynamic_network! {
//         def source = source::<()>();
//         def count  = source.count();
//     }
//     let weak_source = source.downgrade();
//     assert!(weak_source.upgrade().is_some());
//     drop(source);
//     assert!(weak_source.upgrade().is_some());
//     drop(count);
//     assert!(weak_source.upgrade().is_none());
// }

// #[test]
// fn lifetime_management_2() {
//     frp::new_dynamic_network! {
//         def source  = source::<()>();
//         def count   = source.count();
//         def sampler = count.sampler();
//     }
//     // Dropping `count`. It's lifetime should be managed by `sampler` now.
//     drop(count);
//     assert_eq!(sampler.value(), 0);
//     source.emit(());
//     assert_eq!(sampler.value(), 1);
//     source.emit(());
//     assert_eq!(sampler.value(), 2);
//     let weak_source = source.downgrade();
//     drop(source);
//     assert!(weak_source.upgrade().is_some());
//     drop(sampler);
//     assert!(weak_source.upgrade().is_some());
// }

#[test]
fn test_toggle_true() {
    frp::new_network! { network
        def toggle_src = source::<()>();
        def map_src    = source::<()>();
        def toggle     = toggle_src.toggle_true();
        def _map       = map_src.map2(&toggle,|_,value| assert!(*value));
    }
    map_src.emit(());
}

#[test]
fn test_toggle() {
    frp::new_network! { network
        def toggle_src = source::<()>();
        def map_src    = source::<()>();
        def toggle     = toggle_src.toggle();
        def _map       = map_src.map2(&toggle,|_,value| assert!(*value));
    }
    toggle_src.emit(());
    map_src.emit(());
}

#[test]
fn test_gate() {
    let passed_events = Rc::new(Cell::new(0));
    frp::new_network! { network
        behavior   <- source::<bool>();
        some_event <- source::<()>();

        gated      <- some_event.gate(behavior);
        eval_ gated (passed_events.set(passed_events.get() + 1));

    };

    let input = &[false, true, true];
    for val in input {
        behavior.emit(val);
        some_event.emit(());
    }
    let true_count = input.iter().filter(|&&val| val).count();
    assert_eq!(passed_events.get(), true_count);
}

#[test]
fn test_filter() {
    let passed_events = Rc::new(Cell::new(0));
    frp::new_network! { network
        source   <- source::<bool>();
        filtered <- source.filter(|&value| value);
        eval filtered ([passed_events](&value) {
            assert!(value);
            passed_events.set(passed_events.get() + 1);
        });
    };

    let input = &[false, true, true, false, false];
    for val in input {
        source.emit(*val);
    }
    let true_count = input.iter().filter(|&&val| val).count();
    assert_eq!(passed_events.get(), true_count);
}

#[test]
fn test_filter_map() {
    let passed_events = Rc::new(Cell::new(0));
    frp::new_network! { network
        source        <- source::<bool>();
        filter_mapped <- source.filter_map(|v| v.then_some(()));
        eval_ filter_mapped (passed_events.set(passed_events.get() + 1));
    };

    let input = &[false, true, true, false, false];
    for val in input {
        source.emit(*val);
    }
    let true_count = input.iter().filter(|&&val| val).count();
    assert_eq!(passed_events.get(), true_count);
}
