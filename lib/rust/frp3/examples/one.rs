use enso_frp3 as frp;
pub fn main() {
    #[derive(Clone, PartialEq, Debug)]
    struct Wrapped(u32);

    struct A<T> {
        x: T,
    }

    frp::new_network! { net
        source1 <- source();
        source2 <- source();
        source3 <- source();
        source4 <- source(7);
        source5 <- source(Wrapped(15));

        let nested_source = A { x: A { x: source1 } };

        any  <- any(source1, source2, source3, source4);
        any_ <- any_(source1, source2, source3, source4, source5);

        all <- all(source1, source2, source3, source4, source5);

        any_manual <- any(...);
        any_manual <+ source1;
        any_manual <+ source2;

        all_vec <- all [source1, source2, source3, source4];
        count <- all.count();
        nest_count <- nested_source.x.x.count();
        trace nest_count;

        boolean <- bool(source1, source2);
        trace boolean;

        fan <- fan();
        fan <+ source1;
        fan <+ source2;
        fan <+ source5;
        let fan_int = fan.output::<i32>();
        let fan_wrap = fan.output::<Wrapped>();

        s_any <- any.sampler();
        s_any_ <- any_.sampler();
        s_all <- all.sampler_with_init((0, 0, 0, 0, Wrapped(0)));
        s_any_manual <- any_manual.sampler();
        s_all_vec <- all_vec.sampler();
        fan_int <- fan_int.sampler();
        fan_wrap <- fan_wrap.sampler_with_init(Wrapped(5));
    };

    source1.emit(1);
    source2.emit(2);
    source3.emit(3);

    assert_eq!(s_any.value(), 3);
    // assert_eq!(s_any_.value(), ());
    assert_eq!(s_all.value(), (1, 2, 3, 7, Wrapped(15)));
    assert_eq!(s_any_manual.value(), 2);
    assert_eq!(s_all_vec.value(), [1, 2, 3, 7]);
    assert_eq!(count.value(), 3);
    assert_eq!(fan_int.value(), 2);
    assert_eq!(fan_wrap.value(), Wrapped(5));

    source5.emit(Wrapped(10));
    assert_eq!(fan_int.value(), 2);
    assert_eq!(fan_wrap.value(), Wrapped(10));

    assert_eq!(1, 2);
}
