mod common;
use enso_frp3 as frp;

#[cfg(test)]
mod test {
    use super::common::Collector;
    use super::*;
    use frp::*;

    #[test]
    fn test_basic_source() {
        let (_guard, collector) = Collector::new();
        let net = Network::new();
        let source = net.source::<i32>();

        net.map_with(source, (), move |x| {
            collector.push(*x);
        });
        source.emit(&1);
        source.emit(&2);
        collector.assert_chunk(&[1, 2]);
    }

    #[test]
    fn test_basic_map() {
        let (_guard, collector) = Collector::new();
        let net = Network::new();
        let source = net.source::<i32>();
        let mapped = net.map_with(source, (), move |x| *x * 2);
        net.map_with(mapped, (), move |x| collector.push(*x));
        source.emit(&1);
        source.emit(&2);
        collector.assert_chunk(&[2, 4]);
    }

    #[test]
    fn test_basic_map2() {
        let (_guard, collector) = Collector::new();
        let (_guard, collector2) = Collector::new();
        let net = Network::new();
        let source1 = net.source::<i32>();
        let source2 = net.source::<i32>();
        let mapped = net.map_with(source1, source2, move |a, b| *a + *b * 10);
        let mapped2 = net.map_with_ref(source1, source2, move |a, _| a);
        net.map_with(mapped, (), move |x| collector.push(*x));
        net.eval(mapped2, move |x: &i32| {
            collector2.push(*x);
        });
        source1.emit(1);
        source2.emit(2);
        source1.emit(3);
        collector.assert_chunk(&[1, 23]);
        collector2.assert_chunk(&[1, 3]);
    }

    #[test]
    fn test_nested_map() {
        let (_guard, collector) = Collector::new();
        let (_guard, collector2) = Collector::new();
        let net = Network::new();
        let source1 = net.source::<i32>();
        let source2 = net.source::<i32>();
        let mapped = net.map_with(source1, source2, move |a, b| *a + *b * 10);
        let mapped2 =
            net.map_with(source1, (source2, mapped), move |a, b, c| a * 100000 + b * 100 + c);
        net.eval(mapped, move |x| collector.push(*x));
        net.eval(mapped2, move |x| collector2.push(*x));
        source1.emit(1);
        source2.emit(2);
        source1.emit(3);
        collector.assert_chunk(&[1, 23]);
        collector2.assert_chunk(&[10_00_01, 30_02_23]);
    }

    #[test]
    fn test_macros() {
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
        assert_eq!(s_any_.value(), ());
        assert_eq!(s_all.value(), (1, 2, 3, 7, Wrapped(15)));
        assert_eq!(s_any_manual.value(), 2);
        assert_eq!(s_all_vec.value(), [1, 2, 3, 7]);
        assert_eq!(count.value(), 3);
        assert_eq!(fan_int.value(), 2);
        assert_eq!(fan_wrap.value(), Wrapped(5));

        source5.emit(Wrapped(10));
        assert_eq!(fan_int.value(), 2);
        assert_eq!(fan_wrap.value(), Wrapped(10));
    }

    #[test]
    fn test_all_count() {
        let net = Network::new();

        frp::extend! { net
            source1 <- source::<i32>();
            source2 <- source::<i32>();
            all <- all(source1, source2);
            count <- all.count();
        };

        assert_eq!(count.value(), 0);

        net.trace(all);
        source1.emit(1);
        source2.emit(2);
        source1.emit(3);
        assert_eq!(count.value(), 3);
        assert_eq!(count.value(), 3);
        source1.emit(0);
        source1.emit(0);
        assert_eq!(count.value(), 5);
        assert_eq!(count.value(), 5);
    }

    #[test]
    fn test_map_ref() {
        struct Wrapped(i32);
        let (_guard, collector) = Collector::new();
        let net = Network::new();
        let source = net.source::<i32>();
        let wrapped = net.map(source, move |a| Wrapped(*a));
        let inner_ref = net.map_ref(wrapped, move |Wrapped(ref a)| a);
        net.eval(inner_ref, move |x| collector.push(*x));
        source.emit(1);
        source.emit(3);
        collector.assert_chunk(&[1, 3]);
    }

    #[test]
    fn test_basic_all() {
        let (_guard, collector) = Collector::new();
        let net = Network::new();
        let source1 = net.source::<i32>();
        let source2 = net.source::<i32>();
        let all = net.all((source1, source2));
        let mapped = net.filter_map_with(all, (), move |(a, b)| Some(*a + *b * 10));
        net.map_with(mapped, (), move |x| collector.push(*x));
        source1.emit(1);
        source2.emit(2);
        source1.emit(3);
        collector.assert_chunk(&[1, 21, 23]);
    }

    #[test]
    fn test_very_long_chain() {
        let (_guard, collector) = Collector::new();

        let net = Network::new();
        let source = net.source::<i32>();

        let node = (0..111).fold(source.as_stream(), |stream, _| {
            net.map(stream, move |x| {
                collector.push(*x);
                if x % 2 == 0 {
                    x / 2
                } else {
                    x * 3 + 1
                }
            })
        });
        let last = net.sampler(node);

        source.emit(27);
        assert_eq!(last.value(), 1);
        collector.assert_chunk(&[
            27, 82, 41, 124, 62, 31, 94, 47, 142, 71, 214, 107, 322, 161, 484, 242, 121, 364, 182,
            91, 274, 137, 412, 206, 103, 310, 155, 466, 233, 700, 350, 175, 526, 263, 790, 395,
            1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502, 251, 754, 377, 1132, 566, 283,
            850, 425, 1276, 638, 319, 958, 479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429,
            7288, 3644, 1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232, 4616, 2308, 1154, 577,
            1732, 866, 433, 1300, 650, 325, 976, 488, 244, 122, 61, 184, 92, 46, 23, 70, 35, 106,
            53, 160, 80, 40, 20, 10, 5, 16, 8, 4, 2,
        ]);
    }

    #[test]
    fn test_fan() {
        let (_guard, collector_a) = Collector::new();
        let (_guard, collector_b) = Collector::new();

        struct A(i32);
        struct B(i32);
        struct C(i32);

        let net = Network::new();
        let fan = net.fan();

        let a_output = fan.output::<A>();
        let b_output = fan.output::<B>();
        net.eval(a_output, move |x| collector_a.push(x.0));
        net.eval(b_output, move |x| collector_b.push(x.0));

        fan.emit(&A(1));
        fan.emit(&B(2));
        fan.emit(&A(3));
        fan.emit(&B(4));
        fan.emit(&C(5));

        collector_a.assert_chunk(&[1, 3]);
        collector_b.assert_chunk(&[2, 4]);
    }

    #[test]
    fn test_all_vec() {
        let (_guard, collector) = Collector::new();
        let net = Network::new();

        let source1 = net.source::<i32>();
        let source2 = net.source::<i32>();
        let source3 = net.source::<i32>();
        let any_vec = net.all_vec((source1, source2, source3));
        net.eval(any_vec, move |x| x.iter().for_each(|x| collector.push(*x)));

        source1.emit(&1);
        source2.emit(&2);
        source3.emit(&3);

        collector.assert_chunk(&[1, 0, 0, 1, 2, 0, 1, 2, 3]);
    }

    #[test]
    fn test_gate() {
        let (_guard, collector1) = Collector::new();
        let (_guard, collector2) = Collector::new();
        let net = Network::new();
        let source = net.source::<i32>();
        let cond = net.source::<bool>();
        let gate = net.gate(source, cond);
        let gate_not = net.gate_not(source, cond);
        net.eval(gate, move |x| collector1.push(*x));
        net.eval(gate_not, move |x| collector2.push(*x));
        source.emit(1);
        source.emit(3);
        collector1.assert_chunk(&[]);
        collector2.assert_chunk(&[1, 3]);
        cond.emit(true);
        source.emit(5);
        source.emit(5);
        collector1.assert_chunk(&[5, 5]);
        collector2.assert_chunk(&[]);
        cond.emit(false);
        source.emit(7);
        source.emit(5);
        collector1.assert_chunk(&[]);
        collector2.assert_chunk(&[7, 5]);
    }
}
