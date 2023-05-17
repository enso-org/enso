mod common;

// #[test]
// fn base_effect() {
//     collect!(data);

//     frp::create_network(|n| {
//         let x = n.signal(0);

//         n.effect(move |_| {
//             data.push(x.get());
//         });
//         data.assert_chunk(&[0]);
//         x.set(1);
//         data.assert_chunk(&[1]);
//     });

//     data.assert_empty();
// }
