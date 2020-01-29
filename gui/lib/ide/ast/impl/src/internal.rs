/// Iterate recursively over tree-like structure implementing `IntoIterator`.
pub fn iterate_subtree<T>(ast:T) -> impl Iterator<Item=T::Item>
    where T: IntoIterator<Item=T> + Copy {
    let generator = move || {
        let mut nodes:Vec<T> = vec![ast];
        while !nodes.is_empty() {
            let ast = nodes.pop().unwrap();
            nodes.extend(ast.into_iter());
            yield ast;
        }
    };

    shapely::GeneratingIterator(generator)
}
