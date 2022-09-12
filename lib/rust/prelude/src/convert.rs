pub trait FromInContext<Ctx, T> {
    fn from_in_context(context: Ctx, arg: T) -> Self;
}

pub trait IntoInContext<Ctx, T> {
    fn into_in_context(self, context: Ctx) -> T;
}

impl<Ctx, T, U> IntoInContext<Ctx, U> for T
where U: FromInContext<Ctx, T>
{
    fn into_in_context(self, context: Ctx) -> U {
        U::from_in_context(context, self)
    }
}
