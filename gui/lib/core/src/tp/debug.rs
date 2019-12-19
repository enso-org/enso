use std::any::type_name;

pub trait TypeDebugName {
    fn type_debug_name() -> String; 
}

impl<T> TypeDebugName for T {
    default fn type_debug_name() -> String {
        type_name::<Self>().to_string()
    }
}
