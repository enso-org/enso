use ensogl_pack::prelude::*;

#[tokio::main]
async fn main() -> Result {
    ensogl_pack::main_lib(std::env::args().skip(1).collect()).await
}
