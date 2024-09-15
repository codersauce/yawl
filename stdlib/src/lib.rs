use std::process;

#[no_mangle]
pub extern "C" fn errorlevel(exit_code: i32) {
    process::exit(exit_code);
}
