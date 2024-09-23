use std::process;

#[no_mangle]
pub extern "C" fn errorlevel(exit_code: i32) {
    println!("exit code: {}", exit_code);
    process::exit(exit_code);
}

#[no_mangle]
pub extern "C" fn print_debug(value: i32) {
    println!("Debug: {}", value);
}
