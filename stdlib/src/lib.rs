use std::process;
use std::io::{self, Write};

#[no_mangle]
pub extern "C" fn errorlevel(exit_code: i32) {
    process::exit(exit_code);
}

#[no_mangle]
pub extern "C" fn print(value: i32) {
    print!("{}", value);
    io::stdout().flush().unwrap();
}

#[no_mangle]
pub extern "C" fn println(value: i32) {
    println!("{}", value);
}
