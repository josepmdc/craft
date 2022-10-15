use std::ffi::{c_void, CStr};

use llvm_sys::support::LLVMAddSymbol;

macro_rules! cstr {
    ($str:expr) => {
        unsafe { CStr::from_bytes_with_nul_unchecked(concat!($str, "\0").as_bytes()) }
    };
}

static PRINT_NAME: &'static CStr = cstr!("print");
#[no_mangle]
pub extern "C" fn print(x: f64) {
    println!("{}", x);
}

pub fn shim_lib_functions() {
    let fn_name = PRINT_NAME.as_ptr();
    let fn_ptr = print as *mut c_void;
    unsafe { LLVMAddSymbol(fn_name, fn_ptr) };
}
