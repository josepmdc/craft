use std::{
    ffi::{CStr, c_void},
    io::{stderr, Write},
};

use llvm_sys::support::LLVMAddSymbol;

macro_rules! cstr {
    ($str:expr) => {
        unsafe { CStr::from_bytes_with_nul_unchecked(concat!($str, "\0").as_bytes()) }
    };
}

static PUTCHARD_NAME: &'static CStr = cstr!("putchard");
#[no_mangle]
pub extern "C" fn putchard(x: f64) -> f64 {
    let mut stderr = stderr();
    match write!(stderr, "{}", x as u8 as char).ok() {
        Some(()) => (),
        None => return 1.0,
    }
    match stderr.flush().ok() {
        Some(()) => (),
        None => return 1.0,
    }

    0.0
}

static PRINT_NAME: &'static CStr = cstr!("print");
#[no_mangle]
pub extern "C" fn print(x: f64) -> f64 {
    let mut stderr = stderr();
    match writeln!(stderr, "{}", x).ok() {
        Some(()) => (),
        None => return 1.0,
    }
    match stderr.flush().ok() {
        Some(()) => (),
        None => return 1.0,
    }

    0.0
}

pub struct PrintFunc {
    pub name: &'static CStr,
    pub func_pointer: extern "C" fn(f64) -> f64,
}

#[used]
pub static PRINT_FNS: [PrintFunc; 2] = [
    PrintFunc {
        name: PUTCHARD_NAME,
        func_pointer: putchard,
    },
    PrintFunc {
        name: PRINT_NAME,
        func_pointer: print,
    },
];

pub fn shim_lib_functions() {
    for func in &PRINT_FNS {
        let fn_name = func.name.as_ptr();
        let fn_ptr = func.func_pointer as *mut c_void;
        unsafe { LLVMAddSymbol(fn_name, fn_ptr) };
    }
}
