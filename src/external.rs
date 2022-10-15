use std::ffi::{c_void, CStr};

use llvm_sys::support::LLVMAddSymbol;

use crate::parser::stmt::{Function, Prototype, Stmt};

macro_rules! cstr {
    ($str:expr) => {
        CStr::from_bytes_with_nul_unchecked(concat!($str, "\0").as_bytes())
    };
}

#[no_mangle]
pub extern "C" fn print(x: f64) {
    print!("{}", x);
}

#[no_mangle]
pub extern "C" fn println(x: f64) {
    println!("{}", x);
}

pub fn shim_builtin_functions() {
    unsafe {
        LLVMAddSymbol(cstr!("print").as_ptr(), print as *mut c_void);
        LLVMAddSymbol(cstr!("println").as_ptr(), println as *mut c_void);
    };
}

pub fn builtin_funtions() -> Vec<Stmt> {
    vec![
        Stmt::Function(new_builtin_fn("print", vec!["x".to_string()])),
        Stmt::Function(new_builtin_fn("println", vec!["x".to_string()])),
    ]
}

fn new_builtin_fn(name: &str, args: Vec<String>) -> Function {
    Function {
        prototype: Prototype {
            name: name.to_string(),
            args,
        },
        body: vec![],
        return_expr: None,
        is_anon: false,
        is_builtin: true,
    }
}
