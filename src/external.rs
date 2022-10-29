use std::ffi::{c_void, CStr};

use llvm_sys::support::LLVMAddSymbol;

use crate::parser::{
    stmt::{Function, Prototype, Stmt},
    Type, Variable,
};

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
        Stmt::Function(new_builtin_fn(
            "print",
            vec![Variable {
                name: "x".to_string(),
                type_: Type::F64,
            }],
            Type::Void,
        )),
        Stmt::Function(new_builtin_fn(
            "println",
            vec![Variable {
                name: "x".to_string(),
                type_: Type::F64,
            }],
            Type::Void,
        )),
    ]
}

fn new_builtin_fn(name: &str, params: Vec<Variable>, return_type: Type) -> Function {
    Function {
        prototype: Prototype {
            name: name.to_string(),
            params,
            return_type,
        },
        body: vec![],
        return_expr: None,
        is_builtin: true,
    }
}
