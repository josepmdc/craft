use inkwell::AddressSpace;

use super::Compiler;

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn build_printf(&self) {
        let arg_ty = self.context.i8_type().ptr_type(AddressSpace::default());
        let func_ty = self.context.i32_type().fn_type(&[arg_ty.into()], true);
        self.module.add_function("printf", func_ty, None);
    }
}
