#include "Binding.h"

LED::lisp::Binding::Binding(cl_object symbol, cl_object value)
  : env_(ecl_process_env())
{
    ecl_bds_bind(env_, symbol, value);
}

LED::lisp::Binding::~Binding() {
    ecl_bds_unwind1(env_);
}
