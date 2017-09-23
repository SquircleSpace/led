#include "lisp/LispBridge.h"

LED::Effects::LispBridge::LispBridge() {
    LispEnvironment::ensureLoaded();
    effect_ = cl_funcall(1, ecl_make_symbol("MAKE-EFFECT", "LED/EFFECT"));
    shader_ = cl_symbol_function(ecl_make_symbol("SHADER", "LED/EFFECT"));
    beginFrame_ = cl_symbol_function(ecl_make_symbol("BEGIN-FRAME", "LED/EFFECT"));
    endFrame_ = cl_symbol_function(ecl_make_symbol("END-FRAME", "LED/EFFECT"));
}
