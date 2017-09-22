#include "Effects/LispBridge.h"

namespace {
    class LispEnvironment {
        LispEnvironment();
        ~LispEnvironment();
    public:
        static void ensureLoaded();
    };
}

extern "C" {
    extern void init_lib_LISPLOGIC(cl_object cblock);
}

LispEnvironment::LispEnvironment() {
    ecl_set_option(ECL_OPT_TRAP_SIGINT, 0); // ^c should quit
    char empty[] = {'\0'};
    char *argv[] = {empty};
    cl_boot(0, argv);
    read_VV(OBJNULL, init_lib_LISPLOGIC);
}

LispEnvironment::~LispEnvironment() {
    cl_shutdown();
}

void LispEnvironment::ensureLoaded() {
    static LispEnvironment l;
}

LED::Effects::LispBridge::LispBridge() {
    LispEnvironment::ensureLoaded();
    effect_ = cl_funcall(1, ecl_make_symbol("MAKE-EFFECT", "LED/EFFECT"));
    shader_ = cl_symbol_function(ecl_make_symbol("SHADER", "LED/EFFECT"));
    beginFrame_ = cl_symbol_function(ecl_make_symbol("BEGIN-FRAME", "LED/EFFECT"));
    endFrame_ = cl_symbol_function(ecl_make_symbol("END-FRAME", "LED/EFFECT"));
}
