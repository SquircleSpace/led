#include "LispEnvironment.h"
#include "Binding.h"

#include <ecl/ecl.h>

extern "C" {
    extern void init_lib_LISPLOGIC(cl_object cblock);
}

LED::LispEnvironment::LispEnvironment() {
    ecl_set_option(ECL_OPT_TRAP_SIGINT, 0); // ^c should quit
    char empty[] = {'\0'};
    char *argv[] = {empty};
    cl_boot(0, argv);
    auto sym = ecl_make_symbol("*STANDARD-OUTPUT*", "COMMON-LISP");
    auto stream = cl_make_broadcast_stream(0);
    auto require = [] () {
        cl_require(1, ecl_make_keyword("ASDF"));
    };
    LED::lisp::Binding::with(sym, stream, require);
    read_VV(OBJNULL, init_lib_LISPLOGIC);
}

LED::LispEnvironment::~LispEnvironment() {
    cl_shutdown();
}

void LED::LispEnvironment::ensureLoaded() {
    static LispEnvironment l;
}
