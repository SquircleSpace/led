#include "LispEnvironment.h"

#include <ecl/ecl.h>

extern "C" {
    extern void init_lib_LISPLOGIC(cl_object cblock);
}

LED::LispEnvironment::LispEnvironment() {
    ecl_set_option(ECL_OPT_TRAP_SIGINT, 0); // ^c should quit
    char empty[] = {'\0'};
    char *argv[] = {empty};
    cl_boot(0, argv);
    read_VV(OBJNULL, init_lib_LISPLOGIC);
}

LED::LispEnvironment::~LispEnvironment() {
    cl_shutdown();
}

void LED::LispEnvironment::ensureLoaded() {
    static LispEnvironment l;
}
