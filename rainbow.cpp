#include <iostream>
#include <cstdlib>
#include <string>
#include <array>
#include <thread>
#include <numeric>
#include <cmath>
#include <algorithm>

#include "MVar.h"
#include "Effects/EffectRunner.h"
#include "Effects/RainbowCylinder.h"
#include "lisp/LispEnvironment.h"

#include <ecl/ecl.h>

static void lisp_repl(LED::MVar<LED::Effects::EffectRunner<LED::Effects::Effect>> runner) {
    (void)runner;
    LED::LispEnvironment::ensureLoaded();

    auto env = ecl_process_env();
    auto output = cl_core.standard_output;
    ECL_CATCH_ALL_BEGIN(env) {
        auto package_sym = ecl_make_symbol("*PACKAGE*", "COMMON-LISP");
        ecl_bds_bind(env, package_sym, ecl_find_package("LED/MAIN"));
        writestr_stream("Let's get lisp'n\n", output);

        auto die = ecl_make_symbol("DIE", "LED");
        auto restart = ecl_make_symbol("RESTART", "LED");

        while (true) {
            ECL_RESTART_CASE_BEGIN(env, cl_list(2, die, restart)) {
                cl_format(3, output, ecl_cstring_to_base_string_or_nil("~A> "), cl_package_name(cl_symbol_value(package_sym)));
                ecl_finish_output(output);

                auto form = cl_read(0);
                if (form == OBJNULL) {
                    return;
                }

                auto result = si_eval_with_env(1, form);
                ecl_prin1(result, output);
                ecl_terpri(output);
            } ECL_RESTART_CASE(1, args) {
                (void)args;
                exit(1);
            } ECL_RESTART_CASE(2, args) {
                (void)args;
                // We're cool.
            } ECL_RESTART_CASE_END;
        }
        ecl_bds_unwind1(env);
    } ECL_CATCH_ALL_END;
}

int main(int argc, char **argv) {
    auto effect = LED::make_MVar<LED::Effects::RainbowCylinder>();
    auto runner = LED::make_MVar<LED::Effects::EffectRunner<LED::Effects::Effect>>();
    runner.lock()->setEffect(effect);
    auto repl = std::thread(lisp_repl, runner);
    return LED::Effects::main(runner, argc, argv);
}
