#include "MVar.h"
#include "Effects/EffectRunner.h"
#include "Effects/LispBridge.h"

int main(int argc, char **argv) {
    auto effect = LED::make_MVar<LED::Effects::LispBridge>();
    LED::Effects::EffectRunner<LED::Effects::LispBridge> runner;
    runner.setEffect(effect);
    runner.main(argc, argv);

    return 0;
}
