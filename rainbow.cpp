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

int main(int argc, char **argv) {
    auto effect = LED::make_MVar<LED::Effects::RainbowCylinder>();
    LED::Effects::EffectRunner<LED::Effects::RainbowCylinder> runner;
    runner.setEffect(effect);
    runner.main(argc, argv);

    return 0;
}
