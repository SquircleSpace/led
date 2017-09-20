#include <iostream>
#include <cstdlib>
#include <string>
#include <array>
#include <thread>
#include <numeric>
#include <cmath>
#include <algorithm>

#include "MVar.h"
#include "EffectRunner.h"
#include "RainbowCylinder.h"

int main(int argc, char **argv) {
    auto effect = LED::make_MVar<LED::RainbowCylinder>();
    LED::EffectRunner<LED::RainbowCylinder> runner;
    runner.setEffect(effect);
    runner.main(argc, argv);

    return 0;
}
