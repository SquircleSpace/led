#include <fadecandy/color.h>

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

namespace LED {
    class RainbowCylinder {
        float cycleTime_ = 5;
        float pixelGap_ = 1.0 / 15;
        float stripGap_ = 1.0 / 10;
        bool alternateDirections_ = true;
        float timeOffset_ = 0;
        float frameCost_ = 0;
        float value_ = 0.75;
        float saturation_ = 1;
    public:
        using PixelInfo = Effect::PixelInfo;
        using FrameInfo = Effect::FrameInfo;
        using DebugInfo = Effect::DebugInfo;

        void shader(Vec3 &, const PixelInfo &) const;
        void postProcess(const Vec3 &, const PixelInfo &);
        void beginFrame(const FrameInfo &);
        void endFrame(const FrameInfo &);
        void debug(const DebugInfo &);
    };
}

void LED::RainbowCylinder::shader(Vec3 &rgb, const PixelInfo &pixel) const {
    const int strip = std::round(pixel.getNumber("strip"));
    const int offset = std::round(pixel.getNumber("offset"));
    const int direction = (!alternateDirections_ || strip % 2 == 0 ? 1 : -1);
    const float pixel_offset_factor = (stripGap_ * strip) + (pixelGap_ * offset);
    const float time_factor = timeOffset_ / cycleTime_;
    const float cyclePortion = std::fmod(time_factor + pixel_offset_factor * direction, 1);
    hsv2rgb(rgb, cyclePortion, saturation_, value_);
}

void LED::RainbowCylinder::postProcess(const Vec3 &rgb, const PixelInfo &) {
    frameCost_ += rgb[0] + rgb[1] + rgb[2];
}

void LED::RainbowCylinder::beginFrame(const FrameInfo &frame) {
    timeOffset_ = std::fmod(timeOffset_ + frame.timeDelta, cycleTime_);
    frameCost_ = 0;
}

void LED::RainbowCylinder::endFrame(const FrameInfo &) {
}

void LED::RainbowCylinder::debug(const DebugInfo &) {
}

int main(int argc, char **argv) {
    auto effect = LED::make_MVar<LED::RainbowCylinder>();
    LED::EffectRunner<LED::RainbowCylinder> runner;
    runner.setEffect(effect);
    runner.main(argc, argv);

    return 0;
}
