#include <fadecandy/effect_runner.h>
#include <fadecandy/effect.h>
#include <fadecandy/color.h>

#include <iostream>
#include <cstdlib>
#include <string>
#include <array>
#include <thread>
#include <numeric>
#include <cmath>
#include <algorithm>

namespace LED {
    class RainbowCylinder : public Effect {
        using Super = Effect;
        static constexpr float CYCLE_TIME = 10;
        static constexpr float PIXEL_GAP = 1.0 / 15;
        static constexpr float STRIP_GAP = 1.0 / 10;
        float timeOffset_ = 0;
        float frameCost_ = 0;
    public:
        void shader(Vec3 &, const PixelInfo &) const override;
        void postProcess(const Vec3 &, const PixelInfo &) override;
        void beginFrame(const FrameInfo &) override;
        void endFrame(const FrameInfo &) override;
    };
}

void LED::RainbowCylinder::shader(Vec3 &rgb, const PixelInfo &pixel) const {
    const int strip = std::round(pixel.getNumber("strip"));
    const int offset = std::round(pixel.getNumber("offset"));
    const float pixel_offset_factor = STRIP_GAP * strip + PIXEL_GAP * offset;
    const float time_factor = timeOffset_ / CYCLE_TIME;
    const float cyclePortion = std::fmod(time_factor + pixel_offset_factor, 1);
    hsv2rgb(rgb, cyclePortion, 1, .75);
}

void LED::RainbowCylinder::postProcess(const Vec3 &rgb, const PixelInfo &pixel) {
    (void)pixel;
    frameCost_ += rgb[0] + rgb[1] + rgb[2];
    Super::postProcess(rgb, pixel);
}

void LED::RainbowCylinder::beginFrame(const FrameInfo &frame) {
    timeOffset_ = std::fmod(timeOffset_ + frame.timeDelta, CYCLE_TIME);
    frameCost_ = 0;
    Super::beginFrame(frame);
}

void LED::RainbowCylinder::endFrame(const FrameInfo &frame) {
    Super::endFrame(frame);
}

int main(int argc, char **argv) {
    LED::RainbowCylinder rainbow;
    EffectRunner runner;
    runner.setEffect(&rainbow);
    runner.main(argc, argv);

    return 0;
}
