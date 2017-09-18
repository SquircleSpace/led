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
        float cycleTime_ = 5;
        float pixelGap_ = 1.0 / 15;
        float stripGap_ = 1.0 / 10;
        bool alternateDirections_ = true;
        float timeOffset_ = 0;
        float frameCost_ = 0;
        float value_ = 0.75;
        float saturation_ = 1;
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
    const int direction = (!alternateDirections_ || strip % 2 == 0 ? 1 : -1);
    const float pixel_offset_factor = (stripGap_ * strip) + (pixelGap_ * offset);
    const float time_factor = timeOffset_ / cycleTime_;
    const float cyclePortion = std::fmod(time_factor + pixel_offset_factor * direction, 1);
    hsv2rgb(rgb, cyclePortion, saturation_, value_);
}

void LED::RainbowCylinder::postProcess(const Vec3 &rgb, const PixelInfo &pixel) {
    (void)pixel;
    frameCost_ += rgb[0] + rgb[1] + rgb[2];
    Super::postProcess(rgb, pixel);
}

void LED::RainbowCylinder::beginFrame(const FrameInfo &frame) {
    timeOffset_ = std::fmod(timeOffset_ + frame.timeDelta, cycleTime_);
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
