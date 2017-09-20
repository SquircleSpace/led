#include "Effects/RainbowCylinder.h"

#include <fadecandy/color.h>

void LED::Effects::RainbowCylinder::shader(Vec3 &rgb, const PixelInfo &pixel) const {
    const int strip = std::round(pixel.getNumber("strip"));
    const int offset = std::round(pixel.getNumber("offset"));
    const int direction = (!alternateDirections_ || strip % 2 == 0 ? 1 : -1);
    const float pixel_offset_factor = (stripGap_ * strip) + (pixelGap_ * offset);
    const float time_factor = timeOffset_ / cycleTime_;
    const float cyclePortion = std::fmod(time_factor + pixel_offset_factor * direction, 1);
    hsv2rgb(rgb, cyclePortion, saturation_, value_);
}

void LED::Effects::RainbowCylinder::postProcess(const Vec3 &rgb, const PixelInfo &) {
    frameCost_ += rgb[0] + rgb[1] + rgb[2];
}

void LED::Effects::RainbowCylinder::beginFrame(const FrameInfo &frame) {
    timeOffset_ = std::fmod(timeOffset_ + frame.timeDelta, cycleTime_);
    frameCost_ = 0;
}

void LED::Effects::RainbowCylinder::endFrame(const FrameInfo &) {
}

void LED::Effects::RainbowCylinder::debug(const DebugInfo &) {
}
