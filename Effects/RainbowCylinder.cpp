#include "Effects/RainbowCylinder.h"

#include <fadecandy/color.h>

void LED::Effects::RainbowCylinder::shader(Vec3 &rgb, const PixelInfo &pixel) const {
    const int strip = std::round(pixel.getNumber("strip"));
    const int offset = std::round(pixel.getNumber("offset"));
    const int direction = (!state_.alternateDirections || strip % 2 == 0 ? 1 : -1);
    const float pixel_offset_factor = (state_.stripGap * strip) + (state_.pixelGap * offset);
    const float time_factor = state_.timeOffset / state_.cycleTime;
    const float cyclePortion = std::fmod(time_factor + pixel_offset_factor * direction, 1);
    hsv2rgb(rgb, cyclePortion, state_.saturation, state_.value);
}

void LED::Effects::RainbowCylinder::postProcess(const Vec3 &rgb, const PixelInfo &) {
    frameCost_ += rgb[0] + rgb[1] + rgb[2];
}

void LED::Effects::RainbowCylinder::beginFrame(const FrameInfo &frame) {
    state_.timeOffset = std::fmod(state_.timeOffset + frame.timeDelta, state_.cycleTime);
    frameCost_ = 0;
}

void LED::Effects::RainbowCylinder::endFrame(const FrameInfo &) {
}

void LED::Effects::RainbowCylinder::debug(const DebugInfo &) {
}
