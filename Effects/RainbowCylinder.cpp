#include "Effects/RainbowCylinder.h"

#include <fadecandy/color.h>

LED::Effects::RainbowCylinder::State LED::Effects::RainbowCylinder::defaultState() {
    State state;
    state.set_cycle_time(5);
    state.set_pixel_gap(1.0 / 15);
    state.set_strip_gap(1.0 / 10);
    state.set_value(0.75);
    state.set_saturation(1);
    state.set_alternate_directions(true);
    return state;
}

void LED::Effects::RainbowCylinder::shader(Vec3 &rgb, const PixelInfo &pixel) const {
    const int strip = std::round(pixel.getNumber("strip"));
    const int offset = std::round(pixel.getNumber("offset"));
    const int direction = (!state_.alternate_directions() || strip % 2 == 0 ? 1 : -1);
    const float pixel_offset_factor = (state_.strip_gap() * strip) + (state_.pixel_gap() * offset);
    const float time_factor = timeOffset_ / state_.cycle_time();
    const float cyclePortion = std::fmod(time_factor + pixel_offset_factor * direction, 1);
    hsv2rgb(rgb, cyclePortion, state_.saturation(), state_.value());
}

void LED::Effects::RainbowCylinder::postProcess(const Vec3 &rgb, const PixelInfo &) {
    frameCost_ += rgb[0] + rgb[1] + rgb[2];
}

void LED::Effects::RainbowCylinder::beginFrame(const FrameInfo &frame) {
    timeOffset_ = std::fmod(timeOffset_ + frame.timeDelta, state_.cycle_time());
    frameCost_ = 0;
}

void LED::Effects::RainbowCylinder::endFrame(const FrameInfo &) {
}

void LED::Effects::RainbowCylinder::debug(const DebugInfo &) {
}
