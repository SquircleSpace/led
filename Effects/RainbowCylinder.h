#pragma once

#include "Effects/Effect.h"

namespace LED { namespace Effects {
    class RainbowCylinder : public Effect {
    public:
        struct State {
            float cycleTime = 5;
            float pixelGap = 1.0 / 15;
            float stripGap = 1.0 / 10;
            float timeOffset = 0;
            float value = 0.75;
            float saturation = 1;
            bool alternateDirections = true;
        };
    private:
        float frameCost_ = 0;
        State state_;

    public:
        State &getState() {
            return state_;
        }

        const State &getState() const {
            return state_;
        }

        void setState(const State &s) {
            state_ = s;
        }

        using PixelInfo = Effect::PixelInfo;
        using FrameInfo = Effect::FrameInfo;
        using DebugInfo = Effect::DebugInfo;

        void shader(Vec3 &, const PixelInfo &) const override;
        void postProcess(const Vec3 &, const PixelInfo &) override;
        void beginFrame(const FrameInfo &) override;
        void endFrame(const FrameInfo &) override;
        void debug(const DebugInfo &) override;
    };
}}
