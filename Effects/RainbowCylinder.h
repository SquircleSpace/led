#pragma once

#include "Effects/Effect.h"
#include "RainbowCylinderState.pb.h"

namespace LED { namespace Effects {
    class RainbowCylinder : public Effect {
        float frameCost_ = 0;
        float timeOffset_ = 0;
        Proto::RainbowCylinder state_ = defaultState();
        static Proto::RainbowCylinder defaultState();

    public:
        using State = Proto::RainbowCylinder;

        State &state() {
            return state_;
        }

        const State &state() const {
            return state_;
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
