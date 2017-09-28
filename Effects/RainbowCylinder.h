#pragma once

#include "Effects/Effect.h"
#include "RainbowCylinderState.pb.h"

namespace LED { namespace Effects {
    class RainbowCylinder : public Effect {
        float frameCost_ = 0;
        float timeOffset_ = 0;
        RainbowCylinderState state_ = defaultState();
        static RainbowCylinderState defaultState();

    public:
        using State = RainbowCylinderState;

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
