#include "Effects/Effect.h"

namespace LED { namespace Effects {
    class RainbowCylinder {
        float cycleTime_ = 5;
        float pixelGap_ = 1.0 / 15;
        float stripGap_ = 1.0 / 10;
        float timeOffset_ = 0;
        float frameCost_ = 0;
        float value_ = 0.75;
        float saturation_ = 1;
        bool alternateDirections_ = true;
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
}}
