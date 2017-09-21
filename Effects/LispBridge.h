#include "Effects/Effect.h"
#include "MVar.h"

#include <ecl/ecl.h>

namespace LED { namespace Effects {
    class LispBridge {
        cl_object effect_ = OBJNULL;
        cl_object shader_ = OBJNULL;
        cl_object beginFrame_ = OBJNULL;
        cl_object endFrame_ = OBJNULL;

    public:
        LispBridge();

        using PixelInfo = Effect::PixelInfo;
        using FrameInfo = Effect::FrameInfo;
        using DebugInfo = Effect::DebugInfo;

        void shader(Vec3 &, const PixelInfo &) const {
            cl_funcall(1, shader_);
        }

        void postProcess(const Vec3 &, const PixelInfo &) {
        }

        void beginFrame(const FrameInfo &) {
            cl_funcall(1, beginFrame_);
        }

        void endFrame(const FrameInfo &) {
            cl_funcall(1, endFrame_);
        }

        void debug(const DebugInfo &) {
        }
    };
}}
