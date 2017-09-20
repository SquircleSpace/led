#pragma once

#include <fadecandy/effect_runner.h>

#include <cassert>
#include <memory>

#include "Effect.h"
#include "MVar.h"

namespace LED { namespace Effects {
    template <typename Effect_t>
    class EffectRunner : private ::EffectRunner {
        using Super = ::EffectRunner;

        class MVarEffectAdapter : public Effect {
            MVar<Effect_t> effect_;
            MVarLock<Effect_t> lock_;
        public:
            MVarEffectAdapter(MVar<Effect_t> effect)
              : effect_(std::move(effect))
            {
            }

            MVarEffectAdapter() = default;

            MVar<Effect_t> getEffect() const {
                return effect_;
            }

            void shader(Vec3 &rgb, const PixelInfo &p) const override {
                assert(lock_);
                const Effect_t &effect = *lock_;
                effect.shader(rgb, p);
            }

            void postProcess(const Vec3 &rgb, const PixelInfo &p) override {
                assert(lock_);
                lock_->postProcess(rgb, p);
            }

            void beginFrame(const FrameInfo &f) override {
                assert(!lock_);
                lock_ = effect_.lock();
                lock_->beginFrame(f);
            }

            void endFrame(const FrameInfo &f) override {
                assert(lock_);
                lock_->endFrame(f);
                lock_ = MVarLock<Effect_t>{};
            }

            void debug(const DebugInfo &d) override {
                assert(!lock_);
                // EffectRunner calls this after endFrame, so we need to
                // temporarily aquire the lock.
                effect_.lock()->debug(d);
            }
        };

        MVarEffectAdapter effect_;

    public:
        using Super::setServer;
        using Super::setLayout;
        void setEffect(MVar<Effect_t> e) {
            effect_ = MVarEffectAdapter{std::move(e)};
            Super::setEffect(&effect_);
        }
        using Super::setMaxFrameRate;
        using Super::setVerbose;

        using Super::hasLayout;
        using Super::getLayout;
        std::shared_ptr<Effect_t> getEffect() const {
            return effect_;
        };
        using Super::isVerbose;
        using Super::getClient;

        using Super::getPixelInfo;
        using Super::getPixel;
        using Super::getPixelColor;

        using Super::getFrameRate;
        using Super::getTimePerFrame;
        using Super::getBusyTimePerFrame;
        using Super::getIdleTimePerFrame;
        using Super::getPercentBusy;

        using Super::FrameStatus;

        using Super::doFrame;

        using Super::run;

        using Super::parseArguments;
        using Super::main;
    };
}}
