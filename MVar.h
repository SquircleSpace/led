#pragma once

#include <mutex>
#include <memory>
#include <cassert>

namespace LED {
    template <typename T>
    class MVar;

    template <typename T, typename... Args>
    MVar<T> make_MVar(Args &&... args) {
        return MVar<T>{std::make_shared<T>(std::forward<Args>(args)...), std::make_shared<std::mutex>()};
    }

    template <typename T>
    using MVarLock = typename MVar<T>::Lock;

    template <typename T>
    class MVar {
        std::shared_ptr<std::mutex> mutex_;
        std::shared_ptr<T> value_;

        template <typename U, typename... Args>
        friend MVar<U> make_MVar(Args &&...);

        friend class Lock;

        template <typename U>
        friend class MVar;

        MVar(std::shared_ptr<T> value, std::shared_ptr<std::mutex> mutex)
          : mutex_(std::move(mutex))
          , value_(std::move(value))
        {
            assert((mutex_ && value_) || (!mutex_ && !value_));
        }

    public:
        class Lock {
            MVar<T> mvar_;
            std::unique_lock<std::mutex> lock_;
        public:
            Lock() = default;

            Lock(MVar<T> &mvar)
              : mvar_(mvar)
            {
                if (mvar_.value_) {
                    lock_ = std::unique_lock<std::mutex>(*mvar_.mutex_);
                }
            }

            operator bool() const {
                return static_cast<bool>(mvar_.value_);
            }

            T &operator*() const {
                return *mvar_.value_;
            }

            std::shared_ptr<T> operator->() const {
                return mvar_.value_;
            }
        };

        template <typename U>
        MVar(MVar<U> &other)
          : mutex_(other.mutex_)
          , value_(other.value_)
        {
        }

        MVar() = default;

        Lock lock() {
            return Lock{*this};
        }

        template <typename Fn>
        void with_lock(Fn fn) {
            Lock lock{*this};
            fn(*lock);
        }
    };
}
