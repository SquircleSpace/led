#pragma once

#include <mutex>

namespace LED {
    template <typename T>
    class MVar;

    template <typename T, typename... Args>
    MVar<T> make_MVar(Args &&... args) {
        return MVar<T>{std::make_shared<typename MVar<T>::Allocation>(std::forward<Args>(args)...)};
    }

    template <typename T>
    using MVarLock = typename MVar<T>::Lock;

    template <typename T>
    class MVar {
        struct Allocation {
            friend class Lock;

            std::mutex mutex_;
            T value_;

            std::unique_lock<std::mutex> lock() {
                return std::unique_lock<std::mutex>{mutex_};
            }

            Allocation(const Allocation &) = delete;
            Allocation(Allocation &&) = delete;
            Allocation &operator=(const Allocation &) = delete;
            Allocation &operator=(Allocation &&) = delete;

            template <typename... Args>
            Allocation(Args &&... args)
              : value_(std::forward(args)...)
            {
            }
        };

        std::shared_ptr<Allocation> allocation_;

        template <typename U, typename... Args>
        friend MVar<U> make_MVar(Args &&...);

        friend class Lock;

        MVar(std::shared_ptr<Allocation> allocation)
          : allocation_(std::move(allocation))
        {
        }

    public:
        class Lock {
            std::shared_ptr<typename MVar<T>::Allocation> allocation_;
            std::unique_lock<std::mutex> lock_;
        public:
            Lock() = default;

            Lock(MVar<T> &mvar)
                : allocation_(mvar.allocation_)
                {
                    if (allocation_) {
                        lock_ = allocation_->lock();
                    }
                }

            operator bool() const {
                return static_cast<bool>(allocation_);
            }

            T &operator*() const {
                return allocation_->value_;
            }

            T *operator->() const {
                return &allocation_->value_;
            }
        };

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
