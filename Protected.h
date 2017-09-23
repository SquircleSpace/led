#pragma once

namespace LED {
    template <typename T>
    using ProtectedLock = typename Protected<T>::Lock;

    template <typename T>
    class Protected {
        T value_;
        std::mutex mutex_;
    public:
        class Lock {
            std::unique_lock<std::mutex> lock_;
            T *value_ = nullptr;
        public:
            Lock() = default;

            Lock(Protected &p)
              : lock_(p.mutex_)
              , value_(&p.value_)
            {
            }

            T *get() {
                return value_;
            }

            const T *get() const {
                return value_;
            }

            T &operator *() {
                return *value_;
            }

            const T &operator *() const {
                return *value_;
            }

            T *operator ->() {
                return value_;
            }

            const T *operator ->() const {
                return value_;
            }
        };

        Protected() = default;

        template <typename... Args>
        Protected(Args &&... args)
          : value_(std::forward<Args>(args)...)
        {
        }

        Protected(const Protected &) = delete;
        Protected(Protected &&) = delete;
        Protected &operator=(const Protected &) = delete;
        Protected &operator=(Protected &&) = delete;

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
