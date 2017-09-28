#pragma once

#include <string>

#include <ecl/ecl.h>

namespace LED { namespace lisp {
    class Binding {
        cl_env_ptr env_;
        Binding(cl_object, cl_object);
        ~Binding();

    public:
        template <typename Fn>
        static auto with(cl_object symbol, cl_object value, Fn fn) {
            Binding binding(symbol, value);
            return fn();
        }

        Binding(const Binding &) = delete;
        Binding(Binding &&) = delete;
        Binding &operator=(const Binding &) = delete;
        Binding &operator=(Binding &&) = delete;
    };
}}
