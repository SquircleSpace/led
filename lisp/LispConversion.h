#pragma once

#include <ecl/ecl.h>

namespace LED {
    template <typename T>
    static cl_object toLisp(T);

    template <typename T>
    static T fromLisp_mayError(cl_object);

    template <typename T>
    static T fromLisp(cl_object o) {
        T result;
        cl_env_ptr env = ecl_process_env();
        ECL_CATCH_ALL_BEGIN(env) {
            result = fromLisp_mayError<T>(o);
        } ECL_CATCH_ALL_IF_CAUGHT {
            // We don't need your stinkin' lisp exception.
            throw std::invalid_argument("Failed to convert object");
        } ECL_CATCH_ALL_END;
        return result;
    }

    template <>
    inline cl_object toLisp(float f) {
        return ecl_make_single_float(f);
    }

    template <>
    inline float fromLisp(cl_object o) {
        return ecl_to_float(o);
    }

    template <>
    inline cl_object toLisp(double d) {
        return ecl_make_double_float(d);
    }

    template <>
    inline double fromLisp(cl_object o) {
        return ecl_to_double(o);
    }

    template <>
    inline cl_object toLisp(bool b) {
        return b ? ECL_T : ECL_NIL_SYMBOL;
    }

    template <>
    inline bool fromLisp(cl_object o) {
        return o != ECL_NIL && o != ECL_NIL_SYMBOL;
    }

    template <>
    inline cl_object toLisp(Vec3 v) {
        static const auto type = ecl_make_symbol("SINGLE-FLOAT", "COMMON-LISP");
        auto vector = si_make_vector(type, ecl_make_fixnum(3), ECL_NIL, ECL_NIL, ECL_NIL, ECL_NIL);
        for (int i = 0; i != 3; ++i) {
            ecl_aset_unsafe(vector, i, toLisp(v[i]));
        }
        return vector;
    }

    template <>
    inline Vec3 fromLisp_mayError(cl_object o) {
        static const auto desiredType = ecl_make_symbol("SINGLE-FLOAT", "COMMON-LISP");
        auto elementType = cl_array_element_type(o);
        auto length = ecl_array_dimension(o, 0);
        if (elementType != desiredType || length != 3) {
            throw std::invalid_argument("Bad array");
        }
        Vec3 result;
        for (int i = 0; i != 3; ++i) {
            result[i] = fromLisp<float>(ecl_aref_unsafe(o, i));
        }
        return result;
    }
}
