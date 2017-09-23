#pragma once

namespace LED {
    class LispEnvironment {
        LispEnvironment();
        ~LispEnvironment();
    public:
        static void ensureLoaded();
    };
}
