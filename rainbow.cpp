#include <iostream>
#include <cstdlib>
#include <string>
#include <array>
#include <thread>
#include <numeric>
#include <cmath>
#include <algorithm>

#include "MVar.h"
#include "Effects/EffectRunner.h"
#include "Effects/RainbowCylinder.h"

#include <unistd.h>

struct Pipe {
    int readEnd = -1;
    int writeEnd = -1;
};

Pipe make_pipe() {
    int fds[2];
    const int &writeEnd = fds[1];
    const int &readEnd = fds[0];
    if (0 != pipe(fds)) {
        std::cerr << "Couldn't make a pipe" << std::endl;
        exit(2);
    }
    Pipe result;
    result.readEnd = readEnd;
    result.writeEnd = writeEnd;
    return result;
}

static size_t retry_read(int fd, char *place, size_t count) {
    size_t result = 0;
    while (count > 0) {
        auto read_result = read(fd, place, count);
        if (read_result < 0) {
            if (EINTR == errno) {
                read_result = 0;
            } else {
                std::cerr << "Couldn't read" << std::endl;
                exit(2);
            }
        } else if (read_result == 0) {
            return result;
        }

        result += read_result;
        count -= read_result;
        place += read_result;
    }
    return result;
}

static void lisp_receive_loop(LED::MVar<LED::Effects::RainbowCylinder> rainbow, int fd) {
    (void)rainbow;
    std::cout << "Reading from " << fd << std::endl;
    while (true) {
        unsigned int length = 0;
        for (int i = 3; i >= 0; --i) {
            unsigned char word = 0;
            size_t bytes_read = retry_read(fd, reinterpret_cast<char *>(&word), sizeof(word));
            if (bytes_read != 1) {
                std::cerr << "EOF on control stream" << std::endl;
                exit(2);
            }
            const int wide = word;
            std::cout << "Word length " << wide << std::endl;
            length = length | (wide << (8 * i));
        }
        std::cout << "Byte length " << length << std::endl;
        std::string data(length, '\0');
        char *ptr = &data[0];
        size_t bytes_read = retry_read(fd, ptr, length);
        if (bytes_read != length) {
            std::cerr << "EOF on control stream" << std::endl;
            exit(2);
        }
        LED::Effects::Proto::CompoundEffect effect;
        effect.ParseFromString(data);
        std::cout << effect.DebugString() << std::endl;

        if (effect.has_basic()) {
            auto basic = effect.basic();
            if (basic.has_rainbow()) {
                auto newRainbow = basic.rainbow();
                auto lock = rainbow.lock();
                lock->state().MergeFrom(newRainbow);
                std::cout << "Settings applied" << std::endl;
                std::cout << lock->state().DebugString() << std::endl;
            }
        }
    }
}

static void lisp_control_thread(LED::MVar<LED::Effects::RainbowCylinder> rainbow) {
    const Pipe pipe = make_pipe();
    const pid_t pid = fork();
    if (0 == pid) {
        // child
        close(pipe.readEnd);
        if (3 != pipe.writeEnd) {
            dup2(pipe.writeEnd, 3);
            close(pipe.writeEnd);
        }
        const char * const argv[] = {
            "./lisp/control",
            "--swank",
            "--control-file",
            "/dev/fd/3",
        };
        execvp("./lisp/control", const_cast<char * const *>(argv));
        std::cout << "uh oh" << std::endl;
        exit(2);
    } else if (pid > 0) {
        // parent
        close(pipe.writeEnd);
        lisp_receive_loop(rainbow, pipe.readEnd);
        close(pipe.readEnd);
    } else {
        // error
        std::cerr << "Fork problem" << std::endl;
        exit(2);
    }
}

int main(int argc, char **argv) {
    auto effect = LED::make_MVar<LED::Effects::RainbowCylinder>();
    auto runner = LED::make_MVar<LED::Effects::EffectRunner<LED::Effects::Effect>>();
    runner.lock()->setEffect(effect);
    std::thread t(lisp_control_thread, effect);
    return LED::Effects::main(runner, argc, argv);
}
