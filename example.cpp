#include "simp.h"


int main(int argc, char *argv[]) {
    auto t = simp::make_tuple(60, -20, 13.f, 23.3, 707, 404,
                              10.f, 100, 0, 8, 7, "10a", 0x10a,
                              888, 998, 4, 5, 2, 10, 999,
                              -12, 50, 40, 30, "Hi");

    t.for_each([](auto e) {
        std::cout << e << std::endl;
    });
}
