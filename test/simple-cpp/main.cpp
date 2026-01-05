#include <iostream>
#include "greeter.hpp"

int main() {
    std::cout << "Testing simple C++ build" << std::endl;

    Greeter greeter("Zenon");
    greeter.greet();

    return 0;
}
