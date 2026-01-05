#include "greeter.hpp"
#include <iostream>

Greeter::Greeter(const std::string& name) : name_(name) {}

void Greeter::greet() const {
    std::cout << get_greeting() << std::endl;
}

std::string Greeter::get_greeting() const {
    return "Hello, " + name_ + "!";
}
