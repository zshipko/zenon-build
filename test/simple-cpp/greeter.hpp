#ifndef GREETER_HPP
#define GREETER_HPP

#include <string>

class Greeter {
public:
    Greeter(const std::string& name);
    void greet() const;
    std::string get_greeting() const;

private:
    std::string name_;
};

#endif
