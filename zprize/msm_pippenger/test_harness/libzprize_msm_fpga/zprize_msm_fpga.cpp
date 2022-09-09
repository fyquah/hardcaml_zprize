#include <iostream>

extern "C" void testcall_cpp(float value)
{
    std::cout << "Hello, world from C++! Value passed: " << value << std::endl;
}
