#include <iostream>

extern "C" {
double Main();
}

int main() {
  std::cout << "Main() output: " << Main() << std::endl;
}
