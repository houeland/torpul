#include <iostream>

extern "C" {
double Main();

double Multiply(double left, double right) {
  return left * right;
}

double GreaterThan(double bigger, double smaller) {
  return bigger > smaller ? 1.0 : 0.0;
}
}

int main() {
  std::cout << "Main() output: " << Main() << std::endl;
}
