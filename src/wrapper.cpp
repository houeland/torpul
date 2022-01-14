#include <iostream>

extern "C" {
double Main();

double Multiply(double left, double right) {
  return left * right;
}

double GreaterThan(double bigger, double smaller) {
  return bigger > smaller ? 1.0 : 0.0;
}

double Print(double value) {
  std::cout << "Print(" << value << ")" << std::endl;
  return -123.4;
}

double Print256(double value) {
  std::cout << "Print256(" << value << ")" << std::endl;
  return -345.6;
}

double DoNothing() {
  return -234.5;
}
}

int main() {
  const auto output = Main();
  std::cout << "Main() output: " << output << std::endl;
}
