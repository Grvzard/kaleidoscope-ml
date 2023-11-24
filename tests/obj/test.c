#include <stdio.h>

extern double add(double a, double b);

int main() {
  printf("42 = %lf!!!", add(40, 2));
  return 0;
}