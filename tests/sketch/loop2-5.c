/*
TERMINATION for x > 6

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

features int[0,31] A;
features int[0,31] B;

int main() {
  int x=A;
  while (x >= 0) {
    if (x < B) {
      x = x - 2;
    }
  }
  return 0;
}