/*
TERMINATION for x > 6

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

features int[0,10] A;
features int[0,10] B;

int main() {
  int x=A;
  while (x <= 10) {
    if (x > B) {
      x = x + 2;
    }
  }
  return 0;
}