/*  tacas2013b.c
TERMINATION

suggested parameters:
- partition abstract domain = octagons/polyhedra
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

features int[-10,10] A;

int main() {
  int x, K=A;
  while (x != K) {
    if (x > K) {
      x = x - 1;
    } else {
      x = x + 1;
    }
  }
  return 0;
}