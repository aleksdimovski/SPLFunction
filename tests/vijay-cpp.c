/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
- conflict-driven analysis = 1
*/

features int[0,9] A;

int main() {
  int x, y;
  #if (A < 0 && A % 2 == 0) x = 2; #else #if (A < 0) x = 1; #else #if (A >= 0 && A % 2 == 0) x = -2; #else x = -1; #endif #endif #endif
	
	
  while (y < -2 || y > 2) {
    y = y + x;
  }
  return 0;
}