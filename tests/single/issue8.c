/*
TERMINATION for x + z >= 0 || y >= 1 || -2y + z >= 0 || -x >= 2

suggested parameters:
- partition abstract domain = polyhedra
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

//features int[0,7] A;

int main() {
    int flag, x, y, z;
	int A=[0,7];
    if (A<=2) x=[-10,-2]; else x=[-1,9];
	
    flag = 1;
    while (y < z && flag > 0 && x < -1) {
      if (y > 0) {
        y = -x * y - 1;
      } else {
        flag = 0;
      }
    }
}
