/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

//features int[0,15] A;

int main() {
  int x;
  int A=[0,15];
	
  if (A<=2) x=[-100,0]; else x=[1,100];
	
  while (x) {
    if (x > 0) {
      x = x - 1;
    } else {
      x = x + 1;
    }
  }
  return 0;
}
