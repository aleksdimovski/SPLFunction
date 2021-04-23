/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
- SMART WIDENING
*/

features int[1,4] A;

void main() {
  int x;
	
  #if (A<=2) x=[-99,-1]; #else x=[0,99]; #endif;		
	
  while (x != 0) {
    if (x > 0)
      x = x - 2;
    else
      x = x + 1;
  }
}
