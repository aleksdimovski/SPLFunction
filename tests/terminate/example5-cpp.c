/*
TERMINATION for x > 0

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 5
*/

features int[0,15] A;
features int[0,15] B;

int main() {
  int x;

  #if (A<=B+2) x=[3,9]; #else x=[-5,2]; #endif;	
	
	while (x < 10)
    	x = 2 * x;
  return 0;
}