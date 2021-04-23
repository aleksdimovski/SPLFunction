/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

features int[0,5] A;

int main() {
  int x;
	
  #if (A<=2) x=[0,10]; #else x=[20,100]; #endif;	
	
  while (x <= 10) 
    x = x + 1;
  return 0;
}