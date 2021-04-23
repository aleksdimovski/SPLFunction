/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

features int[0,99] A;
features int[0,99] B;

int main() {
  int x, y=A;
	
//  #if (A<=1) {x=[0,10]; x++; } #else x=[20,100]; #endif;	
	
  while (x <= 10) 
	if (y>B) x = x + 1; else x = x-1; 
	
  return 0;
}