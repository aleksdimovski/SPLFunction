/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

//features int[0,5] A;

int main() {
  int x,y,r;
	
//  #if (A<=2) x=[0,10]; #else x=[20,100]; #endif;	
	
  while (r > 0) {
    r = r + x;
	r = r - y; 
  }
  return 0;
}