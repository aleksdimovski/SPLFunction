/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

features int[0,10] A;

int main() {
  int x;
	
  #if (A<=2) x=[-100,0]; #else x=[1,100]; #endif;
	
  while (x>0) {
    #if (A<3) x=x-1; #else x=x+1; #endif;
  }
  
  return 0;
}
