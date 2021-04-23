/* from Velroyen & Rummer TAP 2008 paper
TERMINATION for x even

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

features int[1,4] A;

int main() {
  int x;
	
  #if (A<=2) x=[0,2]; #else x=4; #endif;	
  #if (A>3) x=[5,99]; #endif;	
	
  while (x != 0) {
  	if (x < 0) {
  		x = x + 2;
  		if (x < 0) {
  			x = -x;			
  		}
  	} else {
  		x = x - 2;
  		if (x > 0) {
  			x = -x;
  		}
  	}
  }
  return 0;
}