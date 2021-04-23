/* from Gopan & Reps CAV 2006 paper
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 3
*/

features int[0,3] A;
features int[1,2] B;

void main() {
  int x, y;
	
  #if (A<B) y=50; #else y=55; #endif;	
	
  while (x >= 0) {
    if (y <= 50)
      x = x + 1;
    else
      x = x - 1;
	#if (A<2) y = y + 1; #else y = y - 1; #endif;
    
  }
	
}