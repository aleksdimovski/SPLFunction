/* from Gopan & Reps CAV 2006 paper
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 3
*/

features int[0,2] A;
features int[1,2] B;

void main() {
  int x, y;
	
  #if (A<B) x=5; #else x=20; #endif;	
	
  while (true) {

    #if (A<2) x = x + 1; #else x = x - 1; #endif;
    
    while (x==10) y=y+1;
    
  }
	
}
