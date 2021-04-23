/* 
TERMINATION

suggested parameters:
- partition abstract domain = polyhedra
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
- SMART WIDENING
*/

features int[1,4] A;

void main() {
	

  int y, z;
	
  #if (A<=2) z=1; #else z=[2,99]; #endif;	
  #if (A>3) z=[-99,0]; #endif;		
	
  while (y >= 0 || z >= 0) {
    y--;
    z--;
  }
}