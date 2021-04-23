/* from Urban Miné SAS 2014 paper
TERMINATION

suggested parameters:
- partition abstract domain = polyhedra
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

features int[0,15] A;

void main() {
  int x, y;
  while (x > 0 && y > 0) {
	#if (A<=3) x = x - y; #else x = x + y; #endif;
    
  }
}