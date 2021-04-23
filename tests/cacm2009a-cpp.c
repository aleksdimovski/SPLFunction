/* from Cook & Podelski & Rybalchenko CACM 2009 paper
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

features int[0,4] A;

void main() {
  int x, y;
	
  #if (A<=2) x=1; #else x=[2,100]; #endif;	
  #if (A>3) x=[-100,0]; #endif;	
	
  while (x > 0 && y > 0)
    if (?) {
      x = x - 1;
      y = y + 1;
    } else
      y = y - 1;
}