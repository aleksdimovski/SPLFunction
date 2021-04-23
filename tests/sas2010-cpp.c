/* from Alias & Darte & Feautrier & Gonnord SAS 2010 paper
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

features int[1,4] A;

int main() {
  int x1, x2;
	
  #if (A<=2) x2=10; #else x2=[11,99]; #endif;	
  #if (A>3) x2=[-99,9]; #endif;	
	
  while (x1 >= 0 && x2 >= 0) {
    if (?) {
      while (x2 <= 10 && ?) {
        x2 = x2 + 1;
      }
      x1 = x1 - 1;
    }
    x2 = x2 - 1;
  }
  return 0;
}