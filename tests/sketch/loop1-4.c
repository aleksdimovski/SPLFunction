/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

features int[0,15] A;
features int[0,15] B;

int main() {
  int x, y=A;	
	
  while (x <= 10) 
	if (y>B) x = x + 1; else x = x-1; 
	
  return 0;
}