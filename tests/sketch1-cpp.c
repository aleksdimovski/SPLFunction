/*
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

features int[0,5] A;

int main() {
  int x;
		
	
  while (x <= 10) {
	if (x>A)
    	x = x + 1;
	else x = x -1; 
  }
	
  return 0;
}