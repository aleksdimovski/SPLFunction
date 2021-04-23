/* from Podelski & Rybalchenko VMCAI 2004 paper
TERMINATION

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 5
*/

features int[-2,0] A;

void main() {
  int x;
   
	
  while (x >= 0)
    x = A * x + 10;
	
}