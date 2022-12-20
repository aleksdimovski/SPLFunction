/* from Urban Miné VMCAI 2015 paper
GUARANTEE/RECURRENCE (x == 0)

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = ordinals 1
- backward widening delay = 2 [default]
*/

features int[0,5] A;

void main() {
  int x;
  while (true) {
    #if (A<=2) x=[-100,0]; #else x=[1,100]; #endif;
    while (x != 0) {
      if (x > 0)
        x = x - 1;
      else
        x = x + 1;
    }
  }
}
