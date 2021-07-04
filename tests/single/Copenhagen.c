

features int[0,7] A;
features int[0,7] B;
features int[0,7] C;

int main()
{
    int x, y, oldx;
	int A=[0,7], B=[0,7], C=[0,7];
	
	if (A<C) y=[-9,-1]; else y=[1,9];
	
	while (x >= 0 && y >= 0) {
		oldx = x;
		x = y - 1;
		if (B<=2) y = oldx - 1; else y = oldx + 1;
		
	}
	return 0;
}