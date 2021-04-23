

features int[0,7] A;
features int[0,7] B;
features int[0,7] C;

int main()
{
    int x, y, oldx;
		#if (A<C) y=[-9,-1]; #else y=[1,9]; #endif;
	while (x >= 0 && y >= 0) {
		oldx = x;
		x = y - 1;
		#if (B<=2) y = oldx - 1; #else y = oldx + 1; #endif;
		
	}
	return 0;
}