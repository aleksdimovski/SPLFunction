

features int[0,7] A;
features int[0,7] B;

int main()
{
    int x, y, oldx;
		#if (A<=2) y=[-9,-1]; #else y=[1,9]; #endif;
	while (x >= 0 && y >= 0) {
		oldx = x;
		x = y - 1;
		#if (A<=2) y = oldx - 1; #else y = oldx + 1; #endif;
		
	}
	return 0;
}