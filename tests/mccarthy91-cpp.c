
features int[0,7] A;

int main() {
	int x1, x2=1;
	
  	#if (A<2) x1=[0,100]; #else x1=[101,110]; #endif;	
	
	while (x2 >= 1) {
		if (x1 > 100) {
			x1 = x1 - 10;
			x2 = x2 - 1;
		} else {
			x1 = x1 + 11;
			x2 = x2 + 1;
		}
	}
	return x2;
}