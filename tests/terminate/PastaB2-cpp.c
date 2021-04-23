

features int[0,15] A;
features int[0,7] B;

int main() {
    int x;
    int y;
    
    while (x > y) {
		#if (A<=2) x = x-1; #else x = x+1; #endif;
        #if (B<=2) y = y+1; #else y = y-1; #endif;
        
    }
    
    return 0;
}