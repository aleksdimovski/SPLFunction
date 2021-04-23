

features int[0,7] A;

int main() {
    int x;
    int y;
    #if (A<=2) x=[-9,-1]; #else x=[1,9]; #endif;
    while (x > 0) {
        y = 0;
        while (y < x) {
            y = y+1;
        }
        x = x-1;
    }
    
    return 0;
}