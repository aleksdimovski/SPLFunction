// FuncTion arguments:
// -ctl "AF{AG{y > 0}}" 

features int[0,10] A;

int main() {
    int x;
    #if (A<5) x=[-100,-1]; #else x=[0,100]; #endif;
    
    while (x>=0) {
    	x = x + 1; 
    }

    while (true) {        
        if (x<=10) x = x + 1;
        else x = -x; 
    }
}
