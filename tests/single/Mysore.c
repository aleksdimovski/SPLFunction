
//features int[0,15] A;

int main()
{
    int c, x;
	int A=[0,15];
	
	if (A<=2) x=[-9,0]; else x=[1,9];
	if (c >= 2) {
	    while (x + c >= 0) {
		    x = x - c;
		    c = c + 1;
	    }
    }
	return 0;
}