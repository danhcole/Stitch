#include "stch_headers.h"


int c(int a)
{
	if (a % 2)
	{
		return 3 * a + 1;
	}
	return a / 2;
	}

int main()
	{
	int x;
	x = 7859;
	while (x != 1) {
		x = c(x);
		printf("%d\n", x);
	}
	return 0;
}
