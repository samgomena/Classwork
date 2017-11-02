void ByteMult(unsigned long* a, unsigned long* b, unsigned long*c, long n)
{
	long i=0, j=0, k=0;
	unsigned long sum;
	for (i = 0; i < n; i++)
	{
		long in = i*n;
		for (j = 0; j < n; j++)
		{
			sum = 0;
			for (k = 0; k < n; k++)
			{
				sum = sum + a[in+k] * b[k*n+j];
			}
			c[in+j] = sum;
		}
	}
}
