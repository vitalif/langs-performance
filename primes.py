import os
import sys
import time
import platform

def get_primes7(n):
	"""
	standard optimized sieve algorithm to get a list of prime numbers
	--- this is the function to compare your functions against! ---
	"""
	if n < 2:
		return []
	if n == 2:
		return [2]
	# do only odd numbers starting at 3
	if sys.version_info.major <= 2:
		s = range(3, n + 1, 2)
	else:  # Python 3
		s = list(range(3, n + 1, 2))
	# n**0.5 simpler than math.sqr(n)
	mroot = n ** 0.5
	half = len(s)
	i = 0
	m = 3
	while m <= mroot:
		if s[i]:
			j = (m * m - 3) // 2  # int div
			s[j] = 0
			while j < half:
				s[j] = 0
				j += m
		i = i + 1
		m = 2 * i + 3
	return [2] + [x for x in s if x]

start_time = time.time()
period_time = int(os.environ.get('RUN_TIME', '5'))
iterations = 0

while (time.time() - start_time) < period_time or iterations < 3:
	res = get_primes7(10000000)
	print("Found {} prime numbers.".format(len(res)))
	iterations = iterations + 1

result_time = time.time() - start_time

print("%s %d.%d.%d: %d iterations in %.02f sec = %.02f sec per 30 iterations" % (
	platform.python_implementation(), sys.version_info.major, sys.version_info.minor, sys.version_info.micro,
	iterations, result_time, result_time/iterations*30))
