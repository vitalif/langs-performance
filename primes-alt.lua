function get_primes7(n)
	if n < 2 then return {} end
	if n == 2 then return {2} end

	local s = {}
	local c = 1
	for i = 3, n, 2 do
		s[c] = i
		c = c + 1
	end

	local mroot = math.floor(math.sqrt(n))
	local half = #s
	local i = 0
	local m = 3

	while m <= mroot do
		if s[i+1] and s[i+1] ~= 0 then
			local j = math.floor((m*m-3)/2) -- int div
			s[j+1] = 0
			while j < half do
				s[j+1] = 0
				j = j + m
			end
		end
		i = i + 1
		m = 2*i + 3
	end

	local res = {}
	res[1] = 2

	local c = 2
	for x = 1, #s do
		if s[x] ~= 0 then
			res[c] = s[x]
			c = c + 1
		end
	end
	return res
end

for i = 1, 30 do
	local res = get_primes7(10000000)
	print("Found " .. #res .. " prime numbers.")
end
