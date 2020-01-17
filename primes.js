function get_primes7(n) {
	if (n < 2) { return []; }
	if (n == 2) { return [2]; }

	var s = [];
	for (var i = 3; i < n + 1; i += 2) {
		s.push(i);
	}

	var mroot = Math.floor(Math.sqrt(n));
	var half = s.length;
	var i = 0;
	var m = 3;

	while (m <= mroot) {
		if (s[i]) {
			var j = Math.floor((m*m-3)/2);   // int div
			s[j] = 0;
			while (j < half) {
				s[j] = 0;
				j += m;
			}
		}
		i = i + 1;
		m = 2*i + 3;
	}

	// due to a bug in node.js 4.3, we need to declare and init on separate lines
	// or else node.js performs about four times slower
	var res = [];
	res.push(2);

	for (var x = 0; x < s.length; x++) {
		if (s[x]) {
			res.push(s[x]);
		}
	}
	return res;
}

var startTime = Date.now();
var periodTime = parseInt(process.env.RUN_TIME || '5', 10) * 1000;
var iterations = 0;
while ((Date.now() - startTime) < periodTime || iterations < 3) {
	var res = get_primes7(10000000);
	console.log("Found " + res.length + " prime numbers.");
	iterations++;
}
var time = Date.now() - startTime;
var per30 = time / 1000 / iterations * 30;
console.log(iterations + " iterations in "+(Math.round(time/10)/100)+" seconds = "+(Math.round(per30*100)/100)+" seconds per 30 iterations");
