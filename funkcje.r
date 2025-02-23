przeciwprostokatna = function(a, b){
	if (a <= 0 || b <= 0){
		print("złe dane")
	}
	return(sqrt(a^2 + b^2))
}

wektor = function(n){
	if (n <= 0){
		print("złe dane")
	}
	return(1:n)
}

kostka = function(n){
	if (n <= 0){
		print("złe dane")
	}
	return(sample(1:6, n, replace = TRUE))
}