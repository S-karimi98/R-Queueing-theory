limitdist <- function(m, n, la, mu) {
	# returns the limit distribution for an M/M/m/m+n queue with
	# arrival rate la, service rate mu, m servers, buffer size n
	# returns x, a vector of length m + n + 1,
	# giving the limiting probs for 0, 1, ..., m + n customers in the system

	la_k <- function(k) {
		# returns the rate at which the number in the system increases from k to k+1
		# 0 <= k <= m + n - 1
		if(k <= m + n - 1) {
			return(la)
		} else {
			return(0)
            }		
	}
	mu_k <- function(k) {
		# returns the rate at which the number in the system decreases from k to k-1
		# 1 <= k <= m + n
		if(k <= m) {
			return(k*mu)
		} else {
			return(m*mu)
		}
	}
	x <- rep(1, m + n + 1)
	for (i in 1:(m+n)) {
		for (j in 1:i) {
			x[i+1] <- x[i+1]*la_k(j-1)/mu_k(j)
		}
	}
	x <- x/sum(x)
	return(x)
}

num = 10
a = limitdist(num,3,60,30)
avg_num_in_sys = c(0,1,2,3,4,5)%*%a
limitdist <- function(m, n, la, mu) {
	# returns the limit distribution for an M/M/m/m+n queue with
	# arrival rate la, service rate mu, m servers, buffer size n
	# returns x, a vector of length m + n + 1,
	# giving the limiting probs for 0, 1, ..., m + n customers in the system

	la_k <- function(k) {
		# returns the rate at which the number in the system increases from k to k+1
		# 0 <= k <= m + n - 1
		if(k <= m + n - 1) {
			return(la)
		} else {
			return(0)
            }		
	}
	mu_k <- function(k) {
		# returns the rate at which the number in the system decreases from k to k-1
		# 1 <= k <= m + n
		if(k <= m) {
			return(k*mu)
		} else {
			return(m*mu)
		}
	}
	x <- rep(1, m + n + 1)
	for (i in 1:(m+n)) {
		for (j in 1:i) {
			x[i+1] <- x[i+1]*la_k(j-1)/mu_k(j)
		}
	}
	x <- x/sum(x)
	return(x)
}

vals = c()
tmp = c()
price = 2
queue = 3
for(i in 1:11)
{
	num = i
	a = limitdist(num,queue,60,30)
	avg_num_in_sys = c(0:(num+queue)) %*% a
	tmp = sum(a[1:num]%*%c(1:num))*60*price
	vals = c(vals,tmp-num*20)
	#print(a[(length(a)-num):(length(a)-1)])
	print(
			paste(i,
				tmp-num*20,				
				round(2*num*avg_num_in_sys-num*20,price)
				)
		)
}
vals
plot(vals)
lines(vals)
