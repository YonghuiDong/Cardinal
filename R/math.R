
# gives the L1 norm of a vector
l1norm <- function(x) sum(abs(x))

# gives the L2 norm of a vector
l2norm <- function(x) sqrt(sum(x^2))

# soft thresholding
soft <- function(x, delta) sign(x) * pmax(0, abs(x)-delta)

# find the combined mean deviation of multiple samples
combinedMean <- function(xbar, n) {
	sum(xbar * n) / sum(n)
}

# find the combined mean deviation of multiple samples
combinedVar <- function(xbar, var, n) {
	xc <- combinedMean(xbar, n)
	(sum(n * var) + sum(n * (xbar - xc)^2)) / sum(n)
}

# find the grouped mean, e.g., of a histogram
groupMean <- function(x, f) {
	if ( any(f < 0) ) f[f < 0] <- 0
	n <- sum(f)
	sum(f * x) / n
}

# find the grouped variance, e.g., of a histogram
groupVar <- function(x, f) {
	if ( any(f < 0) ) f[f < 0] <- 0
	n <- sum(f)
	xbar <- sum(f * x) / n
	Sfx2 <- sum(f * x^2)
	(Sfx2 / n) - xbar^2
}

# find the kurtosis of a vector
kurtosis <- function(x, na.rm=FALSE) {
	if ( na.rm ) x <- x[!is.na(x)]
	n <- length(x)
	n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)
}

# bisection search along a sequence
bisection.seq <- function(x, fun, ..., iter.max=20, epsilon=1e-6) {
	if ( fun(x[[1]], ...) < fun(x[[length(x)]], ...) ) {
		lo <- 1
		hi <- length(x)
		to.lo <- floor
		to.hi <- ceiling
	} else {
		lo <- length(x)
		hi <- 1
		to.lo <- ceiling
		to.hi <- floor
	}
	i <- round((lo + hi) / 2, digits=0)
	iter <- 1
	while ( iter <= iter.max && l2norm(fun(x[[i]], ...)) > epsilon ) {
		if ( fun(x[[i]], ...) > 0 ) {
			hi <- i
			i <- to.lo((lo + hi) / 2)
		} else {
			lo <- i
			i <- to.hi((lo + hi) / 2)
		}
		iter <- iter + 1
	}
	i
}

# bin a signal
bin <- function(x, lbound, ubound, sum=FALSE) {
	x.new <- mapply(function(l, u) {
		sum(x[l:u], na.rm=TRUE)
	}, lbound, ubound)
	if ( sum ) {
		x.new
	} else {
		x.new / abs(ubound - lbound + 1)
	}
}

# returns a list of approximately even subsets of a vector
intervals <- function(x, blocks) {
	ints <- floor(seq(from=1, to=length(x)+1, length.out=blocks))
	begin <- ints[-length(ints)]
	end <- ints[-1] - 1
	apply(cbind(begin, end), 1, function(i) x[i[1]:i[2]])
}
