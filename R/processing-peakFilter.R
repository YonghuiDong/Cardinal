
#### Peak filtering methods ####
## ---------------------------

setMethod("peakFilter", "MSImageSet",
	function(object, method = "freq",
		...,
		pixel = pixels(object),
		plot = FALSE)
	{
		if ( !centroided(object) )
			.stop("peakFilter: 'object' is not centroided. Run 'peakAlign' on it first.")
		fun <- peakFilter.method(method)
		prochistory(processingData(object)) <- .history()
		.message("peakFilter: Using method = ", match.method(method))
		.time.start()
		feature <- featureApply(object, .fun=fun, ..., .use.names=FALSE, .simplify=TRUE)
		object <- object[feature,pixel]
		if ( plot )
			wrap(plot(object, pixel=seq_len(ncol(object)), ...), ..., signature=fun)
		.message("peakFilter: Done.")
		.time.stop()
		object
	})

peakFilter.method <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method, c("freq"))
		method <- switch(method,
			freq = peakFilter.freq,
			match.fun(method))
	}
	match.fun(method)
}

peakFilter.freq <- function(x, freq.min=length(x) / 100, ...) {
	sum(x > 0) > freq.min
}
