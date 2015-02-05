
#### run the Cardinal Shiny app ####

runCardinal <- function() {
	if ( require(shiny, quietly=TRUE) ) {
		runApp(system.file(package="Cardinal"))
	} else {
		.stop("Package 'shiny' is required. Please install it.")
	}
}
