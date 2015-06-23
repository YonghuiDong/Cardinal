
require(shiny)
require(Cardinal)

library(CardinalWorkflows)
data(pig206)

shinyServer(function(input, output, session) {


	######################################################
	##-------------------- Set up ----------------------##
	######################################################

	#### Progress bar and message functions ####
	##------------------------------------------
	
	shinyMessage <- function(...) {
		incProgress(amount=0, message=paste(...))
		Sys.sleep(1)
	}

	shinyProgressStart <- function(..., min=0, max=1) {
		incProgress(amount=0, message=paste(...))
	}

	shinyProgressIncrement <- function() {
		i <- Cardinal:::.Cardinal$progress$i
		n <- Cardinal:::.Cardinal$progress$max
		incProgress(amount=1 / n, message=paste("Processing", i, "of", n))
	}

	shinyProgressStop <- function(...) {
		incProgress(amount=0, message=paste(...))
	}

	shinyActive <- function(active=TRUE) {
		if ( active ) {
			Cardinal:::.create.message("shiny",
				message=shinyMessage)
			Cardinal:::.create.progress("shiny",
				start=shinyProgressStart,
				increment=shinyProgressIncrement,
				stop=shinyProgressStop)
		} else {
			Cardinal:::.destroy.message("shiny")
			Cardinal:::.destroy.progress("shiny")
		}
	}

	##########################################################
	##-------------------- Main Panel ----------------------##
	##########################################################


	#### Set up reactive values needed for server ####
	##------------------------------------------------

	# IonImage and MassSpectrum modes = {
	#	explore, select, done-selecting,
	#	preview-region, preview-annotation
	# }

	modal <- reactiveValues(
		mz=NA, # save m/z values during modal interaction
		x=NA, y=NA, # save coordinates during modal interaction
		Dataset=NULL, # *switch* datasets
		IonImage="explore", # current ion image mode
		MassSpectrum="explore", # current mass spectrum mode
		Process="none") # current signal pre-processing *preview*

	ranges <- reactiveValues(
		IonImage=list(xlim=c(NA,NA), ylim=c(NA,NA), zlim=c(NA,NA)),
		MassSpectrum=list(xlim=c(NA,NA), ylim=c(NA,NA))
	)

	trigger <- reactiveValues(
		PixelData=0, # when the pixelData has changed
		FeatureData=0, # when the featureData has changed
		IonImage=0, # when ion image needs updating
		MassSpectrum=0) # when mass spectrum needs updating

	feature_0 <- reactive({
		if ( !is.na(input$mz_0) && !is.null(object_0()) ) {
			features(object_0(), mz=input$mz_0)
		} else {
			numeric()
		}
	})

	pixel_0 <- reactive({
		if ( !is.na(input$x_0) && !is.na(input$y_0) && !is.null(object_0()) ) {
			pixels(object_0(), x=input$x_0, y=input$y_0, sample=input$Sample_0)
		} else {
			numeric()
		}
	})

	subset_0 <- reactive({
		if ( input$Sample_0 != "<None>" && !is.null(object_0()) ) {
			pData(object_0())$sample %in% input$Sample_0	
		} else {
			logical()
		}
	})

	coord_0 <- reactive({
		if ( !is.na(input$x_0) && !is.na(input$y_0) ) {
			substitute(list(italic(x) == xx, italic(y) == yy),
				env=list(xx=input$x_0, yy=input$y_0))
		} else {
			character()
		}
	})

	#### Main Dataset, Ion Image, and Mass Spectrum ####
	##--------------------------------------------------

	## create main dataset object getter and dataset selection

	output$Dataset_0 <- renderUI({
		choices <- unlist(eapply(globalenv(), is, "MSImageSet"))
		choices <- c("<None>", sort(names(choices)[choices]))
		selectInput("Dataset_0", "Dataset", choices=choices,
			selected=modal$Dataset)
	})

	object_0 <- function() {
		tryCatch(get(input$Dataset_0), error=function(e) NULL)
	}

	## observe dataset selection and reset inputs on loading new dataset

	observe({
		obj <- tryCatch(get(input$Dataset_0), error=function(e) NULL)
		if ( is.null(obj) ) {
			updateNumericInput(session, "mz_0", value=NA)
			updateNumericInput(session, "x_0", value=NA)
			updateNumericInput(session, "y_0", value=NA)
			updateSelectInput(session, "Sample_0", choices="<None>")
			updateNumericInput(session, "IonImageZoom_0", value=NA)
			ranges$IonImage <- list(
				xlim=c(NA,NA),
				ylim=c(NA,NA),
				zlim=c(NA,NA))
			ranges$MassSpectrum <- list(
				xlim=c(NA,NA),
				ylim=c(NA,NA))
		} else {
			updateNumericInput(session, "mz_0", value=mz(obj)[1])
			updateNumericInput(session, "x_0", value=coord(obj)[1,"x"])
			updateNumericInput(session, "y_0", value=coord(obj)[1,"y"])
			updateSelectInput(session, "Sample_0", choices=sampleNames(obj))
			updateNumericInput(session, "IonImageZoom_0", value=100)
			ranges$IonImage <- list(
				xlim=range(coord(obj)[["x"]]),
				ylim=range(coord(obj)[["y"]]),
				zlim=range(spectra(obj)[1,
					pData(obj)$sample %in% sampleNames(obj)[1]]), na.rm=TRUE)
			ranges$MassSpectrum <- list(
				xlim=range(mz(obj)),
				ylim=range(spectra(obj)[1,
					pData(obj)$sample %in% sampleNames(obj)[1]]), na.rm=TRUE)
		}
	})

	## observe and set basic parameters for the ion image plotting region

	observe({
		if ( !is.na(input$IonImageZoom_0) && !is.null(isolate(object_0())) ) {
			zoom <- input$IonImageZoom_0 / 100
			xrange <- range(coord(isolate(object_0()))[["x"]])
			xlim <- c(input$x_0 + (xrange[1] - input$x_0) / zoom,
				input$x_0 + (xrange[2] - input$x_0) / zoom)
			isolate(ranges$IonImage$xlim <- xlim)
		}
	})

	observe({
		if ( !is.na(input$IonImageZoom_0) && !is.null(isolate(object_0())) ) {
			zoom <- input$IonImageZoom_0 / 100
			yrange <- range(coord(isolate(object_0()))[["y"]])
			ylim <- c(input$y_0 + (yrange[1] - input$y_0) / zoom,
				input$y_0 + (yrange[2] - input$y_0) / zoom)
			isolate(ranges$IonImage$ylim <- ylim)
		}
	})

	observe({
		if ( !is.na(input$mz_0) && !is.null(isolate(object_0())) ) {
			zrange <- range(spectra(isolate(object_0()))[isolate(feature_0()),isolate(subset_0())], na.rm=TRUE)
			zlim <- diff(zrange) * input$ColorRegionsRange / 100 + min(zrange)
			isolate(ranges$IonImage$zlim <- zlim)
		}
	})

	## observe and set basic parameters for the mass spectrum plotting region

	observe({
		if ( length(pixel_0()) > 0 && !any(is.na(ranges$MassSpectrum$xlim)) && !is.null(isolate(object_0())) ) {
			features <- features(isolate(object_0()), mz=ranges$MassSpectrum$xlim)
			if ( input$AutoscaleIntensityRange ) {
				ylim <- range(spectra(isolate(object_0()))[features[1]:features[2],pixel_0()], na.rm=TRUE)
				isolate(ranges$MassSpectrum$ylim <- ylim)
			} else {
				ylim <- range(spectra(isolate(object_0()))[,pixel_0()], na.rm=TRUE)
				isolate(ranges$MassSpectrum$ylim <- ylim)
			}
		}
	})

	observe({
		if ( length(pixel_0()) > 0 && !is.null(input$MassRange_0) && !is.null(isolate(object_0())) ) {
			xlim <- input$MassRange_0
			isolate(ranges$MassSpectrum$xlim <- xlim)
		}
	})

	#### Plot and Observe the Ion Image ####
	##--------------------------------------

	plotNull <- function() {
		plot(0, 0, type='n', xlab="", ylab="", xaxt='n', yaxt='n')
		text(0, 0, "Nothing to plot.")
	}

	plotIonImage_0 <- function(...) {
		image(isolate(object_0()), mz=input$mz_0,
			main=substitute(italic(m/z) == mz, list(mz=input$mz_0)),
			xlim=ranges$IonImage$xlim,
			ylim=ranges$IonImage$ylim,
			zlim=ranges$IonImage$zlim,
			strip=FALSE,
			contrast.enhance=input$ContrastEnhance,
			smooth.image=input$SmoothImage,
			colorkey=input$ShowColorkey,
			col.regions=match.fun(input$ColorRegions)(100),
			subset=subset_0(),
			...)
	}

	plotIonImageGroups_0 <- function(groups, varLabel=NULL, ...) {
		image(isolate(object_0()), groups ~ x * y,
			strip=FALSE,
			colorkey=FALSE,
			col.regions=rainbow(nlevels(groups), alpha=0.75),
			subset=subset_0(),
			add=TRUE,
			...)
		legend("topleft",
			title=varLabel,
			legend=levels(groups),
			fill=rainbow(nlevels(groups)),
			bg=rgb(1, 1, 1, 0.75))
	}

	output$IonImage_0 <- renderPlot({
		trigger$IonImage
		par(mar=c(3,3,3,1), mgp=c(1.5,0.5,0), cex.axis=1, cex.lab=1)
		if ( is.na(input$mz_0) || is.null(isolate(object_0())) ) {
			plotNull()	
		} else {
			plotIonImage_0()
			if ( isolate(modal$IonImage == "select") ) {
				box(col="red", lwd=2)
				mtext("Click to Select a Region", col="blue")
				points(modal$x, modal$y, pch=20, col="white")
				lines(modal$x, modal$y, lwd=1.5, col="white")
				points(tail(modal$x, 1), tail(modal$y, 1), lwd=4, cex=1.5, col="black")
				points(tail(modal$x, 1), tail(modal$y, 1), lwd=2, cex=1.5, col="white")
			} else if ( isolate(modal$IonImage == "preview-region") ) {
				regions <- isolate(input$SelectIonImageRegions)
				if ( !is.null(regions) && all(regions != "") ) {
					regions <- as.list(pData(isolate(object_0()))[regions])
					annotation <- do.call(Cardinal:::make.annotation, regions)
					plotIonImageGroups_0(annotation)
				}
				isolate(modal$IonImage <- "explore")
			} else if ( isolate(modal$IonImage == "preview-annotation") ) {
				name <- isolate(input$SelectIonImageAnnotations)
				if ( !is.null(name) && name != "" ) {
					annotation <- pData(isolate(object_0()))[[name]]
					plotIonImageGroups_0(annotation, varLabel=name)
				}
				isolate(modal$IonImage <- "explore")
			} else {
				points(input$x_0, input$y_0, pch=4, lwd=4, cex=2, col="black")
				points(input$x_0, input$y_0, pch=4, lwd=2, cex=2, col="white")
			}
		}
	}, bg="transparent")

	observe({
		x <- input$IonImageClick_0$x
		y <- input$IonImageClick_0$y
		if ( !is.null(x) && !is.null(y) && !is.null(isolate(object_0())) ) {
			if ( isolate(modal$IonImage == "select") ) {
				isolate(modal$x <- c(modal$x, x))
				isolate(modal$y <- c(modal$y, y))
				diff <- isolate(sqrt((modal$x - x)^2 + (modal$y - y)^2))
				coord <- coord(isolate(object_0()))
				tol <- 0.02 * sqrt(diff(range(coord$x)) * diff(range(coord$y)))
				if ( length(isolate(modal$x)) > 2 && diff <= tol )
					isolate(modal$IonImage <- "done-selecting")
			} else {
				updateNumericInput(session, "x_0", value=round(x, digits=0))
				updateNumericInput(session, "y_0", value=round(y, digits=0))
			}
		}
	})

	observe({
		if ( !is.null(input$IonImageDblClick_0) && !is.null(isolate(object_0())))
			updateNumericInput(session, "IonImageZoom_0", value=100)
	})

	observe({
		xmin <- input$IonImageBrush_0$xmin
		xmax <- input$IonImageBrush_0$xmax
		ymin <- input$IonImageBrush_0$ymin
		ymax <- input$IonImageBrush_0$ymax
		if ( !is.null(xmin) && !is.null(xmax) && !is.null(ymin) && !is.null(ymax) ) {
			isolate(ranges$IonImage$xlim <- c(xmin, xmax))
			isolate(ranges$IonImage$ylim <- c(ymin, ymax))
			updateNumericInput(session, "IonImageZoom_0", value=NA)
		}
	})

	#### Plot the Mass Spectrum ####
	##------------------------------

	plotMassSpectrum_0 <- function(...) {
		plot(isolate(object_0()), coord=c(x=input$x_0, y=input$y_0, sample=input$Sample_0),
			main=coord_0(),
			xlim=ranges$MassSpectrum$xlim,
			ylim=ranges$MassSpectrum$ylim,
			strip=FALSE,
			col=if (is.null(input$Color_0)) "black" else input$Color_0,
			...)
	}

	plotMassSpectrumGroups_0 <- function(groups, varLabel=NULL, ...) {
		plot(isolate(object_0()), coord=c(x=input$x_0, y=input$y_0, sample=input$Sample_0),
			main=coord_0(),
			xlim=ranges$MassSpectrum$xlim,
			ylim=ranges$MassSpectrum$ylim,
			strip=FALSE,
			groups=force(groups),
			col=rainbow(nlevels(groups)),
			...)
		legend("topleft",
			title=varLabel,
			legend=levels(groups),
			fill=rainbow(nlevels(groups)),
			bg=rgb(1, 1, 1, 0.75))
	}

	output$MassSpectrum_0 <- renderPlot({
		trigger$MassSpectrum
		par(mar=c(3,3,3,1), mgp=c(1.5,0.5,0), cex.axis=1, cex.lab=1)
		if ( is.na(input$x_0) || is.na(input$y_0) || is.null(isolate(object_0())) ) {
			plotNull()
		} else if ( length(isolate(pixel_0())) == 0 ) {
			plotNull()
		} else if ( isolate(modal$Process != "none") ) {
			input$x_0; input$y_0; input$Sample_0; input$Color_0 # take dependencies
			if ( isolate(modal$Process == "normalize") ) {
				thecall <- isolate(input$NormalizeCall)
			} else if ( isolate(modal$Process == "smooth-signal") ) {
				thecall <- isolate(input$SmoothSignalCall)
			} else if ( isolate(modal$Process == "reduce-baseline") ) {
				thecall <- isolate(input$ReduceBaselineCall)
			} else if ( isolate(modal$Process == "peak-pick") ) {
				thecall <- isolate(input$PeakPickCall)
			} else if ( isolate(modal$Process == "peak-align") ) {
				thecall <- isolate(input$PeakAlignCall)
			} else if ( isolate(modal$Process == "peak-filter") ) {
				thecall <- isolate(input$PeakFilterCall)
			} else if ( isolate(modal$Process == "reduce-dimension") ) {
				thecall <- isolate(input$ReduceDimensionCall)
			}
			cl <- parse(text=thecall)[[1]]
			cl[["pixel"]] <- pixel_0()
			cl[["xlim"]] <- ranges$MassSpectrum$xlim
			cl[["ylim"]] <- ranges$MassSpectrum$ylim
			cl[["plot"]] <- TRUE
			isolate(modal$Process <- "none")
			eval(cl, envir=globalenv())
			title(main=coord_0())
			mtext(thecall, col="blue")
			box(col="blue", lwd=2)
		} else {
			plotMassSpectrum_0()
			if ( isolate(modal$MassSpectrum == "select") ) {
				box(col="red", lwd=2)
				mtext("Click to Select a Region", col="blue")
				abline(v=modal$mz, lty=2, lwd=1.5, col="green")
			} else if ( isolate(modal$MassSpectrum == "preview-region") ) {
				regions <- isolate(input$SelectMassSpectrumRegions)
				if ( !is.null(regions) && all(regions != "") ) {
					regions <- as.list(fData(isolate(object_0()))[regions])
					annotation <- do.call(Cardinal:::make.annotation, regions)
					plotMassSpectrumGroups_0(annotation)
				}
				isolate(modal$MassSpectrum <- "explore")
			} else if ( isolate(modal$MassSpectrum == "preview-annotation") ) {
				name <- isolate(input$SelectMassSpectrumAnnotations)
				if ( !is.null(name) && all(name != "") ) {
					annotation <- fData(isolate(object_0()))[[name]]
					plotMassSpectrumGroups_0(annotation, varLabel=name)
				}
				isolate(modal$MassSpectrum <- "explore")
			} else {
				abline(v=input$mz_0, lty=2, lwd=2, col="blue")
			}
		}
	}, bg="transparent")

	output$MassRange_0 <- renderUI({
		if ( is.null(object_0()) ) {
			sliderInput("MassRange_0", label="Mass Range",
				min=0, max=100, value=c(0,100), step=1, width="100%")
		} else {
			sliderInput("MassRange_0", label="Mass Range",
				min=floor(min(mz(object_0()))),
				max=ceiling(max(mz(object_0()))),
				value=range(mz(object_0())),
				step=1,
				width="100%")
		}
	})

	observe({
		mz <- input$MassSpectrumClick_0$x
		if ( !is.null(mz) && !is.null(isolate(object_0())) ) {
			if ( isolate(modal$MassSpectrum == "select") ) {
				isolate(modal$mz <- c(modal$mz, mz))
				if ( length(isolate(modal$mz)) >= 2 )
					isolate(modal$MassSpectrum <- "done-selecting")
			} else {
				feature <- features(isolate(object_0()), mz=mz)
				mz <- mz(isolate(object_0()))[feature]
				updateNumericInput(session, "mz_0", value=mz)	
			}
		}
	})

	observe({
		if ( !is.null(input$MassSpectrumDblClick_0) && !is.null(isolate(object_0())) )
			updateSliderInput(session, "MassRange_0", value=range(mz(isolate(object_0()))))
	})

	observe({
		xmin <- input$MassSpectrumBrush_0$xmin
		xmax <- input$MassSpectrumBrush_0$xmax
		if ( !is.null(xmin) && !is.null(xmax) )
			updateSliderInput(session, "MassRange_0", value=c(xmin, xmax))
	})


	##########################################################
	##-------------------- Explore Tab ---------------------##
	##########################################################


	#### Ion Image Display Options ####
	##---------------------------------

	output$ColorRegions <- renderPlot({
		par(mar=c(0,0,0,0), bty='n', xaxt='n', yaxt='n')
		image(matrix(1:100, ncol=1), col=match.fun(input$ColorRegions)(100))
	})

	#### Mass Spectrum Display Options ####
	##-------------------------------------

	output$Color <- renderUI({
		selectInput("Color_0", "Color", choices=colors(), selected="black")
	})

	#### Select Ion Image Regions ####
	##--------------------------------

	getIonImageRegions <- function(object) {
		choices <- sapply(pData(object), is.logical)
		names(choices)[choices]
	}

	output$SelectIonImageRegions <- renderUI({
		trigger$PixelData
		if ( is.null(object_0()) ) {
			selectizeInput("SelectIonImageRegions", "Select a region", choices=NULL)
		} else {
			selectizeInput("SelectIonImageRegions", "Select a region",
				choices=getIonImageRegions(object_0()),
				multiple=TRUE, options=list(create=TRUE))
		}
	})

	observe({
		obj <- object_0()
		if ( !is.null(obj) ) {
			regions1 <- input$SelectIonImageRegions
			regions2 <- getIonImageRegions(obj)
			if ( all(regions1 != "") && any(!regions1 %in% regions2) ) {
				if ( modal$IonImage == "explore" ) {
					isolate(modal$x <- NULL)
					isolate(modal$y <- NULL)
					modal$IonImage <- "select"
					isolate(trigger$IonImage <- trigger$IonImage + 1)
				} else if ( modal$IonImage == "done-selecting" ) {
					newname <- regions1[which(!regions1 %in% regions2)]
					coord <- coord(obj)[subset_0(),c("x","y")]
					selected <- numeric(ncol(obj))
					selected[subset_0()] <- sp::point.in.polygon(coord$x, coord$y,
						isolate(modal$x), isolate(modal$y))
					selected <- selected > 0
					pData(obj)[[newname]] <- selected
					assign(isolate(input$Dataset_0), obj, envir=globalenv())
					isolate(trigger$PixelData <- trigger$PixelData + 1)
					modal$IonImage <- "preview-region"
					isolate(trigger$IonImage <- trigger$IonImage + 1)

				}
			}
		}
	})

	observe({
		if ( input$PreviewIonImageRegions > 0 ) {
			modal$IonImage <- "preview-region"
			isolate(trigger$IonImage <- trigger$IonImage + 1)
		}
	})

	#### Select Ion Image Annotations ####
	##------------------------------------

	getIonImageAnnotations <- function(object) {
		choices <- sapply(pData(object), is.factor)
		choices["sample"] <- FALSE
		choices <- names(choices)[choices]
		c("", choices)
	}

	output$SelectIonImageAnnotations <- renderUI({
		trigger$PixelData
		if ( is.null(object_0()) ) {
			selectizeInput("SelectIonImageAnnotations", "Create an annotation", choices=NULL)
		} else {
			selectizeInput("SelectIonImageAnnotations", "Create an annotation",
				choices=getIonImageAnnotations(object_0()),
				options=list(create=TRUE))
		}
	})

	observe({
		obj <- object_0()
		if ( !is.null(obj) ) {
			anno1 <- input$SelectIonImageAnnotations
			anno2 <- getIonImageAnnotations(obj)
			if ( all(anno1 != "") && any(!anno1 %in% anno2) ) {
				regions <- isolate(input$SelectIonImageRegions)
				regions <- as.list(pData(obj)[regions])
				annotation <- do.call(Cardinal:::make.annotation, regions)
				pData(obj)[[anno1]] <- annotation
				assign(isolate(input$Dataset_0), obj, envir=globalenv())
				isolate(trigger$PixelData <- trigger$PixelData + 1)
				modal$IonImage <- "preview-annotation"
				isolate(trigger$IonImage <- trigger$IonImage + 1)
			}
		}
	})

	observe({
		if ( input$PreviewIonImageAnnotations > 0 ) {
			modal$IonImage <- "preview-annotation"	
			isolate(trigger$IonImage <- trigger$IonImage + 1)
		}
	})

	#### Select Mass Spectrum Regions ####
	##------------------------------------

	getMassSpectrumRegions <- function(object) {
		choices <- sapply(fData(object), is.logical)
		names(choices)[choices]
	}

	output$SelectMassSpectrumRegions <- renderUI({
		trigger$FeatureData
		if ( is.null(object_0()) ) {
			selectizeInput("SelectMassSpectrumRegions", "Select a region", choices=NULL)
		} else {
			selectizeInput("SelectMassSpectrumRegions", "Select a region",
				choices=getMassSpectrumRegions(object_0()),
				multiple=TRUE, options=list(create=TRUE))
		}
	})

	observe({
		obj <- object_0()
		if ( !is.null(obj) ) {
			regions1 <- input$SelectMassSpectrumRegions
			regions2 <- getMassSpectrumRegions(obj)
			if ( all(regions1 != "") && any(!regions1 %in% regions2) ) {
				if ( modal$MassSpectrum == "explore" ) {
					isolate(modal$mz <- NULL)
					modal$MassSpectrum <- "select"
					isolate(trigger$MassSpectrum <- trigger$MassSpectrum + 1)
				} else if ( modal$MassSpectrum == "done-selecting" ) {
					newname <- regions1[which(!regions1 %in% regions2)]
					mz <- sort(isolate(modal$mz))
					selected <- mz[1] < mz(obj) & mz(obj) < mz[2]
					fData(obj)[[newname]] <- selected
					assign(isolate(input$Dataset_0), obj, envir=globalenv())
					isolate(trigger$FeatureData <- trigger$FeatureData + 1)
					modal$MassSpectrum <- "preview-region"
					isolate(trigger$MassSpectrum <- trigger$MassSpectrum + 1)
				}
			}
		}
	})

	observe({
		if ( input$PreviewMassSpectrumRegions > 0 ) {
			modal$MassSpectrum <- "preview-region"
			isolate(trigger$MassSpectrum <- trigger$MassSpectrum + 1)
		}
	})


	#### Select Mass Spectrum Annotations ####
	##----------------------------------------

	getMassSpectrumAnnotations <- function(object) {
		choices <- sapply(fData(object), is.factor)
		choices <- names(choices)[choices]
		c("", choices)
	}

	observe({
		obj <- object_0()
		if ( !is.null(obj) ) {
			anno1 <- input$SelectMassSpectrumAnnotations
			anno2 <- getMassSpectrumAnnotations(obj)
			if ( all(anno1 != "") && any(!anno1 %in% anno2) ) {
				regions <- isolate(input$SelectMassSpectrumRegions)
				regions <- as.list(fData(obj)[regions])
				annotation <- do.call(Cardinal:::make.annotation, regions)
				fData(obj)[[anno1]] <- annotation
				assign(isolate(input$Dataset_0), obj, envir=globalenv())
				isolate(trigger$FeatureData <- trigger$FeatureData + 1)
				modal$MassSpectrum <- "preview-annotation"
				isolate(trigger$MassSpectrum <- trigger$MassSpectrum + 1)
			}
		}
	})

	observe({
		if ( input$PreviewMassSpectrumAnnotations > 0 ) {
			modal$MassSpectrum <- "preview-annotation"	
			isolate(trigger$MassSpectrum <- trigger$MassSpectrum + 1)
		}
	})

	output$SelectMassSpectrumAnnotations <- renderUI({
		trigger$FeatureData
		if ( is.null(object_0()) ) {
			selectizeInput("SelectMassSpectrumAnnotations", "Create an annotation", choices=NULL)
		} else {
			selectizeInput("SelectMassSpectrumAnnotations", "Create an annotation",
				choices=getMassSpectrumAnnotations(object_0()),
				options=list(create=TRUE))
		}
	})

	#### Download Ion Image ####
	##--------------------------

	output$DownloadIonImage <- downloadHandler(
		filename=function() {
			paste(input$Sample_0, "_mz", input$mz_0, ".", input$IonImageFormat, sep="")
		},
		content=function(file) {
			fun <- match.fun(input$IonImageFormat)
			if ( input$IonImageFormat == "pdf" ) {
				pdf(file, width=input$IonImageWidth, height=input$IonImageHeight)
			} else {
				fun(file, width=input$IonImageWidth, height=input$IonImageHeight,
					units=input$IonImageUnits, res=72)
			}
			if ( is.null(object_0()) ) {
				plotNull()
			} else {
				plotIonImage_0()
			}
			dev.off()
		}
	)

	#### Download Mass Spectrum ####
	##------------------------------

	output$DownloadMassSpectrum <- downloadHandler(
		filename=function() {
			paste(input$Sample_0, "_x", input$x_0, "_y", input$y_0, ".", input$MassSpectrumFormat, sep="")
		},
		content=function(file) {
			fun <- match.fun(input$MassSpectrumFormat)
			if ( input$MassSpectrumFormat == "pdf" ) {
				pdf(file, width=input$MassSpectrumWidth, height=input$MassSpectrumHeight)
			} else {
				fun(file, width=input$MassSpectrumWidth, height=input$MassSpectrumHeight,
					units=input$MassSpectrumUnits, res=72)
			}
			if ( is.null(object_0()) ) {
				plotNull()
			} else {
				plotMassSpectrum_0()
			}
			dev.off()
		}
	)

	#### Dataset Summary ####
	##-----------------------

	# output$Summary_0 <- renderPrint({
	# 	if ( !is.null(object_0()) )
	# 		print(summary(object_0()))
	# })


	##########################################################
	##-------------------- Process Tab ---------------------##
	##########################################################


	#### Normalize ####
	##-----------------

	output$NormalizeCall <- renderUI({
		args <- input$Dataset_0
		args <- c(args, paste0("method='", input$NormalizeMethod, "'"))
		args <- do.call(paste, as.list(c(args, sep=", ")))
		textInput("NormalizeCall", "R function that will be applied:",
			value=paste0("normalize(", args, ")"))
	})

	observe({
		if ( input$NormalizePreview	> 0 ) {
			isolate(modal$Process <- "normalize")
			isolate(trigger$MassSpectrum <- trigger$MassSpectrum + 1)
		}
	})

	observe({
		if ( input$NormalizeApply > 0 ) {
			withProgress(value=0, {
				shinyActive(TRUE)
				thecall <- isolate(input$NormalizeCall)
				cl <- parse(text=thecall)[[1]]
				x <- eval(cl, envir=globalenv())
				name <- make.names(paste(isolate(input$Dataset_0), Sys.time()))
				assign(name, isolate(object_0()), envir=globalenv())
				assign(isolate(input$Dataset_0), x, envir=globalenv())
				shinyActive(FALSE)
			}, detail="R is busy. Please do not click anything.")
			isolate(modal$Dataset <- isolate(input$Dataset_0))
			isolate(trigger$IonImage <- trigger$IonImage + 1)
			isolate(trigger$MassSpectrum <- trigger$MassSpectrum + 1)
		}
	})

	#### Smooth Signal ####
	##---------------------

	output$SmoothSignalCall <- renderUI({
		args <- input$Dataset_0
		args <- c(args, paste0("method='", input$SmoothSignalMethod, "'"))
		if ( input$SmoothSignalMethod %in% c("gaussian", "sgolay", "ma") )
			args <- c(args, paste0("window=", input$SmoothSignalWindow))
		args <- do.call(paste, as.list(c(args, sep=", ")))
		textInput("SmoothSignalCall", "R function that will be applied:",
			value=paste0("smoothSignal(", args, ")"))
	})

	observe({
		if ( input$SmoothSignalPreview > 0 ) {
			isolate(modal$Process <- "smooth-signal")
			isolate(trigger$MassSpectrum <- trigger$MassSpectrum + 1)
		}
	})

	#### Reduce Baseline ####
	##---------------------

	output$ReduceBaselineCall <- renderUI({
		args <- input$Dataset_0
		args <- c(args, paste0("method='", input$ReduceBaselineMethod, "'"))
		if ( input$ReduceBaselineMethod %in% c("median") )
			args <- c(args, paste0("blocks=", input$ReduceBaselineBlocks))
		args <- do.call(paste, as.list(c(args, sep=", ")))
		textInput("ReduceBaselineCall", "R function that will be applied:",
			value=paste0("reduceBaseline(", args, ")"))
	})

	observe({
		if ( input$ReduceBaselinePreview > 0 ) {
			isolate(modal$Process <- "reduce-baseline")
			isolate(trigger$MassSpectrum <- trigger$MassSpectrum + 1)
		}
	})

	#### Peak Pick ####
	##---------------------

	output$PeakPickCall <- renderUI({
		args <- input$Dataset_0
		args <- c(args, paste0("method='", input$PeakPickMethod, "'"))
		if ( input$PeakPickMethod %in% c("simple", "adaptive", "limpic") ) {
			args <- c(args, paste0("SNR=", input$PeakPickSNR))
			args <- c(args, paste0("window=", input$PeakPickWindow))
			args <- c(args, paste0("blocks=", input$PeakPickBlocks))
		}
		args <- do.call(paste, as.list(c(args, sep=", ")))
		textInput("PeakPickCall", "R function that will be applied:",
			value=paste0("peakPick(", args, ")"))
	})

	observe({
		if ( input$PeakPickPreview > 0 ) {
			isolate(modal$Process <- "peak-pick")
			isolate(trigger$MassSpectrum <- trigger$MassSpectrum + 1)
		}
	})

	#### Peak Align ####
	##---------------------

	output$PeakAlignReference <- renderUI({
		choices1 <- unlist(eapply(globalenv(), is, "MSImageSet"))
		choices2 <- unlist(eapply(globalenv(), is, "numeric"))
		choices <- c(choices1, choices2)
		choices <- c("<None>", names(choices)[choices])
		selectInput("PeakAlignReference", "Reference", choices=choices)
	})

	output$PeakAlignCall <- renderUI({
		args <- input$Dataset_0
		if ( !is.null(input$PeakAlignReference) && input$PeakAlignReference != "<None>" )
			args <- c(args, paste0("ref=", input$PeakAlignReference))
		args <- c(args, paste0("method='", input$PeakAlignMethod, "'"))
		if ( input$PeakAlignMethod %in% "diff" ) {
			args <- c(args, paste0("diff.max=", input$PeakAlignDiffMax))
			args <- c(args, paste0("units='", input$PeakAlignUnits, "'"))
		} else if ( input$PeakAlignMethod %in% "DP" )
			args <- c(args, paste0("gap=", input$PeakAlignGap))
		args <- do.call(paste, as.list(c(args, sep=", ")))
		textInput("PeakAlignCall", "R function that will be applied:",
			value=paste0("peakAlign(", args, ")"))
	})

	observe({
		if ( input$PeakAlignPreview > 0 ) {
			isolate(modal$Process <- "peak-align")
			isolate(trigger$MassSpectrum <- trigger$MassSpectrum + 1)
		}
	})

	#### Peak Filter ####
	##-------------------

	output$PeakFilterCall <- renderUI({
		args <- input$Dataset_0
		args <- c(args, paste0("method='", input$PeakFilterMethod, "'"))
		if ( input$PeakFilterMethod %in% c("freq") ) {
			if ( input$PeakFilterFreqType == "% of Pixels" ) {
				args <- c(args, paste0("freq.min=",
					input$PeakFilterFreqMin / 100, " * ncol(", input$Dataset_0, ")"))
			} else {
				args <- c(args, paste0("freq.min=", input$PeakFilterFreqMin))
			}
		}
		args <- do.call(paste, as.list(c(args, sep=", ")))
		textInput("PeakFilterCall", "R function that will be applied:",
			value=paste0("peakFilter(", args, ")"))
	})

	observe({
		if ( input$PeakFilterPreview > 0 ) {
			isolate(modal$Process <- "peak-filter")
			isolate(trigger$MassSpectrum <- trigger$MassSpectrum + 1)
		}
	})

	#### Reduce Dimension ####
	##------------------------

	output$ReduceDimensionPeakReference <- renderUI({
		choices1 <- unlist(eapply(globalenv(), is, "MSImageSet"))
		choices2 <- unlist(eapply(globalenv(), is, "numeric"))
		choices <- c(choices1, choices2)
		choices <- c("<None>", names(choices)[choices])
		selectInput("ReduceDimensionPeakReference", "Reference", choices=choices)
	})

	output$ReduceDimensionCall <- renderUI({
		args <- input$Dataset_0
		args <- c(args, paste0("method='", input$ReduceDimensionMethod, "'"))
		if ( input$ReduceDimensionMethod == "peaks" ) {
			if ( !is.null(input$ReduceDimensionPeakReference) && input$ReduceDimensionPeakReference != "<None>" )
				args <- c(args, paste0("ref=", input$ReduceDimensionPeakReference))
			args <- c(args, paste0("type='", input$ReduceDimensionPeakType, "'"))
		} else if ( input$ReduceDimensionMethod == "bin" ) {
			args <- c(args, paste0("width=", input$ReduceDimensionBinWidth))
			args <- c(args, paste0("offset=", input$ReduceDimensionBinOffset))
			args <- c(args, paste0("fun=", input$ReduceDimensionBinFunction))
		} else if ( input$ReduceDimensionMethod == "resample" ) {
			args <- c(args, paste0("step=", input$ReduceDimensionResampleStep))
			args <- c(args, paste0("offset=", input$ReduceDimensionResampleOffset))
		}
		args <- do.call(paste, as.list(c(args, sep=", ")))
		textInput("ReduceDimensionCall", "R function that will be applied:",
			value=paste0("reduceDimension(", args, ")"))
	})

	observe({
		if ( input$ReduceDimensionPreview > 0 ) {
			isolate(modal$Process <- "reduce-dimension")
			isolate(trigger$MassSpectrum <- trigger$MassSpectrum + 1)
		}
	})

	#### Standardize Samples ####
	##---------------------------

	output$StandardizeSamplesCall <- renderUI({
		args <- input$Dataset_0
		args <- c(args, paste0("method='", input$StandardizeSamplesMethod, "'"))
		if ( input$StandardizeSamplesMethod %in% c("sum") ) {
			if ( input$StandardizeSamplesSumType == "% of Pixels" ) {
				args <- c(args, paste0("sum=",
					input$StandardizeSamplesSum / 100, " * ncol(", input$Dataset_0, ")"))
			} else {
				args <- c(args, paste0("sum=", input$StandardizeSamplesSum))
			}
		}
		args <- do.call(paste, as.list(c(args, sep=", ")))
		textInput("StandardizeSamplesCall", "R function that will be applied:",
			value=paste0("standardizeSamples(", args, ")"))
	})

	##########################################################
	##-------------------- Analyze Tab ---------------------##
	##########################################################


	#### PCA ####
	##-----------

	output$PCAFitCall <- renderUI({
		args <- input$Dataset_0
		args <- c(args, paste0("ncomp=c(", input$PCANumberOfComponents, ")"))
		args <- c(args, paste0("method='", input$PCAMethod, "'"))
		args <- c(args, paste0("scale=", input$PCAScale))
		args <- do.call(paste, as.list(c(args, sep=", ")))
		textInput("PCAFitCall", "R function that will be applied:",
			value=paste0("PCA(", args, ")"))
	})

	output$PCAPredictObject <- renderUI({
		choices <- unlist(eapply(globalenv(), is, "PCA"))
		choices <- c("<None>", sort(names(choices)[choices]))
		selectInput("PCAPredictObject", label="Fitted Object", choices=choices)
	})

	output$PCAPredictCall <- renderUI({
		args <- input$PCAPredictObject
		args <- c(args, paste0("newx=", input$Dataset_0))
		args <- do.call(paste, as.list(c(args, sep=", ")))
		textInput("PCAFitCall", "R function that will be applied:",
			value=paste0("predict(", args, ")"))
	})


	##########################################################
	##-------------------- Results Tab ---------------------##
	##########################################################

	output$ResultsObject <- renderUI({
		choices <- unlist(eapply(globalenv(), is, "ResultSet"))
		choices <- c("<None>", sort(names(choices)[choices]))
		selectInput("ResultsObject", label="Results", choices=choices)
	})

	resultsObject <- function() {
		tryCatch(get(input$ResultsObject), error=function(e) NULL)
	}

	# output$ResultsType <- reactive({
	# 	class(resultsObject())
	# })

	# outputOptions(output, "ResultsType", suspendWhenHidden=FALSE)

	output$ResultsModel <- renderUI({
		if ( is.null(resultsObject()) ) {
			choices <- ""
		} else {
			choices <- rownames(modelData(resultsObject()))
		}
		selectInput("ResultsModel", label="Model", choices=choices)
	})

	output$ResultsPixelMode <- renderUI({
		choices <- sapply(resultsObject()[[input$ResultsModel]], function(r) {
			if ( is.matrix(r) ) {
				nrow(r)
			} else {
				length(r)
			}
		})
		choices <- names(choices)[choices == ncol(resultsObject())]
		selectInput("ResultsPixelMode", label="Pixel Mode", choices=choices)
	})

	output$ResultsPixelColumn <- renderUI({
		if ( is.null(resultsObject()) ) {
			choices <- ""
		} else {
			res <- resultsObject()[[input$ResultsModel]][[input$ResultsPixelMode]]
			if ( is.matrix(res) ) {
				choices <- colnames(res)
			} else {
				choices <- as.character(sort(unique(res)))
			}
		}
		selectInput("ResultsPixelColumn", label="Column", choices=choices,
			selected=choices, multiple=TRUE)
	})

	output$ResultsFeatureMode <- renderUI({
		choices <- sapply(resultsObject()[[input$ResultsModel]], function(r) {
			if ( is.matrix(r) ) {
				nrow(r)
			} else {
				length(r)
			}
		})
		choices <- names(choices)[choices == nrow(resultsObject())]
		selectInput("ResultsFeatureMode", label="Mode", choices=choices)
	})

	output$ResultsFeatureColumn <- renderUI({
		if ( is.null(resultsObject()) ) {
			choices <- ""
		} else {
			res <- resultsObject()[[input$ResultsModel]][[input$ResultsFeatureMode]]
			if ( is.matrix(res) ) {
				choices <- colnames(res)
			} else {
				choices <- as.character(sort(unique(res)))
			}
		}
		selectInput("ResultsFeatureColumn", label="Column", choices=choices,
			selected=choices, multiple=TRUE)
	})

	output$ResultsImage <- renderPlot({
		par(mar=c(3,3,3,1), mgp=c(1.5,0.5,0), cex.axis=1, cex.lab=1)
		if ( is.null(resultsObject()) ) {
			plotNull()
		} else {
			column <- input$ResultsPixelColumn
			if ( is.null(column) || column == "" ) {
				image(resultsObject(),
					model=input$ResultsModel,
					mode=input$ResultsPixelMode)
			} else {
				image(resultsObject(),
					model=input$ResultsModel,
					mode=input$ResultsPixelMode,
					column=input$ResultsPixelColumn)
			}
		}
	}, bg="transparent")

	output$ResultsPlot <- renderPlot({
		par(mar=c(3,3,3,1), mgp=c(1.5,0.5,0), cex.axis=1, cex.lab=1)
		if ( is.null(resultsObject()) ) {
			plotNull()
		} else {
			column <- input$ResultsFeatureColumn
			if ( is.null(column) || column == "" ) {
				plot(resultsObject(),
					model=input$ResultsModel,
					mode=input$ResultsFeatureMode)
			} else {
				plot(resultsObject(),
					model=input$ResultsModel,
					mode=input$ResultsFeatureMode,
					column=input$ResultsFeatureColumn)
			}
		}
	}, bg="transparent")


	#########################################################
	##-------------------- Workspace ----------------------##
	#########################################################

	output$Dataset_1 <- renderUI({
		choices <- unlist(eapply(globalenv(), is, "MSImageSet"))
		choices <- c("<None>", sort(names(choices)[choices]))
		selectInput("Dataset_1", label=NULL, choices=choices,
			selected=input$Dataset_0)
	})

	object_1 <- function() {
		tryCatch(get(input$Dataset_1), error=function(e) NULL)
	}

	output$Summary_1 <- renderPrint({
		if ( !is.null(object_1()) )
			print(object_1())
	})

	# shinyFileChoose(input, "File", session=session, roots=c(Home="~"))

})
