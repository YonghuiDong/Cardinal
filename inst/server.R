
require(shiny)

shinyServer(function(input, output, session) {



	##########################################################
	##-------------------- Main Panel ----------------------##
	##########################################################


	#### Set up reactive values needed for server ####
	##------------------------------------------------

	# IonImage and MassSpectrum = {
	#	explore, select, done-selecting,
	#	preview-region, preview-annotation
	# }

	modal <- reactiveValues(
		mz=NA,
		x=NA, y=NA,
		IonImage="explore",
		MassSpectrum="explore",
		Process="none")

	trigger <- reactiveValues(
		Dataset=0,
		IonImage=0,
		MassSpectrum=0)

	#### Main Dataset, Ion Image, and Mass Spectrum ####
	##--------------------------------------------------

	## create main dataset object getter and dataset selection

	object_0 <- function() {
		tryCatch(get(input$Dataset_0), error=function(e) NULL)
	}

	output$Dataset_0 <- renderUI({
		choices <- unlist(eapply(globalenv(), is, "MSImageSet"))
		choices <- c("<None>", names(choices)[choices])
		selectInput("Dataset_0", "Dataset", choices=choices)
	})

	## observe dataset selection and reset inputs on loading new dataset

	observe({
		obj <- tryCatch(get(input$Dataset_0), error=function(e) NULL)
		if ( is.null(obj) ) {
			updateNumericInput(session, "mz_0", value=NA)
			updateNumericInput(session, "x_0", value=NA)
			updateNumericInput(session, "y_0", value=NA)
			updateSelectInput(session, "Sample_0", choices="<None>")
			updateNumericInput(session, "IonImageZoom_0", value="100")
		} else {
			updateNumericInput(session, "mz_0", value=mz(obj)[1])
			updateNumericInput(session, "x_0", value=coord(obj)[1,"x"])
			updateNumericInput(session, "y_0", value=coord(obj)[1,"y"])
			updateSelectInput(session, "Sample_0", choices=sampleNames(obj))
			updateNumericInput(session, "IonImageZoom_0", value="100")
		}
	})

	## basic parameters for the displayed plots

	feature_0 <- reactive({
		features(object_0(), mz=input$mz_0)
	})

	pixel_0 <- reactive({
		pixels(object_0(), x=input$x_0, y=input$y_0, sample=input$Sample_0)
	})

	xlim_0 <- reactive({
		zoom <- input$IonImageZoom_0 / 100
		xrange <- range(coord(object_0())[["x"]])
		c(input$x_0 + (xrange[1] - input$x_0) / zoom,
			input$x_0 + (xrange[2] - input$x_0) / zoom)
	})

	ylim_0 <- reactive({
		zoom <- input$IonImageZoom_0 / 100
		yrange <- range(coord(object_0())[["y"]])
		c(input$y_0 + (yrange[1] - input$y_0) / zoom,
			input$y_0 + (yrange[2] - input$y_0) / zoom)
	})

	zlim_0 <- reactive({
		zrange <- range(spectra(object_0())[feature_0(),
			pData(isolate(object_0()))$sample %in% input$Sample_0])
		diff(zrange) * input$ColorRegionsRange / 100 + min(zrange)
	})

	coord_0 <- reactive({
		substitute(list(italic(x) == xx, italic(y) == yy),
			env=list(xx=input$x_0, yy=input$y_0))
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
			xlim=xlim_0(), ylim=ylim_0(), zlim=zlim_0(),
			strip=FALSE,
			contrast.enhance=input$ContrastEnhance,
			smooth.image=input$SmoothImage,
			colorkey=input$ShowColorkey,
			col.regions=match.fun(input$ColorRegions)(100),
			subset=pData(isolate(object_0()))$sample %in% input$Sample_0,
			...)
	}

	plotIonImageGroups_0 <- function(groups, varLabel=NULL, ...) {
		image(isolate(object_0()), groups ~ x * y,
			strip=FALSE,
			colorkey=FALSE,
			col.regions=rainbow(nlevels(groups), alpha=0.75),
			subset=pData(isolate(object_0()))$sample %in% input$Sample_0,
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
	})

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

	#### Plot the Mass Spectrum ####
	##------------------------------

	plotMassSpectrum_0 <- function(...) {
		plot(isolate(object_0()), coord=c(x=input$x_0, y=input$y_0, sample=input$Sample_0),
			main=coord_0(),
			xlim=input$MassRange_0,
			strip=FALSE,
			col=if (is.null(input$Color_0)) "black" else input$Color_0,
			...)
	}

	plotMassSpectrumGroups_0 <- function(groups, varLabel=NULL, ...) {
		plot(isolate(object_0()), coord=c(x=input$x_0, y=input$y_0, sample=input$Sample_0),
			main=coord_0(),
			xlim=input$MassRange_0,
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
		if ( is.na(input$x_0) || is.na(input$y_0) || is.null(isolate(object_0())) ) {
			plotNull()
		} else if ( length(isolate(pixel_0())) == 0 ) {
			plotNull()
		} else if ( isolate(modal$Process != "none") ) {
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
			}
			cl <- parse(text=thecall)[[1]]
			cl[["pixel"]] <- pixel_0()
			cl[["xlim"]] <- isolate(input$MassRange_0)
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
	})

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


	##########################################################
	##-------------------- Explore Tab ---------------------##
	##########################################################


	#### Ion Image Display Options ####
	##---------------------------------

	output$ColorRegions <- renderPlot({
		par(mar=c(0,0,0,0), bty='n', xaxt='n', yaxt='n')
		image(matrix(1:100, ncol=1), col=match.fun(input$ColorRegions)(100))
	})

	#### Mass Spectrum Displa Options ####

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
		trigger$Dataset
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
					coord <- coord(obj)
					subset <- pData(obj)[["sample"]] == input$Sample_0
					coord <- coord[subset,c("x","y")]
					selected <- numeric(ncol(obj))
					selected[subset] <- sp::point.in.polygon(coord$x, coord$y,
						isolate(modal$x), isolate(modal$y))
					selected <- selected > 0
					pData(obj)[[newname]] <- selected
					assign(isolate(input$Dataset_0), obj, envir=globalenv())
					isolate(trigger$Dataset <- trigger$Dataset + 1)
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
		trigger$Dataset
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
				isolate(trigger$Dataset <- trigger$Dataset + 1)
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
		trigger$Dataset
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
					isolate(trigger$Dataset <- trigger$Dataset + 1)
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
				isolate(trigger$Dataset <- trigger$Dataset + 1)
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
		trigger$Dataset
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
			args <- c(args, paste0("units=", input$PeakAlignUnits))
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


	# shinyFileChoose(input, "File", session=session, roots=c(Home="~"))

})
