
require(shiny)

shinyUI(navbarPage("Cardinal",

	tabPanel("Data",
		wellPanel(

			#### main dataset ####

			fluidRow(
				column(3,
					uiOutput("Dataset_0")
				),
				column(3,
					numericInput("mz_0", "m/z", value=NA, step=1)
				),
				column(3,
					numericInput("x_0", "x", value=NA, step=1)
				),
				column(3,
					numericInput("y_0", "y", value=NA, step=1)
				)
			),
			fluidRow(
				column(6,
					plotOutput("IonImage_0", clickId="IonImageClick_0"),
					fluidRow(
						column(6,
							selectInput("Sample_0", "Sample", choices="<None>")
						),
						column(6,
							numericInput("IonImageZoom_0", "Ion Image Zoom %", value=100,
								min=100, max=1000, step=25)
						)
					)
				),
				column(6,
					plotOutput("MassSpectrum_0", clickId="MassSpectrumClick_0"),
					uiOutput("MassRange_0")
				)
			)
		),

		#### control panels ####

		tabsetPanel(type="tabs",
			tabPanel("Explore",
				navlistPanel(
					tabPanel("Ion Image",
						tabsetPanel(type="tabs",
							tabPanel("Display Options",
								column(6,
									selectInput("ColorRegions", "Color Regions",
										choices=c("intensity.colors",
											"heat.colors", "terrain.colors",
											"topo.colors", "cm.colors")),
									plotOutput("ColorRegions", height="25px"),
									sliderInput("ColorRegionsRange", NULL,
										min=0, max=100, value=c(0,100), step=1,
										width="100%", post="%"),
									checkboxInput("ShowColorkey", "Show colorkey", value=TRUE)
								),
								column(6,
									selectInput("ContrastEnhance", "Contrast Enhance",
										choices=c("none", "suppression", "histogram")),
									selectInput("SmoothImage", "Smooth Image",
										choices=c("none", "gaussian", "adaptive"))
								)
							),
							tabPanel("Select Regions",
								column(6,
									uiOutput("SelectIonImageRegions"),
									helpText("Enter a new name to select a new region."),
									actionButton("PreviewIonImageRegions", "Preview")
								),
								column(6,
									uiOutput("SelectIonImageAnnotations"),
									helpText("Enter a new name to create an annotation",
										"based on the currently selected regions."),
									actionButton("PreviewIonImageAnnotations", "Preview")
								)
							),
							tabPanel("Download",
								selectInput("IonImageFormat", "Format",
									choices=c("pdf", "png", "jpeg", "tiff")),
								fluidRow(
									column(4,
										numericInput("IonImageWidth", "Width", value=7)
									),
									column(4,
										numericInput("IonImageHeight", "Height", value=7)
									),
									column(4,
										selectInput("IonImageUnits", "Units", choices="in")
									)
								),
								downloadButton("DownloadIonImage", "Download")
							)
						)
					),
					tabPanel("Mass Spectrum",
						tabsetPanel(type="tabs",
							tabPanel("Display Options",
								uiOutput("Color")
							),
							tabPanel("Select Regions",
								column(6,
									uiOutput("SelectMassSpectrumRegions"),
									helpText("Enter a new name to select a new region."),
									actionButton("PreviewMassSpectrumRegions", "Preview")
								),
								column(6,
									uiOutput("SelectMassSpectrumAnnotations"),
									helpText("Enter a new name to create an annotation",
										"based on the currently selected regions."),
									actionButton("PreviewMassSpectrumAnnotations", "Preview")
								)
							),
							tabPanel("Download",
								selectInput("MassSpectrumFormat", "Format",
									choices=c("pdf", "png", "jpeg", "tiff")),
								fluidRow(
									column(4,
										numericInput("MassSpectrumWidth", "Width", value=7)
									),
									column(4,
										numericInput("MassSpectrumHeight", "Height", value=7)
									),
									column(4,
										selectInput("MassSpectrumUnits", "Units", choices="in")
									)
								),
								downloadButton("DownloadMassSpectrum", "Download")
							)
						)
					)
				)
			),
			tabPanel("Process",
				navlistPanel(
					"Spectral processing",
					tabPanel("Normalize"),
					tabPanel("Smooth Signal"),
					tabPanel("Reduce Baseline"),
					tabPanel("Peak Pick"),
					"Additional processing",
					tabPanel("Peak Align"),
					tabPanel("Peak Filter"),
					tabPanel("Reduce Dimension"),
					tabPanel("Standardize Samples")
				)
			),
			tabPanel("Analyze",
				navlistPanel(
					"Multivariate",
					tabPanel("PCA"),
					tabPanel("PLS"),
					tabPanel("O-PLS"),
					"Clustering",
					tabPanel("Spatial K-Means"),
					"Classification",
					tabPanel("Spatial Shrunken Centroids")
				)
			),
			tabPanel("Results",
					selectInput("Results",
						"Results",
						choices="<None>",
						selected="<None>")
			)
		)
	),

	tabPanel("Workspace",
		fluidRow(
			# shinyFilesButton("File", "􏰀File select􏰀", title="􏰀Please select a file􏰀", multiple=FALSE)
		)
	)

))
