
require(shiny)

shinyUI(navbarPage("Cardinal",

	#### Data Page ####

	tabPanel("Data",
		wellPanel(

			#### Main Panel ####

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

		#### Control Panels ####

		tabsetPanel(type="tabs",

			#### Explore Tab ####

			tabPanel("Explore",
				navlistPanel(

					#### Ion Image ####

					tabPanel("Ion Image",
						tabsetPanel(type="tabs",

							#### Ion Image Display Options ####

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


							#### Select Ion Image Regions ####

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

							#### Download Ion Image ####

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

					#### Mass Spectrum ####

					tabPanel("Mass Spectrum",
						tabsetPanel(type="tabs",
							tabPanel("Display Options",
								uiOutput("Color")
							),

							#### Select Mass Spectrum Regions ####

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


							#### Download Mass Spectrum ####

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

			#### Process Tab ####

			tabPanel("Process",
				navlistPanel(
					"Spectral processing",
					tabPanel("Normalize",
						selectizeInput("NormalizeMethod", "Method",
							choices="tic",
							options=list(create=TRUE)),
						actionButton("NormalizeApply", "Apply"),
						actionButton("NormalizePreview", "Preview"),
						hr(),
						wellPanel(
							uiOutput("NormalizeCall"),
							helpText("Warning: This is the exact function
								that will be applied to the dataset. Edit
								at your own risk.")
						)
					),
					tabPanel("Smooth Signal",
						selectizeInput("SmoothSignalMethod", "Method",
							choices=c("gaussian", "sgolay", "ma"),
							options=list(create=TRUE)),
						conditionalPanel(
							condition=paste("input.SmoothSignalMethod == 'gaussian'",
								"|| input.SmoothSignalMethod == 'sgolay'",
								"|| input.SmoothSignalMethod == 'ma'"),
							numericInput("SmoothSignalWindow", "Window",
								min=3, max=101, value=5, step=2)
						),
						actionButton("SmoothSignalApply", "Apply"),
						actionButton("SmoothSignalPreview", "Preview"),
						hr(),
						wellPanel(
							uiOutput("SmoothSignalCall"),
							helpText("Warning: This is the exact function
								that will be applied to the dataset. Edit
								at your own risk.")
						)
					),
					tabPanel("Reduce Baseline"),
					tabPanel("Peak Pick"),
					"Additional processing",
					tabPanel("Peak Align"),
					tabPanel("Peak Filter"),
					tabPanel("Reduce Dimension"),
					tabPanel("Standardize Samples")
				)
			),

			#### Analyze Tab ####

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

			#### Results Tab ####

			tabPanel("Results",
					selectInput("Results",
						"Results",
						choices="<None>",
						selected="<None>")
			)
		)
	),

	#### Workspace Page ####

	tabPanel("Workspace",
		fluidRow(
			# shinyFilesButton("File", "􏰀File select􏰀", title="􏰀Please select a file􏰀", multiple=FALSE)
		)
	)

))
