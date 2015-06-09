
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
					plotOutput("IonImage_0",
						click=clickOpts(id="IonImageClick_0"),
						dblclick=dblclickOpts(id="IonImageDblClick_0"),
						brush=brushOpts(id="IonImageBrush_0", direction="xy", resetOnNew=TRUE)),
					fluidRow(
						column(6,
							selectInput("Sample_0", "Sample", choices="<None>")
						),
						column(6,
							numericInput("IonImageZoom_0", "Ion Image Zoom %", value=NA,
								min=100, max=500, step=25)
						)
					)
				),
				column(6,
					plotOutput("MassSpectrum_0",
						click=clickOpts(id="MassSpectrumClick_0"),
						dblclick=dblclickOpts(id="MassSpectrumDblClick_0"),
						brush=brushOpts(id="MassSpectrumBrush_0", direction="x", resetOnNew=TRUE)),
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
					tabPanel("Reduce Baseline",
						selectizeInput("ReduceBaselineMethod", "Method",
							choices=c("median"),
							options=list(create=TRUE)),
						conditionalPanel(
							condition="input.ReduceBaselineMethod == 'median'",
							numericInput("ReduceBaselineBlocks", "Blocks",
								min=5, max=10000, value=500, step=10)
						),
						actionButton("ReduceBaselineApply", "Apply"),
						actionButton("ReduceBaselinePreview", "Preview"),
						hr(),
						wellPanel(
							uiOutput("ReduceBaselineCall"),
							helpText("Warning: This is the exact function
								that will be applied to the dataset. Edit
								at your own risk.")
						)
					),
					tabPanel("Peak Pick",
						selectizeInput("PeakPickMethod", "Method",
							choices=c("simple", "adaptive", "limpic"),
							options=list(create=TRUE)),
						conditionalPanel(
							condition=paste("input.PeakPickMethod == 'simple'",
								"|| input.PeakPickMethod == 'adaptive'",
								"|| input.PeakPickMethod == 'limpic'"),
							numericInput("PeakPickSNR", "SNR",
								min=1, max=100, value=6, step=1),
							numericInput("PeakPickWindow", "Window",
								min=3, max=101, value=5, step=2),
							numericInput("PeakPickBlocks", "Blocks",
								min=5, max=10000, value=100, step=10)
						),
						actionButton("PeakPickApply", "Apply"),
						actionButton("PeakPickPreview", "Preview"),
						hr(),
						wellPanel(
							uiOutput("PeakPickCall"),
							helpText("Warning: This is the exact function
								that will be applied to the dataset. Edit
								at your own risk.")
						)
					),
					"Additional processing",
					tabPanel("Peak Align",
						selectizeInput("PeakAlignMethod", "Method",
							choices=c("diff", "DP"),
							options=list(create=TRUE)),
						uiOutput("PeakAlignReference"),
						conditionalPanel(
							condition="input.PeakAlignMethod == 'diff'",
							numericInput("PeakAlignDiffMax", "Diff Max",
								min=1, max=10000, value=200, step=1),
							selectInput("PeakAlignUnits", "Units",
								choices=c("'ppm'", "'mz'"))
						),
						conditionalPanel(
							condition="input.PeakAlignMethod == 'DP'",
							numericInput("PeakAlignGap", "Gap",
								min=-1000, max=1000, value=0, step=1)
						),
						actionButton("PeakAlignApply", "Apply"),
						actionButton("PeakAlignPreview", "Preview"),
						hr(),
						wellPanel(
							uiOutput("PeakAlignCall"),
							helpText("Warning: This is the exact function
								that will be applied to the dataset. Edit
								at your own risk.")
						)
					),
					tabPanel("Peak Filter",
						selectizeInput("PeakFilterMethod", "Method",
							choices=c("freq"),
							options=list(create=TRUE)),
						conditionalPanel(
							condition="input.PeakFilterMethod == 'freq'",
							numericInput("PeakFilterFreqMin", "Freq Min",
								min=0, max=10000, value=1),
							selectInput("PeakFilterFreqType", NULL,
								choices=c("% of Pixels", "Exactly"))
						),
						actionButton("PeakFilterApply", "Apply"),
						actionButton("PeakFilterPreview", "Preview"),
						hr(),
						wellPanel(
							uiOutput("PeakFilterCall"),
							helpText("Warning: This is the exact function
								that will be applied to the dataset. Edit
								at your own risk.")
						)
					),
					tabPanel("Reduce Dimension",
						selectizeInput("ReduceDimensionMethod", "Method",
							choices=c("peaks", "bin", "resample"),
							options=list(create=TRUE)),
						conditionalPanel(
							condition="input.ReduceDimensionMethod == 'peaks'",
							uiOutput("ReduceDimensionPeakReference"),
							selectInput("ReduceDimensionPeakType", "Type",
								choices=c("'height'", "'area'"))
						),
						conditionalPanel(
							condition="input.ReduceDimensionMethod == 'bin'",
							numericInput("ReduceDimensionBinWidth", "Width",
								min=0, max=100, value=1),
							numericInput("ReduceDimensionBinOffset", "Offset",
								min=-1, max=1, value=0, step=0.1),
							selectInput("ReduceDimensionBinFunction", "Function",
								choices=c("sum", "mean", "median"))
						),
						conditionalPanel(
							condition="input.ReduceDimensionMethod == 'resample'",
							numericInput("ReduceDimensionResampleStep", "Step",
								min=0, max=100, value=1),
							numericInput("ReduceDimensionResampleOffset", "Offset",
								min=-1, max=1, value=0, step=0.1)
						),
						actionButton("ReduceDimensionApply", "Apply"),
						actionButton("ReduceDimensionPreview", "Preview"),
						hr(),
						wellPanel(
							uiOutput("ReduceDimensionCall"),
							helpText("Warning: This is the exact function
								that will be applied to the dataset. Edit
								at your own risk.")
						)
					),
					tabPanel("Standardize Samples",
						selectizeInput("StandardizeSamplesMethod", "Method",
							choices=c("sum"),
							options=list(create=TRUE)),
						conditionalPanel(
							condition="input.StandardizeSamplesMethod == 'sum'",
							numericInput("StandardizeSamplesSum", "Sum",
								min=1, max=10000, value=100),
							selectInput("StandardizeSamplesSumType", NULL,
								choices=c("% of Pixels", "Exactly"))
						),
						actionButton("StandardizeSamplesApply", "Apply"),
						hr(),
						wellPanel(
							uiOutput("StandardizeSamplesCall"),
							helpText("Warning: This is the exact function
								that will be applied to the dataset. Edit
								at your own risk.")
						)
					)
				)
			),

			#### Analyze Tab ####

			tabPanel("Analyze",
				navlistPanel(
					"Multivariate",
					tabPanel("PCA"),
					tabPanel("PLS"),
					tabPanel("O-PLS"),
					"Clustering + Classification",
					tabPanel("Spatial K-Means"),
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
