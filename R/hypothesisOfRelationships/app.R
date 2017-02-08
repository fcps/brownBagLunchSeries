# Imports the shiny library and the ggplot2 library
library(shiny); library(ggplot2); library(shinythemes)

# Returns a function that defines the user interface properties and layout
ui <- navbarPage("Hypotheses of Relationships",
	
	collapsible = TRUE, fluid = TRUE, 
	
	theme = shinytheme("lumen"),
	
	# Should probably use a Navigation bar based layout so we can present 
	# some informative text that would explain how to use the application and
	# the purpose of the application
	navbarMenu("About",
		tabPanel("Purpose",
		 	fluidRow(
		 		column(8, includeMarkdown("appText/about.Rmd"), offset = 4)
		 	)
		),
	
		tabPanel("Viewing the raw data",
			fluidRow(
				column(8, 
					   includeMarkdown("appText/rawData.Rmd"), 
					   dataTableOutput("rawestData"), offset = 4)
			)		 
		),
		tabPanel("Understanding the Statistics",
			fluidRow(
				column(8, includeMarkdown("appText/statsExplanation.Rmd"),
					   verbatimTextOutput("exampleTestResults"), offset = 4)
			)		 
		),
		tabPanel("Understanding the Plot",
			fluidRow(
				column(8, 
					   includeMarkdown("appText/plotExplanation.Rmd"), 
					   plotOutput("scatterPlot2"), offset = 4)
			)		 
		)
	),
	


	tabPanel("Use it",
	    # Sidebar with a slider input for number of bins 
	    fluidRow(
	    	column(4,
	    	    sliderInput("n", "Number of Observations:",
	                		min = 10, max = 1000, value = 300, step = 1),
	            sliderInput("muRead", "Average Reading Test Score:",
	            			min = 100, max = 300, value = 200, step = 1),
	            sliderInput("sigmaRead", "Standard Deviation of Reading Test Scores:",
	            			min = 1, max = 15, value = 10, step = 0.1),
	            sliderInput("muMath", "Average Math Test Score:",
	            			min = 100, max = 300, value = 200, step = 1),
	            sliderInput("sigmaMath", "Standard Deviation of Math Test Scores:",
	            			min = 1, max = 15, value = 10, step = 0.1),
	            sliderInput("rho", "Amount of Correlation between Test Scores",
	            			min = -1, max = 1, value = 0, step = 0.01),
	    		checkboxInput("regLine", "Display the line of best fit", 
	    					  value = FALSE)
		    ),
        	# Show a plot of the generated distribution
	        column(8,
	         plotOutput("scatterPlot"),
	         verbatimTextOutput("corrResults"),
	         dataTableOutput("rawData")
	      )
   		)
	)		 
)	

# Returns a function that takes user input from the UI, simulates data for two 
# groups and draws an overlapping set of density plots to illustrate what it is
# that is being tested when looking at the difference in means of two groups
server <- function(input, output) {
	
		# Returns a function that creates the simulated data based on the 
	# parameters selected by the end user in the GUI
	dataset <- reactive({

	   	# Sets the pseudorandom number seed value to a constant	
		set.seed(7779311)
	
   		dataset <- data.frame(SimComp::ermvnorm(input$n, 
   								mean = c(input$muRead, input$muMath),
   					  			sd = c(input$sigmaRead, input$sigmaMath),
   								corr = rbind(c(1, input$rho), c(input$rho, 1))))
	   	
   		names(dataset) <- c("reading", "math")
   		
	   	# Returns the data frame with the data for both groups
		return(dataset)
	   	
   	}) # End of function to simulate the data
	
	thePlot <- reactive({
		
		# Defines the data to use for the graph/plot and how the variables in 
   		# the data set should be mapped to features of the graph/plot
    	basePlot <- ggplot(dataset(), aes(x = reading, y = math)) + 
   		
   		# Defines the type of mark that should be used to display the data
   		geom_point(alpha = 0.25) + 

   		# Creates axis titles for the x and y axes
   		xlab("Reading Scores") + ylab("Math Scores") + 
   		
   		# Creates the title for the graph/plot 
   		ggtitle("Relationship between reading and math scores") + 
   		
   		# Defines how to generate the scale for the x axis
   		scale_x_continuous(breaks = seq(100, 300, 10)) + 
   		
   		# Defines how to generate the scale for the x axis
   		scale_y_continuous(breaks = seq(100, 300, 10)) + 
   		
   		# Defines other aesthetic properties of the graph/plot
   		theme(panel.background = element_rect(fill = "White"),  		   
			  strip.text = element_text(size = 12), 
			  legend.title = element_text(size = 12), 
			  legend.text = element_text(size = 10),
			  plot.title = element_text(size = 16),
			  axis.title.x = element_text(size = 20, color = "Black"),
			  axis.text.x = element_text(size = 16, color = "Black"),
			  axis.title.y = element_text(size = 16, color = "Black"),			  
			  axis.text.y = element_text(size = 12, color = "Black"))
	
    	if (input$regLine == TRUE) basePlot <- basePlot + #
    			geom_smooth(method = "loess", show.legend = TRUE)
   		
    	basePlot 
    	
	}) # End of function to generate the graph showing the overlap of the distributions

   	theTest <- reactive({
   		cor.test(~ reading + math, data = dataset())
   	})
	
	# Returns function used to generate the graph showing the distribution of 
	# scores of the two groups as overlaid density plots
   	output$scatterPlot <- renderPlot(thePlot()) 
   	
   	output$scatterPlot2 <- renderPlot(thePlot())
   
   	# Returns a function containing the simulated data so users could view it
   	output$rawData <- renderDataTable(dataset(), options = list(pageLength = 10))

   	output$rawestData <- renderDataTable(dataset(), options = list(pageLength = 200))
   	
	# Returns a function that contains the results of a t-test
   	output$corrResults <- renderPrint(theTest())

   	output$exampleTestResults <- renderPrint(theTest())
   
} # End of the function representing the server side processing

# Runs the application 
shinyApp(ui = ui, server = server)
