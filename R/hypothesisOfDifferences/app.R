# Imports the shiny library and the ggplot2 library
library(shiny); library(ggplot2); library(shinythemes)

# Returns a function that defines the user interface properties and layout
ui <- navbarPage("Hypotheses of Differences",
	
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
					   plotOutput("distPlot2"), offset = 4)
			)		 
		)
	),
	


	tabPanel("Use it",
	    # Sidebar with a slider input for number of bins 
	    fluidRow(
	    	column(4,
	    	    sliderInput("n1", "Number of Observations in Group #1:",
	                		min = 1, max = 100, value = 20),
	            sliderInput("mu1", "Average Test Score for Group #1:",
	            			min = 100, max = 300, value = 200),
	            sliderInput("sigma1", "Standard Deviation of Test Scores for Group #1:",
	            			min = 5, max = 50, value = 10),
	            sliderInput("n2", "Number of Observations in Group #2:",
	                        min = 1, max = 100, value = 45),
	            sliderInput("mu2", "Average Test Score for Group #2:",
	            			min = 100, max = 300, value = 175),
	            sliderInput("sigma2", "Standard Deviation of Test Scores for Group #2:",
	            			min = 5, max = 50, value = 30)
		    ),
        	# Show a plot of the generated distribution
	        column(8,
	         plotOutput("distPlot"),
	         verbatimTextOutput("ttestResults"),
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
	
   		# Generates the group name and random test scores for group 1
   	   	g1 <- data.frame(group = "Group 1", 
	   					 score = rnorm(input$n1, input$mu1, input$sigma1))
	
   		# Generates the group name and random test scores for group 2
	   	g2 <- data.frame(group = "Group 2", 
	   					 score = rnorm(input$n2, input$mu2, input$sigma2))
	   	
	   	# Does the equivalent of a UNION ALL in SQL (e.g., appends the data)
	   	dataset <- rbind(g1, g2)
	   	
	   	# Returns the data frame with the data for both groups
		return(dataset)
	   	
   	}) # End of function to simulate the data
	
	thePlot <- reactive({
   		# Defines the data to use for the graph/plot and how the variables in 
   		# the data set should be mapped to features of the graph/plot
    	ggplot(dataset(), aes(x = score, fill = group)) + 
   		
   		# Defines the type of mark that should be used to display the data
   		geom_density(alpha = 0.35) +

   		# Creates axis titles for the x and y axes
   		xlab("Test Scores") + ylab("Proportion of Students") + 
   		
   		# Creates the title for the graph/plot 
   		ggtitle("Distribution of Students' Scores for the Two Groups") + 
   		
   		# Defines the colors to use for the density plot fills for the two 
   		# groups, how the colors map to the values in the data set, and how to 
   		# assign labels to the legend
   		scale_fill_manual(values = c("#0000FF", "#FF9900"),                
		breaks = c("Group 1", "Group 2"), name = "Student Groups") +   
   		
   		# Defines how to generate the scale for the x axis
   		scale_x_continuous(breaks = seq(100, 300, 10)) + 
   		
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

	}) # End of function to generate the graph showing the overlap of the distributions

   	theTest <- reactive({
   		t.test(score ~ group, data = dataset())
   	})
	
	# Returns function used to generate the graph showing the distribution of 
	# scores of the two groups as overlaid density plots
   	output$distPlot <- renderPlot(thePlot()) 
   	
   	output$distPlot2 <- renderPlot(thePlot())
   
   	# Returns a function containing the simulated data so users could view it
   	output$rawData <- renderDataTable(dataset(), options = list(pageLength = 10))

   	output$rawestData <- renderDataTable(dataset(), options = list(pageLength = 200))
   	
	# Returns a function that contains the results of a t-test
   	output$ttestResults <- renderPrint(theTest())

   	output$exampleTestResults <- renderPrint(theTest())
   
} # End of the function representing the server side processing

# Runs the application 
shinyApp(ui = ui, server = server)

