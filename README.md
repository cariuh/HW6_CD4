library(shiny)
library(ggplot2)

#Read in the Data 
CD4 <- read.csv("http://www.stat.columbia.edu/~gelman/arm/examples/cd4/allvar.csv")

# Define UI for application that graphs outcome of each child as a function of time.
#Each child's data has a time course that can be summarized by a linear fit.  
#Estimate these lines and plot them for all the children
ui <- fluidPage(
   
   # Application title
   titlePanel("CD4 Data of HIV Patients"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("newpid",
                     "Number of HIV Patients:",
                     min = 1,
                     max = length(unique(CD4$newpid)),
                     value = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("Scatterplot")
      )
   )
)

# Define server logic required to draw a Scatterplot
server <- function(input, output) {
   
   output$Scatterplot <- renderPlot({
     ggplot(data = CD4[1:input$newpid,], aes(x = visage - baseage, y = sqrt(CD4PCT), 
       color = factor(newpid))) +
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       ggtitle("Sqrt(CD4PCT) Over Time") +
       labs(y = "Sqrt(CD4PCT)")  + 
       scale_color_discrete(guide = "none")
   })
}
# Run the application 
shinyApp(ui = ui, server = server)
