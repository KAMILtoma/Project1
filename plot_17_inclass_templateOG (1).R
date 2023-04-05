library(shiny)
library(mowateR)
library(lubridate)

load("denver_filters.rda")

for(i in 1:16) {
  colnames(denver_filters[[1]][[i]])[3] <- "ntu"
}

denver_filters <- denver_filters[[1]]

######################
# Define UI ----
######################
ui <- fluidPage(
  
  # App title ----
  titlePanel("Denver Water"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
    
      #First widget, uncomment & fill in the arguments
      numericInput("num", label = h3("Filter number (1-16)"), value= 1,
                   min=1, max=16),

      #Second widget, uncomment & fill in the arguments
      sliderInput("obs", label= "Observation Range", 
                  min=1, max=nrow(denver_filters[[1]]),
                  value = c(1,nrow(denver_filters[[1]]))),
      
      
      
      h1("Outlier Control"),
      
      #Third widget, uncomment & fill in the arguments
      checkboxInput("outliers",label="Show Outliers",
                    value=F),
      
      #Fourth widget, uncomment & fill in the arguments
      radioButtons("outLvl", label=h3("Outlier Display Level"),
                   choices=list("3x std deviations"= 3,
                                "4x std deviations"= 4,
                                "5x std deviations"= 5),
                   selected=4),
                   
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      plotOutput("plot1")
      
    )
  )

)

# Define server logic ----
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
  #Fill in the time series plot here
  den_filter = denver_filters[[input$num]]
  plot(den_filter$Date.Time[input$obs[1]:input$obs[2]],
       den_filter$ntu[input$obs[1]:input$obs[2]],
       type ="l",
       xlab= "Date (2019-2020)",
       ylab="NTU",
       main="NTU Readings 2019-2020")
  
  #Uncomment and fill in the if() statement to show the outliers
  if(input$outliers){
  den_mean=mean(den_filter$ntu, na.rm=T)
  den_sd=sd(den_filter$ntu, na.rm=T)
  
  scale=as.numeric(input$outLvl)
  outliers = den_filter[which(den_filter$ntu > (den_mean + den_sd*scale)), ]
  points(outliers$Date.Time, outliers$ntu, col="red")
  
  }
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

