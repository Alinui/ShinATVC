#https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/USPersonalExpenditure.html

library(shiny)
library(dplyr)
library(ggplot2)
library(markdown)


data(USPersonalExpenditure)
y <- rownames(USPersonalExpenditure)
x <- colnames(USPersonalExpenditure)

# Define UI for application that draws a histogram
ui <- fluidPage(   
    titlePanel("US Expenditure Shiny App"),   
    navbarPage("Navbar",
               tabPanel("Plot",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("variable", "Please select an sector :", y),
                                selectInput("variable2", "Please select a year :", x),
                            ),
                            mainPanel(
                                plotOutput("secPlot"),
                                plotOutput("secPlot2")
                                
                            )
                        )
               ),
               tabPanel("Data",
                        DT::dataTableOutput("table")
               ),
               tabPanel("About",
                        textOutput("Us")
               )
               
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    sect <- reactive({ 
        sect <- USPersonalExpenditure[input$variable,]
    })
    
    output$Us <- renderText('Experimenting with shiny app using the dataset from R : USPersonalExpenditure')
    output$secPlot <- renderPlot({ 
        sector <- sect()
        plot(x,sector)
        title("Plot of US expenditure by sector")
    })
    
    slice <- reactive({
        slice <- USPersonalExpenditure[,input$variable2]
    })
    
    
    output$secPlot2 <- renderPlot({
        slices <- slice()
        lbls <- rownames(USPersonalExpenditure)
        pct <- round(slices/sum(slices)*100)
        lbls <- paste(lbls, pct) # add percents to labels
        lbls <- paste(lbls,"%",sep="") # ad % to labels
        pie(slices,labels = lbls, col=rainbow(length(lbls)),
            main="Pie Chart of US expenditure by year")  
    })
    
    output$table <- DT::renderDataTable({
        DT::datatable(USPersonalExpenditure)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)