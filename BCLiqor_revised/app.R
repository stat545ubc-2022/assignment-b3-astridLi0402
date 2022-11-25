library(shiny)
library(ggplot2)
library(dplyr)
library(datateachr)
library(tidyverse)
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)
print(str(bcl))
ui <- fluidPage(
  # Logo Image
  titlePanel(title=p(img(src="no.jpeg",  height="70em", width="70em", align="right"), "Liquor Store Overview")),
  tabsetPanel(
    tabPanel("Filtered Overview", sidebarLayout(
      sidebarPanel(
        span(textOutput("text"), style="color:red"),
        sliderInput("priceInput", "Price", min = 0, max = 100,
                    value = c(25, 40), pre = "$"),
        radioButtons("typeInput", "Product type",
                     choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                     selected = "WINE"),
        uiOutput("countryOutput"),
        # Feature 1
        sliderInput("sweetnessInput", "Switness", min = 0, max = 10,
                    value = c(1, 5)),
        # Feature 4 -> if Alcohol content is less than 15 it is considered to be not intensive
        numericInput("acInput", "Alcohol Content <=", 15, 1, 50),
        conditionalPanel(
          "input.acInput <= 20",
          "Mild Lover!",
        ),
      ),
      # Feature 2
      mainPanel(plotOutput("coolplot"),
                br(), br(),
                downloadLink('downloadData', 'Click to download the table'),
                tableOutput("results")),
    )),
    # Feature 3
    tabPanel("Overview By Country", plotOutput("testplot"))
  )
  )
server <- function(input, output) {
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }

    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Sweetness >= input$sweetnessInput[1],
             Sweetness <= input$sweetnessInput[2],
             Alcohol_Content <= input$acInput,
             Type == input$typeInput,
             Country == input$countryInput
      )
  })
  output$text <- renderText({
    paste("Total Results Found: ", nrow(filtered()))
    })

  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })

  output$testplot <- renderPlot({
    ggplot(bcl, aes(x=factor(1), fill=Country))+
      geom_bar(width = 1)+
      coord_polar("y") +
      labs(y= "Count", x ="")
  })

  output$results <- renderTable({
    filtered()
  })

  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
      },
    content = function(con) {
      write.csv(filtered(), con)
      }
    )
}
shinyApp(ui = ui, server = server)
