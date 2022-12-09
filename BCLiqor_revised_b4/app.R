library(shiny)
library(ggplot2)
library(dplyr)
library("shinyWidgets")
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)
print(str(bcl))
ui <- fluidPage(
  # Feature 3
  # Logo Image
  titlePanel(title=p(img(src="no.jpeg",  height="70em", width="70em", align="right"), "Liquor Store Overview")),
  tabsetPanel(
    tabPanel("Overview By Country",
             sidebarLayout(
               sidebarPanel(
                 h3("Introduction:"),
                 h4("Here is an overview of the BC liquor data"),
                 h4("This app aims to help you explore drinks in BC liquor store and help you find out what drink you may like"),
                 h4("Feel free to use the second tab to explore and find out the drink you want"),
                 h3("Data Overview:"),
                 h4("Total Drinks in record:" , nrow(bcl)),
                 h4("Drinks are from" , n_distinct(bcl$Country), "different countries")
               ),
               # Feature 2
               mainPanel(br(),
                         h3("Overview in a bar graph"),
                         br(),
                         plotOutput("overviewPlot")),
             )),
    tabPanel("Filtered Overview", sidebarLayout(
      sidebarPanel(
        h3(textOutput("text"), style="color:purple"),
        sliderInput("priceInput", "Price", min = 0, max = 100,
                    value = c(25, 40), pre = "$"),
        radioButtons("typeInput", "Product type",
                     choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                     selected = "WINE"),
        uiOutput("countryOutput"),
        # Feature 1
        sliderInput("sweetnessInput", "Sweetness", min = 0, max = 10,
                    value = c(1, 5)),
        # Feature 4 -> if Alcohol content is less than 15 it is considered to be not intensive
        numericInput("acInput", "Alcohol Content <=", 15, 1, 50),
        conditionalPanel(
          "input.acInput <= 20",
          "Mild Lover!",
        ),
        downloadButton("downloadData", "Download Table"),
      ),
      # Feature 2
      mainPanel(h3("Graph View"),
        plotOutput("coolplot"),
                br(), br(),
                h3("Table View"),
                dataTableOutput("results")),
    ))
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
             Country %in% input$countryInput
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

  output$overviewPlot <- renderPlot({
    ggplot(bcl, aes(x=factor(1), fill=Country))+
      geom_bar(width = 1)+
      coord_polar("y") +
      labs(y= "Count", x ="")
  })

  output$results <- renderDataTable({
    filtered()
  })

  output$countryOutput <- renderUI({
    pickerInput("countryInput", "Country",
                choices = sort(unique(bcl$Country)), selected = c("CANADA", "AUSTRIA"), options = list(`actions-box` = TRUE, `select-all-text` = "Select All"), multiple = TRUE)
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
