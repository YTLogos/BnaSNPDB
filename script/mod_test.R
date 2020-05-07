xx_mod_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    sidebarPanel(
      width = 3,
      numericInput(ns("start"),"Start", value = "14990000"),
      numericInput(ns("end"),"End", value = "15010000"),
      numericInput(ns("maf"),"Minor Allele Frequency", value = 0.05),
      radioButtons(ns("flip"),"Flip The Figure", list("FALSE"=0, "TRUE"=1)),
    mainPanel(
      plotOutput(ns("plot1"))
    )
  )
  )
}

xx_mod_server <- function(input, output, session){
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
}