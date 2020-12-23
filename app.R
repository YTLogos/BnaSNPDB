source("global.R")
ui <- fluidPage(
  tags$script(src="css/addhash.js"),
  div(img(src = "img/bnasnpdb_logo.png")),
  includeCSS("www/css/custom.css"),
  includeCSS("www/css/footer.css"),
  disconnectMessage(
    text = "Your session timed out, reload the application!",
    refresh = "Reload now",
    background = "#f89f43",
    colour = "white",
    overlayColour = "grey",
    overlayOpacity = 0.75,
    top = 250,
    refreshColour = "brown"
  ),
  navbarPage(
    title = "",
    windowTitle = "BnaSNPDB",
    theme = shinytheme("flatly"),
    tabPanel("Home", homepage, icon = icon("home")),
    tabPanel("LDheatmap", mod_ldheatmap_ui("ld")),
    tabPanel("SNPdistribution", mod_snpdistribution_ui("dis")),
    tabPanel("Phylogenetics", mod_phylogenetics_ui("tree")),
    tabPanel("Diversity", mod_diversity_ui("diversity")),
    tabPanel("Extraction", mod_extraction_ui("extract"), icon = icon("search")),
    tabPanel("Documentation", mod_doc_ui("doc"), icon = icon("file-text")),
    tabPanel("About", mod_about_ui("about"), icon = icon("info-circle")),
    footer = footerTagList
  )
)

server <- function(input, output, session) {
  callModule(mod_ldheatmap_server, "ld")
  callModule(mod_snpdistribution_server, "dis")
  callModule(mod_phylogenetics_server, "tree")
  callModule(mod_diversity_server, "diversity")
  callModule(mod_extraction_server, "extract")
  observeEvent(input$disconnect, {
    session$close()
  })
}

shinyApp(ui, server)
