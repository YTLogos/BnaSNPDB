library(shiny)
ui <- fluidPage(
  # tags$head(tags$style('p {color:black;}')),
  includeCSS("www/custom.css"),
  navbarPage(
    title = h2("BnaSNPDB"),
    position = "fixed-top",
    header = tagList(useShinydashboard()),
    windowTitle = "SNP database of 1007 rapeseed germplasm accessions",
    theme = shinytheme("flatly"),
    tabPanel(h3("Home"), mod_home_ui("home")),
    tabPanel(h3("LDheatmap"), mod_ldheatmap_ui("ld")),
    tabPanel(h3("SNP_distribution"), mod_snpdistribution_ui("dis")),
    tabPanel(h3("Phylogenetics"), mod_phylogenetics_ui("tree")),
    tabPanel(h3("Diversity"), mod_diversity_ui("diversity")),
    tabPanel(h3("Extraction"), mod_extraction_ui("extract")),
    tabPanel(h3("Documentation")),
    tabPanel(h3("About"))
  )
)

server <- function(input, output, session) {
  callModule(mod_ldheatmap_server, "ld")
  callModule(mod_snpdistribution_server, "dis")
  callModule(mod_phylogenetics_server, "tree")
  callModule(mod_diversity_server, "diversity")
  callModule(mod_extraction_server, "extract")
}

shinyApp(ui, server)
