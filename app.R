source("global.R")

ui <- fluidPage(
  div(img(src = "img/database_logo.png")),
  includeCSS("www/css/custom.css"),
  includeCSS("www/css/footer.css"),
  navbarPage(
    title = "",
    windowTitle = "SNP database of 1007 rapeseed germplasm accessions",
    theme = shinytheme("flatly"),
    tabPanel("Home", homepage, icon = icon("home")),
    tabPanel("LDheatmap", mod_ldheatmap_ui("ld")),
    tabPanel("SNP_distribution", mod_snpdistribution_ui("dis")),
    tabPanel("Phylogenetics", mod_phylogenetics_ui("tree")),
    tabPanel("Diversity", mod_diversity_ui("diversity")),
    tabPanel("Extraction", mod_extraction_ui("extract"), icon = icon("search")),
    tabPanel("Documentation", icon = icon("file-text")),
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
}

shinyApp(ui, server)
