mod_doc_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(
      width = 10,
      offset = 1,
      includeMarkdown("www/md/documentation.md")
    )
  )
}

mod_doc_server <- function(input, output, session){
  ns <- session$ns
}