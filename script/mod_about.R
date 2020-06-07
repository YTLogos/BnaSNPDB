mod_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 5, offset = 1,
      includeMarkdown("www/md/about.md")
    ),
    column(
      width = 5,
      includeMarkdown("www/md/tech.md")
    )
  )
}

mod_about_server <- function(input, output, session) {
  ns <- session$ns
}
