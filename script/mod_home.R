#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBox("2,404,340", "High Quality SNPs", width = 2),
      valueBox("1,007", "Germplasm Collection", color = "purple", width = 2),
      valueBox(39, "Countries Around The World", color = "yellow", width = 2),
      valueBox(700, "Winter Ecotype", color = "fuchsia", width = 2),
      valueBox(105, "Semi-winter Ecotype", color = "navy", width = 2),
      valueBox(185, "Spring Ecotype", color = "olive", width = 2)
    ),
    column(includeMarkdown("www/home.md"), width = 12)
  )
}

mod_home_server <- function(input, output, session) {
  ns <- session$ns
}
