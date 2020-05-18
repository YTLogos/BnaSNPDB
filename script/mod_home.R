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
      valueBox("1,007", "Germplasm Collections", color = "purple", width = 2),
      valueBox(39, "Countries", color = "yellow", width = 2),
      valueBox(658, "Winter Ecotypes", color = "fuchsia", width = 2),
      valueBox(145, "Semi-winter Ecotypes", color = "navy", width = 2),
      valueBox(188, "Spring Ecotypes", color = "olive", width = 2)
    ),
    column(width= 8,includeMarkdown("www/home.md"), offset = 2)
    )
}

mod_home_server <- function(input, output, session) {
  ns <- session$ns
}
