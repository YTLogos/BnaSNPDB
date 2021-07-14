#' Title
#'
#' @param inputId
#' @param leftLabel
#' @param rightLabel
#' @param leftChoices
#' @param rightChoices
#' @param size
#' @param multiple
#'
#' @return
#' @export
#'
#' @examples
chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 2, multiple = FALSE) {
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)

  if (multiple) {
    multiple <- "multiple"
  } else {
    multiple <- NULL
  }

  tagList(
    singleton(tags$head(
      tags$script(src = "css/chooser-binding.js"),
      tags$style(
        type = "text/css",
        HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(
      id = inputId, class = "chooser",
      div(
        class = "chooser-container chooser-left-container",
        tags$select(class = "left", size = size, multiple = multiple, leftChoices, style = "width:100
px;background:gray50")
      ),
      div(
        class = "chooser-container chooser-center-container",
        icon("arrow-circle-o-right", "right-arrow fa-2x"),
        tags$br(),
        icon("arrow-circle-o-left", "left-arrow fa-2x")
      ),
      div(
        class = "chooser-container chooser-right-container",
        tags$select(class = "right", size = size, multiple = multiple, rightChoices, style = "width:100px;background:gray50")
      )
    )
  )
}

registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
  if (is.null(data)) {
    NULL
  } else {
    list(unselected = as.character(data$left), selected = as.character(data$right))
  }
}, force = TRUE)
