titleBox <- function(title) {
  fluidRow(
    box(
      width = 12, background = "yellow",
      span(strong(title),
        style = "font-size:24px"
      )
    )
  )
}

sectionBox <- function(..., title) {
  fluidRow(
    box(...,
      width = 12,
      title = title,
      solidHeader = TRUE, status = "warning", collapsible = TRUE
    )
  )
}

textBox <- function(...) {
  box(..., status = "success")
}

messageBox <- function(...) {
  box(..., status = "danger", background = "green")
}

module_Box <- function(..., title, imgSrc, text) {
  box(
    ...,
    title = span(title, style = "font-size:18px"),
    solidHeader = TRUE, status = "primary",
    fluidRow(
      column(
        width = 4,
        div(style = "margin-left:5px;margin-right:0px;margin-top:0px;",shiny::img(src = imgSrc, width = "100%"))
      ),
      column(
        width = 8,
        p(text),
      )
    )
  )
}
