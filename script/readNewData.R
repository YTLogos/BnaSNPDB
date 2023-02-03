#' Title
#'
#' @param fileinfo
#'
#' @return
#' @export
#'
#' @examples
readNewData <- function(fileinfo) {
  if (is.null(fileinfo)) {
    fileinfo <- list(name = "sample_group_info.txt", size = 1, type = "text/txt", datapath = "data/Other_data/sample_group_info.txt")
  }
  newdata <- read.table(file = fileinfo$datapath, header = FALSE, stringsAsFactors = FALSE, col.names = c("accession", "type"))
  return(newdata)
}

readNewData_gene <- function(fileinfo) {
  if (is.null(fileinfo)) {
    fileinfo <- list(name = "darmor_test_gene.txt", size = 1, type = "text/txt", datapath = "data/Other_data/darmor_test_gene.txt")
  }
  newdata <- read.table(file = fileinfo$datapath, header = FALSE, stringsAsFactors = FALSE)
  return(newdata)
}

readNewData_sample <- function(fileinfo) {
  if (is.null(fileinfo)) {
    fileinfo <- list(name = "Core.var.txt", size = 1, type = "text/txt", datapath = "data/Other_data/Core.var.txt")
  }
  newdata <- read.table(file = fileinfo$datapath, header = FALSE, stringsAsFactors = FALSE)
  return(newdata)
}