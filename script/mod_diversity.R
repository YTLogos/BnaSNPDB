#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
mod_diversity_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      h2("Parameters:"),
      selectInput(ns("chr"), "Chromosome", selected = "A10", choices = chromosome),
      numericInput(ns("start"), "Start", value = "14900000"),
      numericInput(ns("end"), "End", value = "15150000"),
      numericInput(ns("maf"), "Minor Allele Frequency", value = 0.05),
      checkboxInput(ns("show"), "Show genes?", value = FALSE),
      checkboxGroupInput(ns("groups"), "Ecotypes to calculate diversity:", choices = c("Winter", "Semi-winter", "Spring"), selected = c("Winter", "Spring")),
      selectInput(ns("numerator"), "Numerator ecotype:", choices = c("Spring", "Semi-winter", "Winter")),
      selectInput(ns("denominator"), "Denominator ecotype:", choices = c("Winter", "Semi-winter", "Spring")),
      sliderInput(ns("step"), "Numbers of SNPs in each window:", value = 10, min = 5, max = 20),
      checkboxGroupInput(ns("div_mut_type"), "Select Mutation types:",
        choices = eff_type,
        selected = eff_type
      ),
      br(),
      actionButton(ns("submit"), strong("Submit"), styleclass = "success")
    ),
    mainPanel(
      h2("Diversity Plot:"),
      withSpinner(plotOutput(ns("div_plot"), height = 700), image = "img/custom.gif", image.width = 256, image.height = 256),
      br(),
      selectInput(ns("div_fig_format"),
        "Download figure as:",
        choices = available_fig_formats,
        selected = NULL,
        selectize = TRUE
      ),
      downloadButton(ns("div_fig_download"), "Download Diversity Plot"),
      br(),
      br(),
      br(),
      h2("Allele Informations (Major/Minor):"),
      withSpinner(DT::dataTableOutput(ns("div_allele_info")), image = "img/custom.gif", image.width = 256, image.height = 256),
      h5("ref: reference allele - The reference allele is whatever is found in the reference genome."),
      h5("alt: alternative allele - The alternative allele is the allele found in the sample."),
      h5("major: major allele, is the common allele with high frequency."),
      h5("minor: monir allele, is the less common allele with lower frequency."),
      selectInput(ns("div_snp_format"),
        "Download data as:",
        choices = available_data_formats,
        selected = NULL,
        selectize = TRUE
      ),
      downloadButton(ns("div_data_download"), "Download Allele Data"),
      br(),
      br(),
      br(),
      br()
    )
  )
}


#' Title
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
mod_diversity_server <- function(input, output, session) {
  ns <- session$ns
  par_list <- eventReactive(input$submit, {
    snp_chr <- input$chr
    snp_start <- input$start
    snp_end <- input$end
    groups <- input$groups
    numerator <- input$numerator
    denominator <- input$denominator
    step <- input$step
    muttype <- input$div_mut_type
    maf <- input$maf
    return(list(snp_chr, snp_start, snp_end, groups, numerator, denominator, step, muttype, maf))
  })
  diversity_plot <- reactive({
    diversity(chr = par_list()[[1]], start = par_list()[[2]], end = par_list()[[3]], groups = par_list()[[4]], numerator = par_list()[[5]], denominator = par_list()[[6]], step = par_list()[[7]], muttype = par_list()[[8]], maf = par_list()[[9]], show.gene = input$show)
  })
  allele.info <- eventReactive(input$submit, {
    extractallele(chr = input$chr, start = input$start, end = input$end, accession = input$groups, muttype = input$div_mut_type, maf = input$maf)[[2]]
  })
  output$div_plot <- renderPlot({
    diversity_plot()
  })

  output$div_allele_info <- renderDT({
    DT::datatable(allele.info()[, c(1:25)],
      rownames = FALSE,
      filter = "bottom",
      extensions = "FixedColumns",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 6),
        columnDefs = list(list(className = "dt-right", targets = "_all"))
      )
    )
  })

  #############  DT::datatable   download buttons  ##############
  # output$div_allele_info <- renderDT({
  #   DT::datatable(allele.info()[, c(1:10)],
  #                 extensions = 'Buttons', options = list(
  #                   dom = 'Bfrtip',
  #                   buttons =
  #                     list('copy', 'print', list(
  #                       extend = 'collection',
  #                       buttons = c('csv', 'excel', 'pdf'),
  #                       text = 'Download'
  #                     ))
  #
  #                 )
  #   )
  # })

  output$div_fig_download <- downloadHandler(
    filename = function() {
      glue::glue("{par_list()[[1]]}_{par_list()[[2]]}_{par_list()[[3]]}.diversity.{input$div_fig_format}")
    },
    content = function(file) {
      withProgress(
        message = "Download in progress",
        detail = "Please wait a while ...",
        value = 0,
        {
          for (i in 1:10) {
            incProgress(1 / 10)
            Sys.sleep(0.01)
          }
          if (input$div_fig_format == "png") {
            png(file, width = 10 * 300, height = 8 * 300, res = 300)
          } else if (input$div_fig_format == "pdf") {
            pdf(file, width = 12, height = 8, onefile = F)
          } else if (input$div_fig_format == "jpeg") {
            jpeg(file, width = 8 * 300, height = 8 * 300, res = 300)
          } else if (input$div_fig_format == "tiff") {
            tiff(file, width = 10 * 300, height = 8 * 300, res = 300)
          } else if (input$div_fig_format == "bmp") {
            bmp(file, width = 10 * 300, height = 8 * 300, res = 300)
          } else {
            svg(file)
          }
          print(diversity_plot())
          dev.off()
          if (input$div_fig_format == "pptx") { 
            doc <- read_pptx()
            doc <- add_slide(doc)
            doc <- ph_with(doc,diversity_plot(),location = ph_location_fullsize())
            print(doc, target = file)
          }
        }
      )
    }
  )

  output$div_data_download <- downloadHandler(
    filename = function() {
      glue::glue("{par_list()[[1]]}_{par_list()[[2]]}_{par_list()[[3]]}.allele.data.{input$div_snp_format}")
    },
    content = function(file) {
      if (input$div_snp_format == "txt") {
        write.table(allele.info(), file, row.names = F, col.names = T, quote = F)
      } else if (input$div_snp_format == "csv") {
        readr::write_csv(allele.info(), file, col_names = T)
      } else if (input$div_snp_format == "tsv") {
        readr::write_tsv(allele.info(), file, col_names = T)
      } else {
        writexl::write_xlsx(allele.info(), file, col_names = T)
      }
    }
  )
}
