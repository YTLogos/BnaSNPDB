#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
mod_snpdistribution_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      br(),
      br(),
      HTML("<h4><font color='red'>Parameters:</font></h4>"),
      fileInput(ns("sample_type"), "Choose file. Leave blank for example run.",
        multiple = FALSE,
        accept = c(
          "text/txt",
          "text/comma-separated-values,text/plain",
          ".txt"
        ),
        placeholder = "data/Other_data/sample_group_info.txt"
      ),
      selectInput(ns("chr"), "Chromosome", selected = "A10", choices = chromosome),
      numericInput(ns("start"), "Start", value = "14990000"),
      numericInput(ns("end"), "End", value = "15010000"),
      numericInput(ns("maf"), "Minor Allele Frequency", value = 0.05),
      selectInput(ns("reverse"), "Reverse", choices = list("TRUE", "FALSE"), selected = "FALSE"),
      checkboxGroupInput(ns("mut_type"), h4("Select Mutation types:"),
        choices = eff_type,
        selected = eff_type
      ),
      br(),
      actionButton(ns("submit"), strong("Submit"), styleclass = "success")
    ),
    mainPanel(
      br(),
      br(),
      tabsetPanel(
        tabPanel(h3("Results"),
                 h3("SNPdistribution:"),
                 withSpinner(plotOutput(ns("snp_dis")), type = 4),
                 selectInput(ns("snp_fig_format"),
                             "Download figure as:",
                             choices = available_fig_formats,
                             selected = NULL,
                             selectize = TRUE
                 ),
                 downloadButton(ns("snp_fig_download"), "Download SNPdistribution"),
                 br(),
                 br(),
                 br(),
                 h3("SNP Informations:"),
                 withSpinner(DT::dataTableOutput(ns("dis_snp_info")), type = 7),
                 selectInput(ns("dis_snp_format"),
                             "Download data as:",
                             choices = available_data_formats,
                             selected = NULL,
                             selectize = TRUE
                 ),
                 downloadButton(ns("snpdis_data_download"), "Download SNP Data"),
                 br(),
                 br(),
                 br(),
                 br()
                 ),
        tabPanel(h3("Instruction"), includeMarkdown("www/home.md"))
      )
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
mod_snpdistribution_server <- function(input, output, session) {
  ns <- session$ns
  par_list <- eventReactive(input$submit, {
    snp_chr <- input$chr
    snp_start <- input$start
    snp_end <- input$end
    return(list(snp_chr, snp_start, snp_end))
  })
  sample_info <- eventReactive(input$submit, {
    readNewData(fileinfo = input$sample_type)
  })

  snp_data <- eventReactive(input$submit, {
    extractsnp(
      chr = input$chr, start = input$start, end = input$end, muttype = input$mut_type,
      accession = as.character(sample_info()$accession), maf = input$maf
    )
  })

  snp_distribution_plot <- reactive({
    snpmat <- as.data.frame(as.matrix(snp_data()[[1]]))
    snppos <- snp_data()[[2]]
    snpmat <- cbind(snppos, snpmat)
    snpmat_1 <- snpmat %>% gather(key = "accession", value = allele, -snppos)
    snpmat_info <- left_join(snpmat_1, sample_info(), by = "accession")
    snpmat_info$allele <- factor(snpmat_info$allele)
    snpmat_info$snppos <- factor(snpmat_info$snppos)
    if (input$reverse) {
      snpmat_info$snppos <- forcats::fct_rev(snpmat_info$snppos)
    }
    snp_distribution(snp_data = snpmat_info)
  })

  output$snp_dis <- renderPlot({
    print(snp_distribution_plot())
  })

  output$dis_snp_info <- renderDT({
    DT::datatable(snp_data()[[3]][, c(1:10)],
      rownames = FALSE,
      filter = "bottom",
      selection = "single",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(list(className = "dt-right", target = "_all"))
      )
    )
  })

  ############ download SNPdistribution ############
  output$snp_fig_download <- downloadHandler(
    filename = function() {
      glue::glue("{par_list()[[1]]}_{par_list()[[2]]}_{par_list()[[3]]}.SNPdistribution.{input$snp_fig_format}")
    },
    content = function(file) {
      if (input$snp_fig_format == "png") {
        png(file, width = 10 * 300, height = 8 * 300, res = 300)
      } else if (input$snp_fig_format == "pdf") {
        pdf(file, width = 12, height = 8, onefile = F)
      } else if (input$snp_fig_format == "jpeg") {
        jpeg(file, width = 8 * 300, height = 8 * 300, res = 300)
      } else if (input$snp_fig_format == "tiff") {
        tiff(file, width = 10 * 300, height = 8 * 300, res = 300)
      } else if (input$snp_fig_format == "bmp") {
        bmp(file, width = 10 * 300, height = 8 * 300, res = 300)
      } else {
        svg(file)
      }
      print(snp_distribution_plot())
      dev.off()
    }
  )

  ####### download SNP data ##########
  output$snpdis_data_download <- downloadHandler(
    filename = function() {
      glue::glue("{par_list()[[1]]}_{par_list()[[2]]}_{par_list()[[3]]}.SNP.data.{input$dis_snp_format}")
    },
    content = function(file) {
      if (input$dis_snp_format == "txt") {
        write.table(snp_data()[[3]], file, row.names = F, col.names = T, quote = F)
      } else if (input$dis_snp_format == "csv") {
        readr::write_csv(snp_data()[[3]], file, col_names = T)
      } else if (input$dis_snp_format == "tsv") {
        readr::write_tsv(snp_data()[[3]], file, col_names = T)
      } else {
        writexl::write_xlsx(snp_data()[[3]], file, col_names = T)
      }
    }
  )
}
