#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
mod_extraction_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      h2("Parameters:"),
      selectInput(ns("type"), "What do you want to extract?", choices = c("SNP", "Gene", "Accession")),


      conditionalPanel(
        condition = "input.type=='SNP'",
        ns = ns,
        selectInput(ns("chr"), "Chromosome", selected = "A10", choices = chromosome),
        numericInput(ns("start"), "Start", value = "14990000"),
        numericInput(ns("end"), "End", value = "15010000"),
        numericInput(ns("maf"), "Minor Allele Frequency", value = 0.05),
        h4(strong("Select samples:")),
        checkboxInput(ns("upload_sample"), "Upload Your Samples?", FALSE),
        conditionalPanel(
          condition = "input.upload_sample",
          ns = ns,
          fileInput(ns("sample"), "Upload samples.Leave blank for example run.",
            multiple = FALSE,
            accept = c(
              "text/txt",
              "text/comma-separated-values,text/plain",
              ".txt"
            ),
            placeholder = "data/Other_data/Core.var.txt"
          )
        ),

        conditionalPanel(
          condition = "!input.upload_sample",
          ns = ns,
          chooserInput(ns("snp_accession"), "Available frobs", "Selected frobs", c(), all.var.info, size = 10, multiple = TRUE)
        ),
        checkboxGroupInput(ns("muttype"), "Select Mutation types:", choices = eff_type, selected = eff_type),
        actionButton(ns("snp_submit"), strong("Submit"), styleclass = "success")
      ),

      conditionalPanel(
        condition = "input.type=='Gene'",
        ns = ns,
        selectInput(ns("ref"), "Reference Genome", choices = c("Darmor", "ZS11", "NY7", "Tapidor")),
        checkboxInput(ns("singlegene"), "Extract single gene?", FALSE),

        conditionalPanel(
          condition = "!input.singlegene",
          ns = ns,
          fileInput(ns("genecluster"), "Upload genes.Leave blank for example run.",
            multiple = FALSE,
            accept = c(
              "text/txt",
              "text/comma-separated-values,text/plain",
              ".txt"
            ),
            placeholder = "data/Other_data/darmor_test_gene.txt"
          )
        ),
        conditionalPanel(
          condition = "input.singlegene",
          ns = ns,
          textInput(ns("gene"), "Enter gene name (e.g, BnaA10g22080D):", value = "BnaA10g22080D")
        ),
        checkboxGroupInput(ns("database"), "Select annotation database", choices = database_type, selected = database_type),
        actionButton(ns("gene_submit"), strong("Submit"), styleclass = "success")
      ),


      conditionalPanel(
        condition = "input.type=='Accession'",
        ns = ns,
        h4(strong("Select samples:")),
        checkboxInput(ns("upload_sample_1"), "Upload Your Samples?", FALSE),
        conditionalPanel(
          condition = "input.upload_sample_1",
          ns = ns,
          fileInput(ns("sample"), "Upload samples.Leave blank for example run.",
            multiple = FALSE,
            accept = c(
              "text/txt",
              "text/comma-separated-values,text/plain",
              ".txt"
            ),
            placeholder = "data/Other_data/Core.var.txt"
          )
        ),
        conditionalPanel(
          condition = "!input.upload_sample_1",
          ns = ns,
          "Select Accessions:",
          chooserInput(ns("accession_select"), "Available frobs", "Selected frobs", leftChoices = c(), rightChoices = all.var.info, size = 10, multiple = TRUE),
        ),
        br(),
        actionButton(ns("accession_submit"), strong("Submit"), styleclass = "success")
      ),
      br(),
    ),


    mainPanel(
      conditionalPanel(
        h2("SNP data:"),
        condition = "input.type=='SNP'",
        ns = ns,
        withSpinner(DT::dataTableOutput(ns("snp_info")), type = 7),
        br(),
        selectInput(ns("snp_format"),
          "Download data as:",
          choices = available_data_formats,
          selected = NULL,
          selectize = TRUE
        ),
        downloadButton(ns("snpdata_download"), "Download SNP Data")
      ),

      conditionalPanel(
        h2("Gene annotation:"),
        condition = "input.type=='Gene'",
        ns = ns,
        withSpinner(DT::dataTableOutput(ns("gene_info")), type = 7),
        br(),
        selectInput(ns("gene_format"),
          "Download data as:",
          choices = available_data_formats,
          selected = NULL,
          selectize = TRUE
        ),
        downloadButton(ns("gene_download"), "Download Genes Informations"),
        br(),
        br(),
        br(),
        br()
      ),

      conditionalPanel(
        h2("Geographic distribution:"),
        condition = "input.type=='Accession'",
        ns = ns,
        withSpinner(plotOutput(ns("accession_dis")), type = 7),
        br(),
        selectInput(ns("sample_fig_format"),
          "Download figure as:",
          choices = available_fig_formats,
          selected = NULL,
          selectize = TRUE
        ),
        downloadButton(ns("sample_fig_download"), "Download Geographic distribution"),
        br(),
        br(),
        h2("SNP data:"),
        withSpinner(DT::dataTableOutput(ns("aceession_info")), type = 7),
        br(),
        selectInput(ns("sample_format"),
          "Download data as:",
          choices = available_data_formats,
          selected = NULL,
          selectize = TRUE
        ),
        downloadButton(ns("accession_download"), "Download Accession Informations"),
        br(),
        br(),
        br(),
        br()
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
mod_extraction_server <- function(input, output, session) {
  ns <- session$ns
  sample <- reactive({
    if (input$upload_sample) {
      sample <- readNewData_sample(fileinfo = input$sample)
      id <- as.character(sample$V1)
    } else {
      sample <- sapply(input$ld_accession$selected, function(x) {
        if (x %in% c("Winter", "Semi-winter", "Spring", "Core")) {
          var <- readLines(paste0("./data/Other_data/", x, ".var.txt"))
          return(var)
        } else {
          return(x)
        }
      })
      id <- unique(unlist(sample))
    }
    return(id)
  })

  snp_data <- eventReactive(input$snp_submit, {
    extractsnp(chr = input$chr, start = input$start, end = input$end, accession = sample(), muttype = input$muttype, maf = input$maf)
  })

  output$snp_info <- renderDT({
    DT::datatable(snp_data()[[3]][, c(1:10)],
      rownames = FALSE,
      filter = "bottom",
      options = list(
        scrollX = TRUE,
        autoWidth = FALSE,
        columnDefs = list(list(className = "dt-right", target = "_all"))
      ), escape = FALSE
    )
  })

  # output$snp_info <- DT::renderDataTable(
  #   datatable( data = snp_data()[[3]]
  #              , extensions = 'Buttons',
  #              options = list(
  #                dom = "Blfrtip", buttons =
  #                  list("copy", list(
  #                    extend = "collection"
  #                    , buttons = c("csv", "excel", "pdf")
  #                    , text = "Download",
  #                    server=FALSE
  #                  ) ) # end of buttons customization
  #
  #                # customize the length menu
  #                , lengthMenu = list( c(10, 20, -1) # declare values
  #                                     , c(10, 20, "All") # declare titles
  #                ) # end of lengthMenu customization
  #                , pageLength = 10
  #
  #
  #              ) # end of options
  #
  #   ) # end of datatables
  # )



  output$snpdata_download <- downloadHandler(
    filename = function() {
      glue::glue("{input$chr}_{input$start}_{input$end}.SNP.data.{input$snp_format}")
    },
    content = function(file) {
      if (input$snp_format == "txt") {
        write.table(snp_data()[[3]], file, row.names = F, col.names = T, quote = F)
      } else if (input$snp_format == "csv") {
        readr::write_csv(snp_data()[[3]], file, col_names = T)
      } else if (input$snp_format == "tsv") {
        readr::write_tsv(snp_data()[[3]], file, col_names = T)
      } else {
        writexl::write_xlsx(snp_data()[[3]], file, col_names = T)
      }
    }
  )


  gene_sample <- eventReactive(input$gene_submit, {
    # gene_file <- switch(input$ref, Darmor="darmor_gene_anno",ZS11="zs11_gene_anno",NY7="ny7_gene_anno",Tapidor="tapidor_gene_anno")
    # return(gene_file)
    if (input$ref == "Darmor") {
      gene_file <- darmor_gene_anno
    } else if (input$ref == "ZS11") {
      gene_file <- zs11_gene_anno
    } else if (input$ref == "NY7") {
      gene_file <- ny7_gene_anno
    } else {
      gene_file <- tapidor_gene_anno
    }
    return(gene_file)
  })

  gene_id <- eventReactive(input$gene_submit, {
    if (input$singlegene) {
      id <- as.character(input$gene)
    } else {
      df <- readNewData_gene(fileinfo = input$genecluster)
      id <- as.character(df$V1)
    }
    return(id)
  })

  database_type <- eventReactive(input$gene_submit, {
    d <- c("geneid", "chr", "start", "end", "Predicted_gene_name", input$database)
    return(d)
  })

  gene_anno <- eventReactive(input$gene_submit, {
    gene_sample()[gene_sample()[, 1] %in% gene_id(), colnames(gene_sample()) %in% database_type()]
  })

  output$gene_info <- renderDT({
    DT::datatable(gene_anno(),
      rownames = FALSE,
      filter = "bottom",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        fixedColumns = TRUE,
        columnDefs = list(list(className = "dt-right", target = "_all"))
      ),
      class = "white-space: nowrap"
    )
  })

  output$gene_download <- downloadHandler(
    filename = function() {
      glue::glue("{input$ref}.gene.anno.{input$gene_format}")
    },
    content = function(file) {
      if (input$gene_format == "txt") {
        write.table(gene_anno(), file, row.names = F, col.names = T, quote = F)
      } else if (input$gene_format == "csv") {
        readr::write_csv(gene_anno(), file, col_names = T)
      } else if (input$gene_format == "tsv") {
        readr::write_tsv(gene_anno(), file, col_names = T)
      } else {
        writexl::write_xlsx(gene_anno(), file, col_names = T)
      }
    }
  )



  sample_selected <- eventReactive(input$accession_submit, {
    if (input$upload_sample_1) {
      sample <- readNewData_sample(fileinfo = input$sample)
      id <- as.character(sample$V1)
    } else {
      sample <- sapply(input$accession_select$selected, function(x) {
        if (x %in% c("Winter", "Semi-winter", "Spring", "Core")) {
          var <- readLines(paste0("./data/Other_data/", x, ".var.txt"))
          return(var)
        } else {
          return(x)
        }
      })
      id <- unique(unlist(sample))
    }
    return(id)
  })

  sample_info <- eventReactive(input$accession_submit, {
    sample <- sample_geographic_info[sample_geographic_info[, 1] %in% sample_selected(), , drop = FALSE]
    return(sample)
  })

  geographic_plot <- eventReactive(input$accession_submit, {
    p <- geographic_map +
      geom_point(data = sample_info(), aes(long, lat, color = Type, group = Type), size = 1.5)
    return(p)
  })


  output$accession_dis <- renderPlot({
    print(geographic_plot())
  })
  output$aceession_info <- renderDT({
    DT::datatable(sample_info()[, -c(5, 6)],
      rownames = FALSE,
      filter = "bottom",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(className = "dt-center", target = "_all"))
      ), escape = FALSE
    )
  })

  output$sample_fig_download <- downloadHandler(
    filename = function() {
      glue::glue("The geographic distribution of rapeseed accessions.{input$sample_fig_format}")
    },
    content = function(file) {
      if (input$sample_fig_format == "png") {
        png(file, width = 12 * 300, height = 6 * 300, res = 300)
      } else if (input$sample_fig_format == "pdf") {
        pdf(file, width = 12, height = 6, onefile = F)
      } else if (input$sample_fig_format == "jpeg") {
        jpeg(file, width = 12 * 300, height = 6 * 300, res = 300)
      } else if (input$sample_fig_format == "tiff") {
        tiff(file, width = 12 * 300, height = 6 * 300, res = 300)
      } else if (input$sample_fig_format == "bmp") {
        bmp(file, width = 12 * 300, height = 6 * 300, res = 300)
      } else {
        svg(file)
      }
      print(geographic_plot())
      dev.off()
    }
  )

  output$accession_download <- downloadHandler(
    filename = function() {
      glue::glue("Rapeseed.selected_accession.info.{input$sample_format}")
    },
    content = function(file) {
      if (input$sample_format == "txt") {
        write.table(sample_info(), file, row.names = F, col.names = T, quote = F)
      } else if (input$sample_format == "csv") {
        readr::write_csv(sample_info(), file, col_names = T)
      } else if (input$sample_format == "tsv") {
        readr::write_tsv(sample_info(), file, col_names = T)
      } else {
        writexl::write_xlsx(sample_info(), file, col_names = T)
      }
    }
  )
}
