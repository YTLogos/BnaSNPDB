mod_extraction_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      br(),
      br(),
      HTML("<h4><font color='red'>Parameters:</font></h4>"),
      selectInput(ns("type"), "What do you want to extract?", choices = c("SNP", "Gene", "Accession")),
      conditionalPanel(
        condition = "input.type=='SNP'",
        ns = ns,
        selectInput(ns("chr"), "Chromosome", selected = "A10", choices = chromosome),
        numericInput(ns("start"), "Start", value = "14990000"),
        numericInput(ns("end"), "End", value = "15010000"),
        numericInput(ns("maf"), "Minor Allele Frequency", value = 0.05),
        radioButtons(ns("snp_sample_choose"), "Do You Need Upload Your Samples?", list("FALSE" = 0, "TRUE" = 1), selected = "0"),
        conditionalPanel(
          condition = "input.snp_sample_choose==1",
          ns = ns,
          fileInput(ns("snp_sample"), "Upload Your Samples.Leave blank for example run.",
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
          condition = "input.snp_sample_choose==0",
          ns = ns,
          chooserInput(ns("snp_accession"), "Available frobs", "Selected frobs", leftChoices = c(), rightChoices = all.var.info, size = 10, multiple = TRUE)
        ),
        checkboxGroupInput(ns("mut_type"), h4("Select Mutation types:"), choices = eff_type, selected = eff_type),
        actionButton(ns("snp_submit"), strong("Submit"), styleclass = "success")
      ),
      conditionalPanel(
        condition = "input.type=='Gene'",
        ns = ns,
        selectInput(ns("ref"), "Reference Genome", choices = c("Darmor-bzh", "ZS11", "NY7", "Tapidor")),
        checkboxInput(ns("singlegene"), "Extract single gene?", FALSE),

        conditionalPanel(
          condition = "!input.singlegene",
          ns = ns,
          fileInput(ns("genecluster"), "Upload genes: ",
            multiple = FALSE,
            accept = c(
              "text/txt",
              "text/comma-separated-values,text/plain",
              ".txt"
            ),
            placeholder = "data/Other_data/gene_info.txt"
          )
        ),
        conditionalPanel(
          condition = "input.singlegene",
          ns = ns,
          textInput(ns("gene"), "Enter gene name:", value = "BnaA10g22080D")
        ),

        #   conditionalPanel(
        #     condition = "!input.singlegene",
        #     ns = ns,
        #     fileInput(ns("genecluster"), "Upload genes: ",
        #       multiple = FALSE,
        #       accept = c(
        #         "text/txt",
        #         "text/comma-separated-values,text/plain",
        #         ".txt"
        #       )
        #     )
        #   )
        # ),
        # conditionalPanel(
        #   condition = "input.ref=='ZS11'",
        #   ns = ns,
        #   checkboxInput(ns("singlegene1"), "Single gene?", FALSE),
        #   conditionalPanel(
        #     condition = "input.singlegene1",
        #     ns = ns,
        #     textInput(ns("gene1"), "Enter gene name:", value = "BnaA10T0110200ZS")
        #   ),
        #   conditionalPanel(
        #     condition = "!input.singlegene1",
        #     ns = ns,
        #     fileInput(ns("genecluster1"), "Upload genes: ",
        #       multiple = FALSE,
        #       accept = c(
        #         "text/txt",
        #         "text/comma-separated-values,text/plain",
        #         ".txt"
        #       )
        #     )
        #   )
        # ),
        # conditionalPanel(
        #   condition = "input.ref=='NY7'",
        #   ns = ns,
        #   checkboxInput(ns("singlegene2"), "Single gene?", FALSE),
        #   conditionalPanel(
        #     condition = "input.singlegene2",
        #     ns = ns,
        #     textInput(ns("gene2"), "Enter gene name:", value = "chrA01g000670.t1")
        #   ),
        #   conditionalPanel(
        #     condition = "!input.singlegene2",
        #     ns = ns,
        #     fileInput(ns("genecluster2"), "Upload genes: ",
        #       multiple = FALSE,
        #       accept = c(
        #         "text/txt",
        #         "text/comma-separated-values,text/plain",
        #         ".txt"
        #       )
        #     )
        #   )
        # ),
        # conditionalPanel(
        #   condition = "input.ref=='Tapidor'",
        #   ns = ns,
        #   checkboxInput(ns("singlegene3"), "Single gene?", FALSE),
        #   conditionalPanel(
        #     condition = "input.singlegene3",
        #     ns = ns,
        #     textInput(ns("gene3"), "Enter gene name:", value = "BnaA07g00690.1T")
        #   ),
        #   conditionalPanel(
        #     condition = "!input.singlegene3",
        #     ns = ns,
        #     fileInput(ns("genecluster3"), "Upload genes: ",
        #       multiple = FALSE,
        #       accept = c(
        #         "text/txt",
        #         "text/comma-separated-values,text/plain",
        #         ".txt"
        #       )
        #     )
        #   )
        # ),

        checkboxGroupInput(ns("database"), "Select annotation database", choices = database_type, selected = database_type),
        actionButton(ns("gene_submit"), strong("Submit"), styleclass = "success")
      ),
      conditionalPanel(
        condition = "input.type=='Accession'",
        ns = ns,
        checkboxInput(ns("upload_sample"), "Upload Your Samples?", FALSE),
        conditionalPanel(
          condition = "input.upload_sample",
          ns = ns,
          fileInput(ns("sample"), "Upload samples: ",
            multiple = FALSE,
            accept = c(
              "text/txt",
              "text/comma-separated-values,text/plain",
              ".txt"
            )
          ),
        ),
        conditionalPanel(
          condition = "!input.upload_sample",
          ns = ns,
          chooserInput(ns("accession_select"), "Available frobs", "Selected frobs", leftChoices = c(), rightChoices = all.var.info, size = 10, multiple = TRUE),
        ),
        actionButton(ns("accession_submit"), strong("Submit"), styleclass = "success")
      ),
      br(),
    ),
    mainPanel(
      conditionalPanel(
        br(),
        br(),
        h3("SNP data:"),
        condition = "input.type=='SNP'",
        ns = ns,
        DT::dataTableOutput(ns("snp_info")),
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
        br(),
        br(),
        h3("Gene annotation:"),
        condition = "input.type=='Gene'",
        ns = ns,
        DT::dataTableOutput(ns("gene_info")),
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
        br(),
        br()
      ),

      conditionalPanel(
        br(),
        br(),
        h3("Geographic distribution:"),
        condition = "input.type=='Accession'",
        ns = ns,
        plotOutput(ns("accession_dis")),
        br(),
        selectInput(ns("sample_fig_format"),
          "Download figure as:",
          choices = available_fig_formats,
          selected = NULL,
          selectize = TRUE
        ),
        downloadButton(ns("sample_fig_download"), "Download Geographic distribution"),

        h3("SNP data:"),
        DT::dataTableOutput(ns("aceession_info")),
        br(),
        selectInput(ns("sample_format"),
          "Download data as:",
          choices = available_data_formats,
          selected = NULL,
          selectize = TRUE
        ),
        downloadButton(ns("accession_download"), "Download Accession Informations")
      )
    )
  )
}

mod_extraction_server <- function(input, output, session) {
  ns <- session$ns
  snp_sample <- reactive({
    if (input$snp_sample_choose == "1") {
      df <- readNewData_sample(fileinfo = input$snp_sample)
      df <- as.character(df$V1)
    } else {
      df <- input$snp_accession$selected
    }
    return(df)
  })
  snp_data <- eventReactive(input$snp_submit, {
    extractsnp(chr = input$chr, start = input$start, end = input$end, accession = snp_sample(), muttype = input$muttype, maf = input$maf)
  })

  output$snp_info <- renderDT({
    DT::datatable(snp_data()[[3]][, c(1:10)],
      rownames = FALSE,
      filter = "bottom",
      selection = "single",
      options = list(
        scrollX = TRUE,
        autoWidth = FALSE,
        columnDefs = list(list(className = "dt-right", target = "_all"))
      ), escape = FALSE
    )
  })

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
    if (input$ref == "Darmor-bzh") {
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
    d <- c("geneid", "chr", "start", "end", input$database)
    return(d)
  })

  gene_anno <- eventReactive(input$gene_submit, {
    gene_sample()[gene_sample()[, 1] %in% gene_id(), colnames(gene_sample()) %in% database_type()]
  })
  output$gene_info <- renderDT({
    DT::datatable(gene_anno(),
      rownames = FALSE,
      filter = "bottom",
      selection = "single",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
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
}
