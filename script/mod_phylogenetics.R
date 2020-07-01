mod_phylogenetics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      width = 3,
      h2("Parameters:"),
      selectInput(ns("chr"), "Chromosome", selected = "A10", choices = chromosome),
      numericInput(ns("start"), "Start", value = "14990000"),
      numericInput(ns("end"), "End", value = "15010000"),
      numericInput(ns("maf"), "Minor Allele Frequency", value = 0.05),
      checkboxGroupInput(ns("phylo_mut_type"), "Select Mutation types:",
        choices = eff_type,
        selected = eff_type
      ),
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
        chooserInput(ns("phylo_accession"), "Available frobs", "Selected frobs", c(), all.var.info, size = 10, multiple = TRUE)
      ),
      br(),
      actionButton(ns("phylo_submit"), strong("Submit"), styleclass = "success")
    ),

    mainPanel(
      h2("Phylogenetic Tree:"),
      withSpinner(plotOutput(ns("tree")), type = 4),
      selectInput(ns("tree_fig_format"),
        "Download figure as:",
        choices = available_fig_formats,
        selected = NULL,
        selectize = TRUE
      ),
      downloadButton(ns("tree_fig_download"), "Download Phylogenetics tree"),
      br(),
      br(),
      br(),
      h2("SNP Informations:"),
      withSpinner(DT::dataTableOutput(ns("tree_snp_info")), type = 7),
      br(),
      selectInput(ns("tree_snp_format"),
        "Download data as:",
        choices = available_data_formats,
        selected = NULL,
        selectize = TRUE
      ),
      downloadButton(ns("tree_data_download"), "Download SNP Data"),
      br(),
      br(),
      br(),
      br()
    )
  )
}


mod_phylogenetics_server <- function(input, output, session) {
  ns <- session$ns
  par_list <- eventReactive(input$phylo_submit, {
    phylo_chr <- input$chr
    phylo_start <- input$start
    phylo_end <- input$end
    return(list(phylo_chr, phylo_start, phylo_end))
  })

  sample <- reactive({
    if (input$upload_sample) {
      sample <- readNewData_sample(fileinfo = input$sample)
      id <- as.character(sample$V1)
    } else {
      sample <- sapply(input$phylo_accession$selected, function(x) {
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

  snp_data <- eventReactive(input$phylo_submit, {
    extractsnp(chr = input$chr, start = input$start, end = input$end, muttype = input$phylo_mut_type, accession = sample(), maf = input$maf)
  })

  phylo_tree <- reactive({
    snpmat <- t(as.matrix(snp_data()[[1]]))
    tree <- nj(dist.gene(snpmat))
    p <- ggtree(tree, layout = "circular", branch.length = "none", size = 0.1) + ggtitle("") + theme_void()
    p <- gheatmap(p, all.tree.info, offset = 1, width = 0.1, colnames = FALSE, color = NULL) + theme(legend.title = element_blank()) +
      theme(legend.text = element_text(size = 16, face = "bold")) +
      scale_fill_brewer(palette = "Set1") +
      guides(color = guide_legend(override.aes = list(size = 3)))
    tree_download <<- p
    return(p)
  })
  output$tree <- renderPlot({
    print(phylo_tree())
  })
  output$tree_snp_info <- renderDT({
    DT::datatable(snp_data()[[3]][, c(1:10)],
      rownames = FALSE,
      filter = "bottom",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = FALSE,
        lengthMenu = c(5, 8, 10),
        columnDefs = list(list(className = "dt-right", target = "_all"))
      ), escape = FALSE
    )
  })

  output$tree_fig_download <- downloadHandler(
    filename = function() {
      glue::glue("{par_list()[[1]]}_{par_list()[[2]]}_{par_list()[[3]]}.Phylogenetics.{input$tree_fig_format}")
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
          if (input$tree_fig_format == "png") {
            png(file, width = 10 * 300, height = 10 * 300, res = 300)
          } else if (input$tree_fig_format == "pdf") {
            pdf(file, width = 12, height = 12, onefile = F)
          } else if (input$tree_fig_format == "jpeg") {
            jpeg(file, width = 10 * 300, height = 10 * 300, res = 300)
          } else if (input$tree_fig_format == "tiff") {
            tiff(file, width = 10 * 300, height = 10 * 300, res = 300)
          } else if (input$tree_fig_format == "bmp") {
            bmp(file, width = 10 * 300, height = 10 * 300, res = 300)
          } else {
            svg(file)
          }
          print(tree_download)
          dev.off()
        }
      )
    }
  )

  output$tree_data_download <- downloadHandler(
    filename = function() {
      glue::glue("{par_list()[[1]]}_{par_list()[[2]]}_{par_list()[[3]]}.SNP.data.{input$tree_snp_format}")
    },
    content = function(file) {
      if (input$tree_snp_format == "txt") {
        write.table(snp_data()[[3]], file, row.names = F, col.names = T, quote = F)
      } else if (input$tree_snp_format == "csv") {
        readr::write_csv(snp_data()[[3]], file, col_names = T)
      } else if (input$tree_snp_format == "tsv") {
        readr::write_tsv(snp_data()[[3]], file, col_names = T)
      } else {
        writexl::write_xlsx(snp_data()[[3]], file, col_names = T)
      }
    }
  )
}
