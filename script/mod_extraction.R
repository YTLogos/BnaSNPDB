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
          fileInput(ns("snp_sample"), "Upload Your Samples: ",
            multiple = FALSE,
            accept = c(
              "text/txt",
              "text/comma-separated-values,text/plain",
              ".txt"
            )
          )
        ),
        conditionalPanel(
          condition = "input.snp_sample_choose==0",
          ns = ns,
          chooserInput(ns("snp_accession"), "Available frobs", "Selected frobs", leftChoices = c(), rightChoices = all.var.info, size = 10, multiple = TRUE)
        ),
        checkboxGroupInput(ns("mut_type"), h4("Select Mutation types:"), choices = eff_type, selected = eff_type),
      ),
      conditionalPanel(
        condition = "input.type=='Gene'",
        ns = ns,
        selectInput(ns("ref"), "Reference Genome", choices = c("Darmor-bzh", "ZS11", "NY7", "Tapidor")),
        conditionalPanel(
          condition = "input.ref=='Darmor-bzh'",
          ns = ns,
          checkboxInput(ns("singlegene"), "Single gene?", FALSE),
          conditionalPanel(
            condition = "input.singlegene",
            ns = ns,
            textInput(ns("gene"), "Enter gene name:", value = "BnaA10g22080D")
          ),
          conditionalPanel(
            condition = "!input.singlegene",
            ns = ns,
            fileInput(ns("genecluster"), "Upload genes: ",
              multiple = FALSE,
              accept = c(
                "text/txt",
                "text/comma-separated-values,text/plain",
                ".txt"
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.ref=='ZS11'",
          ns = ns,
          checkboxInput(ns("singlegene1"), "Single gene?", FALSE),
          conditionalPanel(
            condition = "input.singlegene1",
            ns = ns,
            textInput(ns("gene1"), "Enter gene name:", value = "BnaA10T0110200ZS")
          ),
          conditionalPanel(
            condition = "!input.singlegene1",
            ns = ns,
            fileInput(ns("genecluster1"), "Upload genes: ",
              multiple = FALSE,
              accept = c(
                "text/txt",
                "text/comma-separated-values,text/plain",
                ".txt"
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.ref=='NY7'",
          ns = ns,
          checkboxInput(ns("singlegene2"), "Single gene?", FALSE),
          conditionalPanel(
            condition = "input.singlegene2",
            ns = ns,
            textInput(ns("gene2"), "Enter gene name:", value = "chrA01g000670.t1")
          ),
          conditionalPanel(
            condition = "!input.singlegene2",
            ns = ns,
            fileInput(ns("genecluster2"), "Upload genes: ",
              multiple = FALSE,
              accept = c(
                "text/txt",
                "text/comma-separated-values,text/plain",
                ".txt"
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.ref=='Tapidor'",
          ns = ns,
          checkboxInput(ns("singlegene3"), "Single gene?", FALSE),
          conditionalPanel(
            condition = "input.singlegene3",
            ns = ns,
            textInput(ns("gene3"), "Enter gene name:", value = "BnaA07g00690.1T")
          ),
          conditionalPanel(
            condition = "!input.singlegene3",
            ns = ns,
            fileInput(ns("genecluster3"), "Upload genes: ",
              multiple = FALSE,
              accept = c(
                "text/txt",
                "text/comma-separated-values,text/plain",
                ".txt"
              )
            )
          )
        ),

        checkboxGroupInput(ns("database"), "Select annotation database", choices = c("NR", "Swiss-prot", "KEGG", "eggNOG", "GO"), selected = c("NR", "Swiss-prot", "KEGG", "eggNOG", "GO"))
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
        )
      ),
      br(),
      actionButton(ns("submit"), strong("Submit"), styleclass = "success")
    ),
    mainPanel(
      conditionalPanel(
        br(),
        br(),
        h3("SNP data:"),
        condition = "input.type=='SNP'",
        ns = ns,
        DT::dataTableOutput(ns("snp_info"))
      ),

      conditionalPanel(
        br(),
        br(),
        h3("Gene annotation:"),
        condition = "input.type=='Gene'",
        ns = ns,
        DT::dataTableOutput(ns("gene_info"))
      ),

      conditionalPanel(
        br(),
        br(),
        h3("Geographic distribution:"),
        condition = "input.type=='Accession'",
        ns = ns,
        plotOutput(ns("accession_dis")),
        h3("SNP data:"),
        DT::dataTableOutput(ns("aceession_info"))
      )
    )
  )
}

mod_extraction_server <- function(input, output, session) {
  ns <- session$ns
  snp_data <- reactive({
    extractsnp(
      chr = input$chr, start = input$start, end = input$end
    )
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
}



