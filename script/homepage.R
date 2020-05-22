homepage <- dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  dashboardBody(
    column(
      width = 10,
      offset = 1,
      titleBox(title = "BnaSNPDB: SNP Database of Brasssica napus L.")
    ),
    column(
      width = 10,
      offset = 1,
      textBox(
        width = 12,
        p("The SNP database of", em("Brassica napus"), "L. (BnaSNPDB) is an interactive web-based platform and set of analytic tools for effcient retrieve and analysis of SNPs among 1007 rapeseed germplasm accessions based on the data reported by previous research (Wu et al., 2019).")
      )
    ),
    
    column(
      width = 10,
      offset = 1,
      sectionBox(
        title = "What's Inside",
        fluidRow(
          valueBox("2,404,340", "High Quality SNPs", width = 2),
          valueBox("1,007", "Germplasm Collections", color = "purple", width = 2),
          valueBox(39, "Countries", color = "yellow", width = 2),
          valueBox(658, "Winter Ecotypes", color = "fuchsia", width = 2),
          valueBox(145, "Semi-winter Ecotypes", color = "navy", width = 2),
          valueBox(188, "Spring Ecotypes", color = "olive", width = 2)
        )
      )
    ),

    column(
      width = 10,
      offset = 1,
      sectionBox(
        title = "Analysis Modules",
        messageBox(
          width = 12,
          p("We here present 5 modules to retrieve and analyze the SNP dataset. Within each module, the user can select different parameters to retrieve and analyze the SNP dataset. You can find some example figures that these modules can generate in the manuscript ",em("Wu et al. Whole-Genome Resequencing of a Worldwide Collection of Rapeseed Accessions Reveals the Genetic Basis of Ecotype Divergence, Molecular Plant (2019)"), "and manuscript",em("Xuan et al. Genome-wide association study reveals new genes involved in leaf trichome formation in polyploid oilseed rape (Brassica napus L.), Plant, Cell & Environment (2019)."))
        ),
        fluidRow(
          module_Box(
            width = 6,
            title = "LDheatmap",
            imgSrc = "img/ldheatmap.png",
            text = "This module provides a pipeline to retrieve SNPs and display pairwise linkage disequilibria between SNPs and allows the user select different parameters."
          ),
          module_Box(
            width = 6,
            title = "SNP Distribution",
            imgSrc = "img/snpdistribution.png",
            text = "This module compares the conserved SNPs specific to different groups of rapeseed."
          )
        ),
        fluidRow(
          module_Box(
            width = 6,
            title = "Phylogenetic",
            imgSrc = "img/phylogenetics.png",
            text = "This module implements the functionality to perform phylogenetic analysis in user-specified genomic regions."
          ),
          module_Box(
            width = 6,
            title = "Diversity",
            imgSrc = "img/diversity.png",
            text = "This module provides the functionality to calculate nucleotide diversity among groups of rapeseed germplasm accessions in user-specified genomic regions."
          )
        ),
        fluidRow(
          module_Box(
            width = 6,
            title = "Extraction",
            imgSrc = "img/extraction.png",
            text = "This module allows user to extract the detail informations of SNPs, annotation of genes and essential information on the 1007 rapeseed germplasm accessions used in the BnaSNPDB."
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 10, 
        offset = 1, 
        box(
          title = "More about of the SNP Dataset",
          width = 12,
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "warning",
          includeMarkdown("www/home.md"))))
  )
)
