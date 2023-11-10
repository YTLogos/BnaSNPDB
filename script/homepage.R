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
        p("The SNP database of", em("Brassica napus"), "L. (BnaSNPDB) is an interactive web-based platform and set of analytic tools for effcient retrieve and analysis of SNPs among 1007 rapeseed germplasm accessions based on the data reported by previous research(", a("Wu et al., 2019", href = "https://jianglab.netlify.app/pdf/mp.pdf", target = "_blank"), ").")
      )
    ),
    
    column(
      width = 10,
      offset = 1,
      titleBox(title = "Tutorial of BnaSNPDB")
    ),
    column(
      width = 10,
      offset = 1,
      textBox(
        width = 12,
        p("To facilitate user access to BnaSNPDB, we create a short and informative tutorial video for the learning purpose. User can click",a("How to use the BnaSNPDB (video from YouTube)", href = "https://www.youtube.com/watch?v=8mBHzsKVotc", target = "_blank"),"or", a("How to use the BnaSNPDB (video from Bilibili)", href = "https://www.bilibili.com/video/BV1654y1C7hU", target = "_blank"),"to watch the video.")
      )
    ),

    column(
      width = 10,
      offset = 1,
      sectionBox(
        title = "What's Inside",
        fluidRow(
          valueBox(countup(2404340), "High Quality SNPs", width = 4,color = "fuchsia"),
          valueBox(countup(1007), "Germplasm Collections", color = "maroon", width = 4),
          valueBox(countup(39), "Countries", color = "yellow", width = 4)
        ),
        fluidRow(
          valueBox(countup(658), "Winter Ecotypes", color = "aqua", width = 4),
          valueBox(countup(145), "Semi-winter Ecotypes", color = "navy", width = 4),
          valueBox(countup(188), "Spring Ecotypes", color = "olive", width = 4)
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
          p("We here present 5 modules to retrieve and analyze the SNP dataset. Within each module, the user can select different parameters to retrieve and analyze the SNP dataset. You can find some example figures that these modules can generate in the manuscript ", a(em("Wu et al. Whole-Genome Resequencing of a Worldwide Collection of Rapeseed Accessions Reveals the Genetic Basis of Ecotype Divergence, Molecular Plant (2019)"), href = "https://jianglab.netlify.app/pdf/mp.pdf", target = "_blank"), "and manuscript", a(em("Xuan et al. Genome-wide association study reveals new genes involved in leaf trichome formation in polyploid oilseed rape (Brassica napus L.), Plant, Cell & Environment (2019)."), href = "https://jianglab.netlify.app/pdf/pce.13694.pdf", target = "_blank")),
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
          includeMarkdown("www/md/home.md")
        )
      )
    )
  )
)
