
library(shiny)
chromosome <- c("A01","A02","A03","A04","A05","A06","A07","A08","A09","A10",
                "C01","C02","C03","C04","C05","C06","C07","C08","C09","Ann","Cnn")
# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage(title = HTML("<h2><font color='Yellow'>BnaSNPDB</font></h2>"),
               header = tagList(useShinydashboard()),
               theme = shinytheme("cosmo"),
               tabPanel(h2("About"),
                        fluidRow(
                            valueBox("2,404,340", "High Quality SNPs", width = 2),
                            valueBox("1,007", "Germplasm Collection", color = "purple", width = 2),
                            valueBox(39, "Countries Around The World", color = "yellow", width = 2),
                            valueBox(700, "Winter Ecotype", color = "fuchsia", width = 2),
                            valueBox(105, "Semi-winter Ecotype", color = "navy", width = 2),
                            valueBox(185, "Spring Ecotype", color = "olive", width = 2)
                        )
                        ),
               
#####################  LDheatmap  ##########################
               tabPanel(h2("LDheatmap"),
                        sidebarLayout(
                            sidebarPanel(width=3,HTML("<h4><font color='red'>Parameters</font></h4>"),
                                         selectInput("chr",
                                                     "Chromosome", 
                                                     selected = "A10", choices = chromosome),
                                         numericInput("start","Start", value = "14990000"),
                                         numericInput("end","End", value = "15010000"),
                                         numericInput("maf","Minor Allele Frequency", value = 0.05),
                                         radioButtons("flip","Flip The Figure", list("FALSE"=0, "TRUE"=1)),
                                         conditionalPanel(
                                             condition = "input.flip==1",
                                             checkboxInput("ldshowgene","Show Gene Model", FALSE),
                                             conditionalPanel(
                                                 condition = "input.ldshowgene",
                                                 numericInput("ldy", "Y:", value = 72),
                                                 numericInput("ldw","W:", value = 72)
                                             )
                                         ),
                                         checkboxGroupInput("ld_mut_type",h4("Select Mutation types:"),
                                                            choices = eff_type,
                                                            selected = eff_type),
                                         HTML("<h4><font color='black'>Select Accessions:</font></h4>"),
                                         br(),
                                         chooserInput("ld_accession","Available frobs", "Selected frobs",leftChoices = c(), rightChoices = all.var.info, size = 10, multiple = TRUE),
                                         actionButton(inputId = "ld_submit", label = "Submit")
                                         ),
                            mainPanel(
                                HTML("<h1><font color='red'>LDheatmap</font></h1>"),
                                plotOutput("ldheatmap"),
                                selectInput("ld_fig_format",
                                    "Download figure as:",
                                    choices = available_fig_formats,
                                    selected = NULL,
                                    selectize = TRUE
                                ),
                                downloadButton("ld_fig_download","Download LDheadmap"),
                                br(),
                                br(),
                                br(),
                                HTML("<h1><font color='red'>SNP Informations</font></h1>"),
                                DT::dataTableOutput("ld_snp_info"),
                                selectInput("ld_snp_format",
                                            "Download data as:",
                                            choices = available_data_formats,
                                            selected = NULL,
                                            selectize = TRUE
                                ),
                                downloadButton("ld_data_download","Download SNP Data"),
                                br(),
                                br(),
                                br(),
                                br()
                                )
                            )
                        ))
               )

######################### About ###############



######################### LDheatmap ##########################
server <- function(input, output) {
    
    par_list <- eventReactive(input$ld_submit,{
        ld_chr <- input$chr
        ld_start <- input$start
        ld_end <- input$end
        return(list(ld_chr, ld_start, ld_end))
    })
    
    snp_data <- eventReactive(input$ld_submit,{
        extractsnp(chr = input$chr, start = input$start, end = input$end, 
                   muttype = input$ld_mut_type, accession = input$ld_accession$selected, maf = input$maf)
    })
    ldheatmap_fig <- reactive({
        snp.pos <- snp_data()[[2]]
        snp.mat <- t(as.matrix(snp_data()[[1]]))
        snp.mat <- as(snp.mat, "SnpMatrix")
        if(input$flip==0){
            LDheatmap(snp.mat, snp.pos, flip = F, title = NULL, color = heat.colors(20))
        }else{
            if(input$ldshowgene){
                ld <- LDheatmap(snp.mat, snp.pos, flip = T, title = NULL, color = heat.colors(20))
                p1 <- genestru.viz(chr=par_list()[[1]],start=par_list()[[2]], end=par_list()[[3]])
                plot.new()
                ld2 <- LDheatmap.addGrob(ld, rectGrob(gp=gpar(col="white")), height = .3)
                pushViewport(viewport(x=0.483, y=0.85, width = 0.85, height = .3))
                grid.draw(ggplotGrob(p1))
            }else{
                LDheatmap(snp.mat, snp.pos, flip = T, title = NULL, color = heat.colors(20))
            }
        }
    })
    output$ldheatmap <- renderPlot({
        ldheatmap_fig()
    })
    output$ld_snp_info <- renderDT({DT::datatable(snp_data()[[3]][,c(1:10)],
                                                 rownames = FALSE,
                                                 filter = "top",
                                                 selection = "single",
                                                 options = list(
                                                     pageLength=10,
                                                     scrollX=TRUE,
                                                     columnDefs=list(list(className="dt-right", target="_all"))
                                                 ))
    })
    
######################      download the LDheatmap       ###########################
    output$ld_fig_download <- downloadHandler(
        filename = function(){
            glue::glue("{par_list()[[1]]}_{par_list()[[2]]}_{par_list()[[3]]}.LDheatmap.{input$ld_fig_format}")
        },
        content = function(file){
            if(input$ld_fig_format=="png"){
                png(file, width = 10*300,height = 8*300, res=300)
            }else if(input$ld_fig_format=="pdf"){
                pdf(file, width = 12,height = 8, onefile = F)
            }else if(input$ld_fig_format=="jpeg"){
                jpeg(file,width = 8*300,height = 8*300, res=300)
            }else if(input$ld_fig_format=="tiff"){
                tiff(file,width = 10*300,height = 8*300, res=300)
            }else if(input$ld_fig_format=="bmp"){
                bmp(file,width = 10*300,height = 8*300, res=300)
            }else{
                svg(file)
            }
            snp.pos <- snp_data()[[2]]
            snp.mat <- t(as.matrix(snp_data()[[1]]))
            snp.mat <- as(snp.mat, "SnpMatrix")
            if(input$flip==0){
                LDheatmap(snp.mat, snp.pos, flip = F, title = NULL, color = heat.colors(20))
            }else{
                if(input$ldshowgene){
                    ld <- LDheatmap(snp.mat, snp.pos, flip = T, title = NULL, color = heat.colors(20))
                    p1 <- genestru.viz(chr=par_list()[[1]],start=par_list()[[2]], end=par_list()[[3]])
                    plot.new()
                    ld2 <- LDheatmap.addGrob(ld, rectGrob(gp=gpar(col="white")), height = .3)
                    pushViewport(viewport(x=0.483, y=0.85, width = 0.85, height = .3))
                    grid.draw(ggplotGrob(p1))
                }else{
                    LDheatmap(snp.mat, snp.pos, flip = T, title = NULL, color = heat.colors(20))
                }
            }
            dev.off()
        }
    )
    
##############################  download SNP data  #################
    output$ld_data_download <- downloadHandler(
        filename = function(){
            glue::glue("{par_list()[[1]]}_{par_list()[[2]]}_{par_list()[[3]]}.SNP.data.{input$ld_snp_format}")
        },
        content = function(file){
            if(input$ld_snp_format=="txt"){
                write.table(snp_data()[[3]], file, row.names = F, col.names = T, quote = F)
            }else if(input$ld_snp_format=="csv"){
                readr::write_csv(snp_data()[[3]], file, col_names = T)
            }else if(input$ld_snp_format=="tsv"){
                readr::write_tsv(snp_data()[[3]], file, col_names = T)
            }else{
                writexl::write_xlsx(snp_data()[[3]], file, col_names = T)
            }
        }
    )
    
 }

# Run the application 
shinyApp(ui = ui, server = server)
