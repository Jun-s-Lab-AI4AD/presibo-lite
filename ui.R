## ui.R ##
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(DBI)
library(pool)
library(dplyr)
library(DT)
library(RMySQL)
library(writexl)
library(formattable)
library(pracma)
library(shinymanager)

ui <- dashboardPage(
  dashboardHeader(title = "PreSiBO Lite"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Target", tabName = "target"),
      menuItem("Network", tabName = "network"),
      menuItem("Network Subgroups", tabName = "geneProfiles"),
      menuItem("Drug", tabName = "drug")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(type="text/css", "label{ display: table-cell; text-align: right; vertical-align: middle;} .form-group { display: table-row;}"),
      tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle;} .form-group { display: table-row;}")
    ),
    tags$head(tags$style("#tbl_selected{color: orange; font-size: 17px;}")),
    tags$head(tags$style("#sgn_output1{color: orange; font-size: 17px;}")),
    tags$head(tags$style("#sgn_output2{color: orange; font-size: 17px;}")),
    tags$head(tags$style("#ngd_output1{color: orange; font-size: 17px;}")),
    tags$head(tags$style("#ngd_output2{color: orange; font-size: 17px;}")),
    tags$head(tags$style("#ngs_output1{color: orange; font-size: 17px;}")),
    tags$head(tags$style("#ngs_output2{color: orange; font-size: 17px;}")),
    
    
    tabItems(
      
      # Target Search content
      tabItem(tabName = "target",
              ui <- fluidPage(
                searchInput(inputId = "targetSearch", 
                            label = "Gene name or chr:bp", 
                            value = "APOE",
                            placeholder = "APOE",
                            btnSearch = "Search",
                            width = "400px"),
                br(),br(),
                tabsetPanel(
                  
                  # Target Predictor
                  tabPanel("Predictor",
                           ui <- fluidPage(
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               tags$head(
                                 tags$style(HTML("

                                            .selectize-input {
                                              /*height: 50px;*/
                                              width: 300px;
                                              /*font-size: 24pt;*/
                                              /*padding-top: 5px;*/
                                            }
                                      
                                          "))
                               ),
                               selectInput("var",
                                           label = "SBO Selection:",
                                           choices = c("Outcome-Clinical Diagnosis", 
                                                       "Outcome-Tangle (BRAAK)",
                                                       "Outcome-Plaque (CERAD)", 
                                                       "Biomarker-CSF.Abeta", 
                                                       "Biomarker-CSF.pTau",
                                                       "Biomarker-CSF.tTau",
                                                       "Biomarker-PET-FDG-Angular", 
                                                       "Biomarker-PET-FDG-Cingulate",
                                                       "Biomarker-PET-FDG-Temporal",
                                                       "Biomarker-MRI-EntCtx",
                                                       "Biomarker-MRI-HippVol",
                                                       "Biomarker-MRI-ParietalCtx",
                                                       "Biomarker-MRI-TemporalCtx"
                                                       ),
                                           selected = "Outcome-Clinical Diagnosis")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               searchInput(
                                 inputId = "predFilter", label = "Filter: P < ",
                                 value = "1.00",
                                 placeholder = "0.05",
                                 width = "150px"
                               )
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),

                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               downloadButton("downloadPred", "Download Table")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                             div(
                               style="overflow-x: auto;",
                               br(),
                               textOutput("tbl_selected"),
                               br(),
                               DT::dataTableOutput("tbl"),
                               br()
                             )

                           )
                  ),
                  tabPanel("Signature",
                           ui <- fluidPage(
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               p("Analysis: ", strong("AD vs. Control", style = "color:green; font-size:17px"), style = "font-size:17px")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 40px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               searchInput(
                                 inputId = "downSig", label = "Filter: P < ",
                                 placeholder = "0.05",
                                 width = "450px"
                               )
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                             # div(
                             #   style="display: inline-block;vertical-align:top;",
                             #   br(),
                             #   numericInput(inputId = "sigShow",
                             #                label = "Show",
                             #                value = 10)
                             # ),
                             # div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                             # div(
                             #   style="display: inline-block;vertical-align:top;",
                             #   br(),
                             #   searchInput(
                             #     inputId = "sigSearch", label = "Search ",
                             #     placeholder = "ex) Brain, Tissue",
                             #     width = "150px"
                             #   )
                             # ),
                             # div(style="display: inline-block;vertical-align:top; width: 40px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               downloadButton("downloadSig", "Download Table")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
                             div(
                               style="overflow-x: auto;",
                               br(),
                               dataTableOutput("view")
                               # br(), br(), br(), br()
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 0px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               p("Analysis: ", strong("QTL", style = "color:green; font-size:17px"), style = "font-size:17px")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 120px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               searchInput(
                                 inputId = "downSig2", label = "Filter: P < ",
                                 placeholder = "0.05",
                                 width = "450px",
                               )
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                             # div(
                             #   style="display: inline-block;vertical-align:top;",
                             #   br(),
                             #   numericInput(inputId = "sigShow2",
                             #                label = "Show",
                             #                value = 10)
                             # ),
                             # div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                             # div(
                             #   style="display: inline-block;vertical-align:top;",
                             #   br(),
                             #   searchInput(
                             #     inputId = "sigSearch2", label = "Search ",
                             #     placeholder = "ex) Brain, Tissue",
                             #     width = "150px"
                             #   )
                             # ),
                             # div(style="display: inline-block;vertical-align:top; width: 40px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               downloadButton("downloadSig2", "Download Table")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
                             div(
                               style="overflow-x: auto;",
                               br(),
                               dataTableOutput("view2"),
                               br()
                             )
                             # div(style="display: inline-block;vertical-align:top; width: 50px;"),
                             # div(
                             #   style="display: inline-block;vertical-align:top;",
                             #   br(),
                             #   img(src = "genesplot.png", width = 500, height = 250, align = "center")
                             # ),
                             # div(style="display: inline-block;vertical-align:top; width: 50px;"),
                             # div(
                             #   style="display: inline-block;vertical-align:top;",
                             #   br(),
                             #   img(src = "apoeplot.png", width = 300, height = 250, align = "center")
                             # ),
                             # div(style="display: inline-block;vertical-align:top; width: 260px;"),
                             # div(
                             #   style="display: inline-block;vertical-align:top;",
                             #   actionButton("genesplot", "Download Plot")
                             # ),
                             # div(style="display: inline-block;vertical-align:top; width: 330px;"),
                             # div(
                             #   style="display: inline-block;vertical-align:top;",
                             #   actionButton("apoeplot", "Download Plot")
                             # )
                           )
                  )
                )
              )
      ),
      
      # Network Search content
      tabItem(tabName = "network",
              searchInput(inputId = "netSearch", 
                          label = "Gene name", 
                          placeholder = "APOE",
                          value = "APOE",
                          btnSearch = "Search"),
              br(),br(),
              tabsetPanel(

                tabPanel("Signature Guided Networks",
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           textOutput("sgn_output1")
                         ),
                         div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           searchInput(
                             inputId = "sigNet", label = "Filter: Z-Summary > ",
                             placeholder = "0.05",
                             width = "450px",
                           )
                         ),
                         div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           downloadButton("downloadSigNet", "Download Table")
                         ),
                         div(
                           style="overflow-x: auto;",
                           br(),
                           dataTableOutput("viewtbl0")
                           # br(), br(), br(), br()
                         ),
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           textOutput("sgn_output2")
                         ),
                         div(style="display: inline-block;vertical-align:top; width: 75px;",HTML("<br>")),
                        
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           downloadButton("downloadSigNet2", "Download Table")
                         ),
                         div(
                           style="overflow-x: auto;",
                           br(),
                           dataTableOutput("viewtbl12_1")
                         )

                ),
                
                tabPanel("Network Guided Drugs",
                         ui <- fluidPage(

                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             selectInput("source",
                                         label = "Source of Network:",
                                         choices = c("All",
                                                     "Brain-Brain Cell-Level (Astrocyte) Transcriptome",
                                                     "Brain-Brain Cell-Level (Excitatory Neuron) Transcriptome",
                                                     "Brain-Brain Cell-Level (Inhibitory Neuron) Transcriptome",
                                                     "Brain-Brain Cell-Level (Microglia) Transcriptome",
                                                     "Brain-Brain Cell-Level (Oligodendrocyte) Transcriptome",
                                                     "Brain-Brain Cell-Level (OPC) Transcriptome",
                                                     "Brain-Blood Transcriptome",
                                                     "Brain-Brain Proteome"),
                                         selected = "All"
                             ) 
                           ),

                           div(style="display: inline-block;vertical-align:top; width: 20px;"),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             downloadButton("downloadNetDrug", "Download Table")
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 250px;",HTML("<br>")),
                           div(
                             style="overflow-x: auto;",
                             textOutput("ngd_output1"),
                             br(),
                             dataTableOutput("viewtable1")
                           ),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             textOutput("ngd_output2")
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 20px",HTML("<br>")),
 
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             downloadButton("downloadNetDrug2", "Download Table")
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 150px;",HTML("<br>")),
                           div(
                             style="overflow-x: auto;",
                             br(),
                             dataTableOutput("viewtable12_1")
                           )
                         )
                )
              )
      ),
      
      tabItem(tabName = "geneProfiles",
              searchInput(inputId = "geneProfSearch", 
                          label = "Gene name", 
                          placeholder = "APOE",
                          value = "APOE",
                          btnSearch = "Search"),
              br(),br(),
              ui <- fluidPage(
                div(
                  style="display: inline-block;vertical-align:top;",
                  br(),
                  selectInput(inputId = "source1",
                              label = "Source of Network:",
                              # "Brain-Brain Tissue-Level (Prefrontal Cortex) Transcriptome" removed
                              choices = c(
                                          "All",
                                          "Brain-Brain Transcriptome"
                                          ),
                              selected = "All"
                  )
                ),
                div(style="display: inline-block;vertical-align:top; width: 120px;",HTML("<br>")),
                div(
                  style="display: inline-block;vertical-align:top;",
                  br(),
                  selectInput("source2",
                              label = "Selection of SNPs for Polygenic Risk Scores (PRSs):",
                              choices = c("All",
                                          "P<0.05",
                                          "P<0.001",
                                          "P<5E-08"),
                              selected = "All"
                  )
                ),
                div(style="display: inline-block;vertical-align:top; width: 120px;",HTML("<br>")),
                div(
                  style="display: inline-block;vertical-align:top;",
                  # style="overflow-x: auto;",
                  br(),
                  textOutput("ngs_output1")
                ),
                # div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                # div(
                #   style="display: inline-block;vertical-align:top;",
                #   br(),
                #   p("Brain-Blood Tissue-Level (Prefrontal Cortex) Transcriptome Networks ", style = "color:orange; font-size:17px")
                # ),
                # div(style="display: inline-block;vertical-align:top; width: 20px;"),
                # div(
                #   # style="display: inline-block;vertical-align:top;",
                #   style="overflow-x: auto;",
                #   searchInput(
                #     inputId = "netGene", label = "Filter: Z-Summary > ",
                #     placeholder = "0.05",
                #     width = "450px"
                #   )
                # ),
                # div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                # div(
                #   style="display: inline-block;vertical-align:top;",
                #   br(),
                #   searchInput(
                #     inputId = "sigGeneSearch", label = "Search ",
                #     placeholder = "ex) APOE, M17",
                #     width = "150px"
                #   )
                # ),
                div(style="display: inline-block;vertical-align:top; width: 20px;"),
                div(
                  style="display: inline-block;vertical-align:top;",
                  br(),
                  downloadButton("downloadNetGene", "Download Table")
                ),
                div(style="display: inline-block;vertical-align:top; width: 155px;",HTML("<br>")),
                div(
                  style="overflow-x: auto; overflow-y: auto;",
                  br(),
                  dataTableOutput("viewtbl")
                ),
                div(
                  style="display: inline-block;vertical-align:top;",
                  br(),
                  textOutput("ngs_output2")
                ),
                div(style="display: inline-block;vertical-align:top; width: 330px;",HTML("<br>")),
                div(
                  style="display: inline-block;vertical-align:top;",
                  br(),
                  searchInput(
                    inputId = "netGene2", label = "Filter: P < ",
                    placeholder = "0.05",
                    width = "450px"
                  )
                ),
                div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                # div(
                #   style="display: inline-block;vertical-align:top;",
                #   br(),
                #   searchInput(
                #     inputId = "sigGeneSearch2", label = "Search ",
                #     placeholder = "ex) DPM1, NIPAL3",
                #     width = "150px"
                #   )
                # ),
                # div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                div(
                  style="display: inline-block;vertical-align:top;",
                  br(),
                  downloadButton("downloadNetGene2", "Download Table")
                ),
                div(
                  style="overflow-x: auto; overflow-y: auto;",
                  br(),
                  dataTableOutput("viewtbl2_1"),
                  br()
                )
                # div(style="display: inline-block;vertical-align:top; width: 50px;"),
                # div(
                #   style="display: inline-block;vertical-align:top;",
                #   br(),
                #   img(src = "m2prsplot.png", width = 500, height = 200, align = "center")
                # ),
                # div(style="display: inline-block;vertical-align:top; width: 100px;"),
                # div(),
                # div(style="display: inline-block;vertical-align:top; width: 250px;"),
                # div(
                #   style="display: inline-block;vertical-align:top;",
                #   actionButton("m2prsplot", "Download Plot")
                # )
              )
      ), 
      # end of tab item geneProfiles 
      
      # Drug Search content
      tabItem(tabName = "drug",
              div(
                style="display: inline-block;vertical-align:top;",
                br(),
                searchInput(inputId = "searchInd", 
                            label = "Search ", 
                            placeholder = "ex) APOE",
                            btnSearch = "Search")
              ),
              div(style="display: inline-block;vertical-align:top; width: 100px;"),
              # div(
              #   style="display: inline-block;vertical-align:top;",
              #   br(),
              #   numericInput(inputId = "showInd", 
              #               label = "Show", 
              #               value = 10)
              # ),
              # div(style="display: inline-block;vertical-align:top; width: 100px;"),
              div(
                style="display: inline-block;vertical-align:top;",
                br(),
                downloadButton("downloadIndic", "Download Table")
              ),
              div(style="display: inline-block;vertical-align:top; width: 200px;",HTML("<br>")),
              div(
                style="overflow-x: auto; overflow-y: auto;",
                br(),
                dataTableOutput("viewtable2"),
                br()
              )
          )
      
      
    )
  )
)


ui <- secure_app(ui)
