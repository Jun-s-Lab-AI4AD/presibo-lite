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
library(igraph)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "PreSiBO Lite"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Target", tabName = "target"),
      menuItem("Network", tabName = "network"),
      menuItem("Network Subgroups", tabName = "geneProfiles"),
      menuItem("Drug", tabName = "drug")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(type="text/css", "label{ display: table-cell; text-align: right; vertical-align: middle; padding-right: 5px} .form-group { display: table-row;}"),
      tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle;} .form-group { display: table-row;}")
    ),
    tags$head(tags$style("#tbl_selected{color: #3c8dbc; font-size: 17px;}")),
    # tags$head(tags$style("#sgn_output1{color: orange; font-size: 17px;}")),
    # tags$head(tags$style("#sgn_output2{color: orange; font-size: 17px;}")),
    # tags$head(tags$style("#ngd_output1{color: orange; font-size: 17px;}")),
    # tags$head(tags$style("#ngd_output2{color: orange; font-size: 17px;}")),
    # tags$head(tags$style("#ngs_output2{color: orange; font-size: 17px;}")),
    tags$head(
      # Custom CSS to style the card
      tags$style(HTML("
                  .custom-card {
                    background-color: #ffffff;  /* White background */
                    border: 1px solid #cccccc; /* Light gray border */
                    border-radius: 8px;        /* Rounded corners */
                    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); /* Subtle shadow */
                    margin: 20px 0;            /* Spacing around the card */
                    padding: 15px;             /* Padding inside the card */
                  }
                  .custom-card-header {
                    background-color: #3c8dbc; /* Blue background for the header */
                    color: white;             /* White text for the header */
                    /* font-size: 18px;           Larger font size for header */
                    /* font-weight: bold;         Bold header text */
                    padding: 10px;            /* Spacing inside the header */
                    border-top-left-radius: 8px; /* Rounded top corners */
                    border-top-right-radius: 8px; /* Rounded top corners */
                    display: flex;            /* to make sub div flexible */
                    align-items: center;      /* vertically align subdiv */ 
                  }
                  .custom-card-body {
                    padding: 15px; /* Spacing inside the body */
                  }
                  .custom-card-footer {
                    background-color: #f8f9fa; /* Light gray background for the footer */
                    font-size: 14px;          /* Smaller font size for footer */
                    color: #6c757d;           /* Gray text for footer */
                    text-align: center;       /* Center-align footer text */
                    padding: 10px;            /* Spacing inside the footer */
                    border-bottom-left-radius: 8px; /* Rounded bottom corners */
                    border-bottom-right-radius: 8px; /* Rounded bottom corners */
                  }
                "))
    ),
    
    
    tabItems(
      # About page
      tabItem(tabName = "about",
              ui <- fluidPage(
                
                div(class = "custom-card",
                  div(
                    h1("PreSiBO Lite Database")
                  ),
                  div(
                    h4("PreSiBO Lite is a relational database system that enables quick search of AD multi-omics and PRS profile results in a structured manner."),
                    h4("The PreSiBO system uses four main categories of hierarchical features in AD-related genomic, multi-omic and PRS subtype profiles to provide quick insights across analyses. The four features are as follows - "),
                    h4(
                      tags$ol(
                        tags$li(HTML("<b>Predictors</b> - variant (SNP) data from GWAS, genes and polygenic risk scores (PRSs) form the highest, most fundamental level of data that underlie all kinds of effect")),
                        tags$li(HTML("<b>Signatures</b> - tissue and cell level genetic profiles from transcriptome, proteome, methylome and other epigenetic analyses (TWAS, PWAS, MWAS, HWAS) constitute the second (signature) tier")),
                        tags$li(HTML("<b>Biomarkers</b> - AD biomarkers associated with tau and amyloid beta from CSF, plasma, PET and MRI scans from fluid and imaging sources form the third")),
                        tags$li(HTML("<b>Outcomes</b> - phenotype data related to clinical and neuropathological diagnoses are the lowest possible causal tier as these are effects")),
                      )
                    ),
                    h4(HTML("<b>References</b> -")),
                    h4(
                      tags$ol(
                        tags$li("Panitch, R., Hu, J., Chung, J. et al. Integrative brain transcriptome analysis links complement component 4 and HSPA2 to the APOE ε2 protective effect in Alzheimer disease. Mol Psychiatry 26, 6054–6064 (2021). https://doi.org/10.1038/s41380-021-01266-z"),
                        tags$li("Chung, J., Panitch, R., Hu, J., Zhu, C., Mez, J., Farrer, L.A., Stein, T.D., Crane, P.K., Nho, K. and Jun, G.R. (2021), Alzheimer’s disease heterogeneity explained by polygenic risk scores based on brain transcriptomic profiles. Alzheimer's Dement., 17: e054517. https://doi.org/10.1002/alz.054517"),
                        tags$li("B. W. Kunkle et al., “Genetic meta-analysis of diagnosed Alzheimer’s disease identifies new risk loci and implicates Aβ, tau, immunity and lipid processing,” Nat Genet, vol. 51, no. 3, 2019, doi: 10.1038/s41588-019-0358-2."),
                        tags$li("Genome-Wide Association Meta-analysis of Neuropathologic Features of Alzheimer's Disease and Related Dementias, Beecham GW, Hamilton K, Naj AC, Martin ER, Huentelman M, et al. (2014) Genome-Wide Association Meta-analysis of Neuropathologic Features of Alzheimer's Disease and Related Dementias. PLOS Genetics 10(9): e1004606. https://doi.org/10.1371/journal.pgen.1004606"),
                        tags$li("Deming, Y., Li, Z., Kapoor, M. et al. Genome-wide association study identifies four novel loci associated with Alzheimer’s endophenotypes and disease modifiers. Acta Neuropathol 133, 839–856 (2017). https://doi.org/10.1007/s00401-017-1685-y"),
                        tags$li("Corsello SM, Bittker JA, Liu Z, Gould J, McCarren P, Hirschman JE, Johnston SE, Vrcic A, Wong B, Khan M, Asiedu J, Narayan R, Mader CC, Subramanian A, Golub TR. The Drug Repurposing Hub: a next-generation drug library and information resource. Nature Medicine. 23, 405–408 (2017)"),
                      )
                    ),
                    h4(HTML("<b>MIT License - Copyright (c) 2022 Jun Lab</b>"))
                  )
                )
                
              )
        
      ),
      
      # Target Search content
      tabItem(tabName = "target",
              ui <- fluidPage(
                searchInput(inputId = "targetSearch", 
                            label = "Gene name ", 
                            value = "ADCY2",
                            placeholder = "ADCY2",
                            btnSearch = "Search",
                            width = "400px"),
                br(),
                tabsetPanel(
                  # Target Predictor
                  tabPanel("Predictor",
                           ui <- fluidPage(
                             # custom card div
                             div(class = "custom-card",
                               # card header div - selection control div
                               div(class = "custom-card-header",
                                 div(
                                   style="display: inline-block;vertical-align:top;",
                                   tags$head(
                                     tags$style(HTML("
                                                .selectize-input {
                                                  width: 300px;
                                                }
                                              "))
                                   ),
                                   selectInput("var",
                                               label = "SBO Selection: ",
                                               choices = c("Outcome-Clinical Diagnosis", 
                                                           "Outcome-Tangle (BRAAK)",
                                                           "Outcome-Plaque (CERAD)", 
                                                           "Biomarker-CSF.Abeta", 
                                                           "Biomarker-CSF.pTau",
                                                           "Biomarker-CSF.tTau"
                                                           ),
                                               selected = "Outcome-Clinical Diagnosis")
                                 ),
                                 div(style="display: inline-block;vertical-align:top; width: 20px;"),
                                 div(
                                   style="display: inline-block;vertical-align:top;",
                                   searchInput(
                                     inputId = "predFilter", label = "Filter: P < ",
                                     value = "1.00",
                                     placeholder = "0.05",
                                     width = "150px"
                                   )
                                 ),
                               ), # end card header div
                               # div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
  
                               # div(
                               #   style="display: inline-block;vertical-align:top;",
                               #   br(),
                               #   downloadButton("downloadPred", "Download Table")
                               # ),
                               
                               div(class = "custom-card-body",
                                 style="overflow-x: auto;",
                                 textOutput("tbl_selected"),
                                 br(),
                                 DT::dataTableOutput("tbl")
                               )
                            ) # end custom card div 
                           ) # predictor tab fluidPage end
                  ),
                  tabPanel("Signature",
                           ui <- fluidPage(
                             div(class = "custom-card",
                               div(class = "custom-card-header",
                                 div(
                                   style="display: inline-block;vertical-align:top;",
                                   p("Analysis: ", strong("Differential Expression (AD vs Non-AD)"), style = "font-size:17px; margin: 0")
                                 ),
                                 div(style="display: inline-block;vertical-align:top; width: 20px;"),
                                 div(
                                   style="display: inline-block;vertical-align:top;",
                                   searchInput(
                                     inputId = "downSig", label = "Filter: P < ",
                                     placeholder = "0.05",
                                     width = "450px"
                                   )
                                 ),
                               ),
                               
                               # div(
                               #   style="display: inline-block;vertical-align:top;",
                               #   br(),
                               #   downloadButton("downloadSig", "Download Table")
                               # ),
                               div(class = "custom-card-body",
                                 style="overflow-x: auto;",
                                 dataTableOutput("view")
                               ),
                             ),
                             
                             div(class = "custom-card",
                               div(class = "custom-card-header",
                                 div(
                                   style="display: inline-block;vertical-align:top;",
                                   p("Analysis: ", strong("Quantitative Trait Loci"), style = "font-size:17px; margin: 0")
                                 ),
                                 div(style="display: inline-block;vertical-align:top; width: 20px;"),
                                 div(
                                   style="display: inline-block;vertical-align:top;",
                                   searchInput(
                                     inputId = "downSig2", label = "Filter: P < ",
                                     placeholder = "0.05",
                                     width = "450px",
                                   )
                                 ),
                               ),
                               
                               # div(
                               #   style="display: inline-block;vertical-align:top;",
                               #   br(),
                               #   downloadButton("downloadSig2", "Download Table")
                               # ),
                               
                               div(class = "custom-card-body",
                                 style="overflow-x: auto;",
                                 dataTableOutput("view2")
                               )
                             )
                             
                           ) # end of Signature fluidPage block
                  )
                )
              )
      ),
      
      # Network Search content
      tabItem(tabName = "network",
              searchInput(inputId = "netSearch", 
                          label = "Gene name ", 
                          placeholder = "ADCY2",
                          value = "ADCY2",
                          btnSearch = "Search"),
              br(),
              tabsetPanel(

                tabPanel("Signature Guided Networks",
                         # top table
                         div(class = "custom-card",
                           div(class = "custom-card-header",
                             div(
                               style="display: inline-block;vertical-align:top;
                                      color: white; font-size: 17px; font-weight: bold;",
                               textOutput("sgn_output1")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 20px;"),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               searchInput(
                                 inputId = "sigNet", label = "Filter: Z-Summary > ",
                                 placeholder = "0.05",
                                 width = "450px",
                               )
                             ),
                           ),
                           # div(
                           #   style="display: inline-block;vertical-align:top;",
                           #   br(),
                           #   downloadButton("downloadSigNet", "Download Table")
                           # ),
                           div(class = "custom-card-body",
                             div(
                               style="overflow-x: auto;",
                               dataTableOutput("viewtbl0")
                             )
                           ),
                         ),
                         
                         # bottom table and plot
                         div(class = "custom-card",
                           div(class = "custom-card-header",
                             div(
                               style="
                                display: inline-block; vertical-align:top;
                                color: white; font-size: 17px; font-weight: bold;",
                               textOutput("sgn_output2")
                             )
                           ),
                           
                          
                           # div(
                           #   style="display: inline-block;vertical-align:top;",
                           #   br(),
                           #   downloadButton("downloadSigNet2", "Download Table")
                           # ),
                           div(class = "custom-card-body",
                             div(
                               style="overflow-x: auto;",
                               dataTableOutput("viewtbl12_1")
                             )
                           ),
                         ),
                         
                         ## Module graph plot
                         div(class = "custom-card",
                           div(class = "custom-card-header",
                             div(
                               style="display: inline-block;vertical-align:top;",
                               p(strong("Module Plot (Top 20 members)"), style = "font-size:17px; margin: 0")
                             )
                           ),
                           
                           div(class = "custom-card-body",
                             div(
                               actionButton("genGraph", "Generate Graph"),
                               plotOutput("igraphModulePlot", height = "600px", width = "800px")
                             )
                           )
                         ),

                ),
                
                tabPanel("Network Guided Drugs",
                         ui <- fluidPage(
                           # top-table
                           div(class = "custom-card",
                             div(class = "custom-card-header",
                               div(
                                 style="display: inline-block;vertical-align:top;",
                                 selectInput("source",
                                             label = "Source of Network:",
                                             choices = c("All",
                                                         "Brain-Brain Transcriptome"),
                                             selected = "All"
                                 ) 
                               )
                             ),
                             # div(
                             #   style="display: inline-block;vertical-align:top;",
                             #   br(),
                             #   downloadButton("downloadNetDrug", "Download Table")
                             # ),
                             div(class = "custom-card-body",
                               div(
                                 # style="overflow-x: auto;",
                                 # textOutput("ngd_output1"),
                                 # br(),
                                 dataTableOutput("viewtable1")
                               )
                             )
                           ),
                           
                           # bottom-table
                           div(class = "custom-card",
                             div(class = "custom-card-header",
                               div(
                                 style="display: inline-block;vertical-align:top;
                                        color: white; font-size: 17px; font-weight: bold;",
                                 textOutput("ngd_output2")
                               )
                             ),
                             # div(
                             #   style="display: inline-block;vertical-align:top;",
                             #   br(),
                             #   downloadButton("downloadNetDrug2", "Download Table")
                             # ),
                             div(class = "custom-card-body",
                               div(
                                 style="overflow-x: auto;",
                                 dataTableOutput("viewtable12_1")
                               )
                             )
                           ),
                           
                           # donut plot
                           div(class = "custom-card",
                             div(class = "custom-card-header",
                               div(
                                 style="display: inline-block;vertical-align:top;",
                                 p(strong("Module Drug Breakdown"), style = "font-size:17px; margin: 0")
                               )
                             ),
                             div(class = "custom-card-body",
                               div(
                                 plotlyOutput("diseaseAreaDonut")
                               )
                             )
                           )
                         )
                )
              )
      ),
      
      tabItem(tabName = "geneProfiles",
              searchInput(inputId = "geneProfSearch", 
                          label = "Gene name", 
                          placeholder = "ADCY2",
                          value = "ADCY2",
                          btnSearch = "Search"),
              
              ui <- fluidPage(
                # top table
                div(class = "custom-card",
                  div(class = "custom-card-header",
                    div(
                      style="display: inline-block;vertical-align:top;",
                      selectInput(inputId = "source1",
                                  label = "Source of Network:",
                                  # "Brain-Brain Tissue-Level (Prefrontal Cortex) Transcriptome" removed
                                  choices = c(
                                              "All",
                                              "Brain-Brain Transcriptome"
                                              ),
                                  selected = "All"
                      )
                    )
                  ),
                  ## DO NOT REMOVE - uncomment when more PRS p-values are available
                  # div(style="display: inline-block;vertical-align:top; width: 120px;",HTML("<br>")),
                  # div(
                  #   style="display: inline-block;vertical-align:top;",
                  #   br(),
                  #   selectInput("source2",
                  #               label = "Selection of SNPs for Polygenic Risk Scores (PRSs):",
                  #               choices = c("All",
                  #                           "P<0.05",
                  #                           "P<0.001",
                  #                           "P<5E-08"),
                  #               selected = "All"
                  #   )
                  # ),
  
  
                  # div(style="display: inline-block;vertical-align:top; width: 20px;"),
                  # div(
                  #   style="display: inline-block;vertical-align:top;",
                  #   br(),
                  #   downloadButton("downloadNetGene", "Download Table")
                  # ),
                  # div(style="display: inline-block;vertical-align:top; width: 155px;",HTML("<br>")),
                  div(class = "custom-card-body",
                    div(
                      style="overflow-x: auto; overflow-y: auto;",
                      dataTableOutput("viewtbl")
                    )
                  ),
                ),
                
                # bottom table
                div(class = "custom-card",
                  div(class = "custom-card-header",
                    div(
                      style="display: inline-block;vertical-align:top;
                             color: white; font-size: 17px; font-weight: bold;",
                      textOutput("ngs_output2")
                    ),
                    div(style="display: inline-block;vertical-align:top; width: 20px;"),
                    div(
                      style="display: inline-block;vertical-align:top;",
                      searchInput(
                        inputId = "netGene2", label = "Filter: P < ",
                        placeholder = "0.05",
                        width = "450px"
                      )
                    ),
                  ),
                  # div(
                  #   style="display: inline-block;vertical-align:top;",
                  #   br(),
                  #   downloadButton("downloadNetGene2", "Download Table")
                  # ),
                  div(class = "custom-card-body",
                    div(
                      style="overflow-x: auto; overflow-y: auto;",
                      dataTableOutput("viewtbl2_1")
                    )
                  )
                )
                
              )
      ), 
      # end of tab item geneProfiles 
      
      # Drug Search content
      tabItem(tabName = "drug",
              div(
                style="display: inline-block;vertical-align:top;",
                searchInput(inputId = "searchInd", 
                            label = "Drug Search ", 
                            placeholder = "ADCY2 / Launched",
                            btnSearch = "Search")
              ),
              # div(style="display: inline-block;vertical-align:top; width: 100px;"),
              # 
              # div(
              #   style="display: inline-block;vertical-align:top;",
              #   br(),
              #   downloadButton("downloadIndic", "Download Table")
              # ),
              # div(style="display: inline-block;vertical-align:top; width: 200px;",HTML("<br>")),
              div(class = "custom-card",
                div(
                  style="overflow-x: auto; overflow-y: auto;",
                  dataTableOutput("viewtable2")
                )
              )
          )
      
      
    )
  )
)


# ui <- secure_app(ui)
