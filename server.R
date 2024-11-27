## server.R ##
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
library(config)

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(shinymanager)
library(igraph)
library(scales)
library(plotly)

config_file = "config_dp.yml"
# define some credentials
creds <- config::get(file = config_file)
login_creds <- creds$login

credentials <- data.frame(
  user = login_creds$user1,
  password = login_creds$pwd1,
  stringsAsFactors = FALSE
)

# Helper function to enforce scientific notation on specified columns
force_scientific <- function(data, columns, digits = 5) {
  data[columns] <- lapply(data[columns], function(column) {
    format(column, scientific = TRUE, digits = digits)
  })
  return(data)
}

server <- function(input, output, session) {
  
  # authorizing
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials),
    timeout = 60 # in minutes
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  conf <- config::get(file = config_file)
  dbconf <- conf$dbconf
  
  ##############Target Search##############
  
  dfPred <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = 'presibo1',
      host = dbconf$server,
      username = dbconf$uid,
      password = dbconf$pwd
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    
    #######When type the gene name############
    # if (strsplit(input$targetSearch, ":") == input$targetSearch) {
      
      varInitQuery <- "CREATE OR REPLACE VIEW view_variant_reference_gene AS
      SELECT * FROM `extended_gwas_variant_reference_table`
      WHERE gene_id = ?id1 OR geneid_left = ?id1 OR geneid_right = ?id1;"
      initQuery <- sqlInterpolate(conn, varInitQuery, id1 = input$targetSearch)
      print(initQuery)
      dbSendQuery(conn, initQuery)
      
      varQuery <- "SELECT t1.variant_id AS `Variant ID`, t1.snp_id AS `SNP ID`, t1.a1 AS `Allele 1`, t1.a2 AS `Allele 2`, 
                        ROUND(t1.freq1, 3) AS `Frequency1`, ROUND(t1.beta, 3) AS `BETA`, ROUND(t1.se, 3) AS `SE`, t1.p AS `P`,
                        t2.gene_id AS `Gene`, t2.geneid_left AS `Gene Left`, t2.geneid_right AS `Gene Right`, 
                        t2.dist_left AS `Distance Left (bps)`, t2.dist_right AS `Distance Right (bps)`
                        FROM ?id t1
                        INNER JOIN `view_variant_reference_gene` t2
                        USING (variant_id)
                        WHERE t1.p <= ?id1;"

      
      # after SBO selection
      if(input$var == "Outcome-Clinical Diagnosis"){
          query <- sqlInterpolate(conn, varQuery,
                                  id = dbQuoteIdentifier(conn, 'Kunkle_IGAP_GWAS_Variant_Clinical_Dx_ALL_Stage2'),
                                  id1 = input$predFilter
                                )
      }
      else if(input$var == "Outcome-Tangle (BRAAK)"){
          query <- sqlInterpolate(conn, varQuery,
                                  id = dbQuoteIdentifier(conn, 'Beecham_ADGC_GWAS_Variant_Neuropath_BRAAK_ALL'),
                                  id1 = input$predFilter
          )
      }
      else if(input$var == "Outcome-Plaque (CERAD)"){
          query <- sqlInterpolate(conn, varQuery,
                                  id = dbQuoteIdentifier(conn, 'Beecham_ADGC_GWAS_Variant_Neuropath_CERAD_ALL'),
                                  id1 = input$predFilter
          )
      }
      else if(input$var == "Biomarker-CSF.Abeta"){
          query <- sqlInterpolate(conn, varQuery,
                                  id = dbQuoteIdentifier(conn, 'Deming_Public_GWAS_Variant_CSF_Ab42_ALL'),
                                  id1 = input$predFilter
          )
      }
      else if(input$var == "Biomarker-CSF.pTau"){
          query <- sqlInterpolate(conn, varQuery,
                                  id = dbQuoteIdentifier(conn, 'Deming_Public_GWAS_Variant_CSF_pTau_ALL'),
                                  id1 = input$predFilter
          )
      }
      else if(input$var == "Biomarker-CSF.tTau"){
          query <- sqlInterpolate(conn, varQuery,
                                  id = dbQuoteIdentifier(conn, 'Deming_Public_GWAS_Variant_CSF_tTau_ALL'),
                                  id1 = input$predFilter
          )
      }
      print(query)
    # } # end of if (strsplit(input$targetSearch, ":") == input$targetSearch)
    dbGetQuery(conn, query)
  })
  
  
  
  output$tbl <- renderDataTable({
    current_data <- dfPred()
    # Apply scientific notation to the specified columns
    formatted_data <- force_scientific(current_data, 
                                       columns = c("P"), 
                                       digits = 5)
    # Apply scientific notation on numeric cols
    datatable(formatted_data)
  })
  
  # output$downloadPred <- downloadHandler(
  #   filename = function() { paste("Predictor", "xlsx", sep = ".")},
  #   content = function(file) {write_xlsx(dfPred(), path = file)}
  # )
  
  output$tbl_selected <- renderText({ 
    if (input$var != '') {
      paste("Table: ", input$var)
    }
  })
  
  ##############Signature Search - Top table##############
  dfSig <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = 'presibo1',
      host = dbconf$server,
      username = dbconf$uid,
      password = dbconf$pwd
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    
    varInitQuery <- 'CREATE OR REPLACE VIEW `presibo1`.`eqtl_master_view` AS
	                    SELECT * FROM  `presibo1`.`Jun_eQTL_Outcome_Combined`
                      WHERE gene_id = ?id;'
    initQuery <- sqlInterpolate(conn, varInitQuery, id = input$targetSearch)
    print(initQuery)
    dbGetQuery(conn, initQuery)
    
    initQuery2 <- 'CREATE OR REPLACE VIEW `presibo1`.`diff_expr_master_view` AS
                          SELECT gene_id AS "Gene ID", "brain tissue" AS "Source", "e23-meta3" AS "Subgroup", meta3_e23_Zscore AS "Zscore or logFC",
                          		meta3_e23_P AS "P-Value"
                          		FROM `presibo1`.`eqtl_master_view`
                          UNION
                          SELECT gene_id, "brain tissue", "e33-meta3" AS "Subgroup", meta3_e33_Zscore,
                          		meta3_e33_P AS "P Value"
                          		FROM `presibo1`.`eqtl_master_view`
                          UNION
                          SELECT gene_id, "brain tissue", "e34-meta3" AS "Subgroup", meta3_e34_Zscore,
                          		meta3_e34_P AS "P Value"
                          		FROM `presibo1`.`eqtl_master_view`
                          UNION
                          SELECT gene_id, "brain tissue", "all-meta3" AS "Subgroup", meta3_all_Zscore,
                          		meta3_all_P AS "P Value"
                          		FROM `presibo1`.`eqtl_master_view`;'
    print(initQuery2)
    dbGetQuery(conn, initQuery2)
    
    if(input$downSig != ''){
        varQuery <- 'SELECT * FROM `presibo1`.`diff_expr_master_view`
                      WHERE `P-VALUE` < ?id;'
        query <- sqlInterpolate(conn, varQuery, id = input$downSig)
        print(query)
    } else {
        query <- 'SELECT * FROM `presibo1`.`diff_expr_master_view`;'
        print(query)
    }
    
    dbGetQuery(conn, query)
  })
  
  output$view <- renderDataTable({
    current_data <- dfSig()
    # Apply scientific notation to the specified columns
    formatted_data <- force_scientific(current_data, 
                                       columns = c("P-Value"), 
                                       digits = 5)
    # Apply scientific notation on numeric cols
    datatable(formatted_data)
  })
  
  # output$downloadSig <- downloadHandler(
  #   filename = function() { paste("Signature", "xlsx", sep = ".")},
  #   content = function(file) {write_xlsx(dfSig(), path = file)}
  # )
  
  ##############Signature Search2 - Bottom table##############
  dfSig2 <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = dbconf$database,
      host = dbconf$server,
      username = dbconf$uid,
      password = dbconf$pwd
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    
    varInitQuery <- "CREATE OR REPLACE VIEW `presibo1`.`eqtl_master_view` AS
                    	SELECT * FROM  `presibo1`.`Jun_eQTL_Outcome_Combined`
                      WHERE gene_id = ?id;"
    initQuery <- sqlInterpolate(conn, varInitQuery, id = input$targetSearch)
    print(initQuery)
    dbGetQuery(conn, initQuery)
    
    initQuery2 <- 'CREATE OR REPLACE VIEW `presibo1`.`signature_master_view` AS
                    SELECT gene_id AS "Gene ID", "brain tissue" AS "Source", "all" AS "Subgroup",  "BRAAK" AS "Outcome", 
                    		meta2_braak_BETA AS "Beta", meta2_braak_SE AS "SE", meta2_braak_P AS "P-Value"
                    		FROM `presibo1`.`eqtl_master_view`
                    UNION
                    SELECT gene_id, "brain tissue", "all",  "CERAD", 
                    		meta2_cerad_BETA, meta2_cerad_SE, meta2_cerad_P
                    		FROM `presibo1`.`eqtl_master_view`
                    UNION
                    SELECT gene_id, "brain tissue", "all",  "PSD95", 
                    		PSD95_BETA, PSD95_SE, PSD95_P
                    		FROM `presibo1`.`eqtl_master_view`
                    UNION
                    SELECT gene_id, "brain tissue", "all",  "Ab42", 
                    		Ab42_BETA, Ab42_SE, Ab42_P
                    		FROM `presibo1`.`eqtl_master_view`
                    UNION
                    SELECT gene_id, "brain tissue", "all",  "ptau181", 
                    		ptau181_BETA, ptau181_SE, ptau181_P
                    		FROM `presibo1`.`eqtl_master_view`
                    UNION
                    SELECT gene_id, "brain tissue", "all",  "ptau231", 
                    		ptau231_BETA, ptau231_SE, ptau231_P
                    		FROM `presibo1`.`eqtl_master_view`
                    UNION
                    SELECT gene_id, "brain tissue", "all",  "ptau181_ttau", 
                    		ptau181_ttau_BETA, ptau181_ttau_SE, ptau181_ttau_P
                    		FROM `presibo1`.`eqtl_master_view`
                    UNION
                    SELECT gene_id, "brain tissue", "all",  "ptau231_ttau", 
                    		ptau231_ttau_BETA, ptau231_ttau_SE, ptau231_ttau_P
                    		FROM `presibo1`.`eqtl_master_view`
                    UNION
                    SELECT gene_id, "brain tissue", "all",  "C4A", 
                    		C4A_BETA, C4A_SE, C4A_P
                    		FROM `presibo1`.`eqtl_master_view`
                    UNION
                    SELECT gene_id, "brain tissue", "all",  "C4B", 
                    		C4B_BETA, C4B_SE, C4B_P
                    		FROM `presibo1`.`eqtl_master_view`
                    UNION
                    SELECT gene_id, "brain tissue", "all",  "PPP2CB", 
                    		PPP2CB_BETA, PPP2CB_SE, PPP2CB_P
                    		FROM `presibo1`.`eqtl_master_view`
                    UNION
                    SELECT gene_id, "brain tissue", "all",  "PPP2CA", 
                    		PPP2CA_BETA, PPP2CA_SE, PPP2CA_P
                    		FROM `presibo1`.`eqtl_master_view`;'
    print(initQuery2)
    dbGetQuery(conn, initQuery2)
    
    if(input$downSig2 != ''){
        varQuery <- "SELECT * FROM `presibo1`.`signature_master_view`
                      WHERE `P-Value` < ?id;"
        query <- sqlInterpolate(conn, varQuery, id = input$downSig2)
    } else {
        query <- "SELECT `Gene ID`, `Source`, `Subgroup`, `Outcome`, ROUND(`Beta`, 3) AS `Beta`, ROUND(`SE`, 3) AS `SE`, `P-Value` FROM `presibo1`.`signature_master_view`;"
    }
    
    dbGetQuery(conn, query)
  })
  
  output$view2 <- renderDataTable({
    current_data <- dfSig2()
    # Apply scientific notation to the specified columns
    formatted_data <- force_scientific(current_data, 
                                       columns = c("P-Value"), 
                                       digits = 5)
    # Apply scientific notation on numeric cols
    datatable(formatted_data)
  })
  
  # output$downloadSig2 <- downloadHandler(
  #   filename = function() { paste("Signature", "xlsx", sep = ".")},
  #   content = function(file) {write_xlsx(dfSig2(), path = file)}
  # )
  
  ##############Signature Guided Network Search##############
  dfSigNet <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = 'presibo1',
      host = dbconf$server,
      username = dbconf$uid,
      password = dbconf$pwd
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    
    z=-99999
    if(input$sigNet != ""){
        z = input$sigNet
    }
    
    initQuery <- 'CREATE OR REPLACE VIEW `presibo1`.`all_networks_view` AS
                  	SELECT presibo_network_id, module_kme, gene_id
                  	FROM `presibo1`.`presibo_Jaeyoon_ADNI_Brain_Network`;'
    print(initQuery)
    dbGetQuery(conn, initQuery)
    
    varQuery <- 'SELECT presibo_network_id AS `PreSiBO Network ID`, 
                  module_id AS `Module ID`, 
                  omics_source AS `Omics Source`, 
                  discovery_study AS `Discovery Study`, 
                  discovery_tissue AS `Discovery Tissue`, 
                  validation_study AS `Validation Study`, 
                  validation_tissue AS `Validation Tissue`, 
              		tissue_or_cell_type AS `Tissue/Cell Type`,
              		module_color AS `Module Color`, 
              		zsummary AS `Z Summary`, 
              		module_pval_ad_vs_asymad AS `P-val (ADvsAsymAD)`, 
              		module_pval_ad AS `P-val (AD)`
              		FROM `presibo1`.`network_reference_table`
              		WHERE presibo_network_id IN
              			(SELECT presibo_network_id
              			FROM `presibo1`.`all_networks_view`
                          WHERE gene_id = ?id)
                  AND zsummary > ?zsumm;'
    query <- sqlInterpolate(conn, varQuery, id = input$netSearch, zsumm = z)
    print(query)
    
    dbGetQuery(conn, query)
  })
  
  output$viewtbl0 <- renderDataTable({
    current_data <- dfSigNet()
    # Apply scientific notation to the specified columns
    formatted_data <- force_scientific(current_data, 
                                       columns = c("P-val (ADvsAsymAD)", "P-val (AD)"), 
                                       digits = 5)
    datatable(formatted_data, selection = list(mode = 'single', target = "row", 
                                           selected = 1
    ), 
    ) %>% formatStyle(2, cursor = 'pointer')
  })
  
  # output$downloadSigNet <- downloadHandler(
  #   filename = function() { paste("Signature_Network", "xlsx", sep = ".")},
  #   content = function(file) {write_xlsx(dfSigNet(), path = file)}
  # )
  
  output$sgn_output1 <- renderText({ 
    if (input$netSearch != '') {
      paste("Gene networks containing ", input$netSearch)
    }
  })
  
  output$sgn_output2 <- renderText({ 
    row_count = input$viewtbl0_rows_selected
    paste("Profile of genes in selected network ", dfSigNet()[row_count,2])
  })
  
  dfSigNet2 <- reactive({
    # renderPrint(input$viewtbl0_rows_selected)
    conn <- dbConnect(
      MySQL(),
      dbname = "presibo1",
      host = dbconf$server,
      username = dbconf$uid,
      password = dbconf$pwd
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    
    initQuery <- 'CREATE OR REPLACE VIEW `presibo1`.`all_networks_view` AS
                  	SELECT presibo_network_id, module_kme, gene_id, ensgid
                  	FROM `presibo1`.`presibo_Jaeyoon_ADNI_Brain_Network`;'
    print(initQuery)
    dbGetQuery(conn, initQuery)
    
    # Note - Can use ENSGID here instead of gene ID as its more consistent
    varInitQuery2 <- "CREATE OR REPLACE VIEW `presibo1`.`network_eqtl_master_view` AS
                      	SELECT * FROM  `presibo1`.`Jun_eQTL_Outcome_Combined`
                          WHERE ensgid IN 
                          (SELECT ensgid FROM `presibo1`.`all_networks_view`
                      		WHERE presibo_network_id = ?id);"
    row_count = input$viewtbl0_rows_selected
    selected_pn_number = dfSigNet()[row_count,1]
    initQuery2 <- sqlInterpolate(conn, varInitQuery2, id = selected_pn_number)
    print(initQuery2)
    dbGetQuery(conn, initQuery2)
    

    query <- "SELECT gene_id AS `Gene ID`, meta3_all_P AS `Meta3All-P`, meta2_braak_P AS `Meta2-BRAAK-P`,
                      meta2_cerad_P AS `Meta2-CERAD-P`, PSD95_P AS `PSD95-P`, Ab42_P AS `Ab42-P`, ptau181_P AS `ptau181-P`, 
                      ptau231_P AS `ptau231-P`, ptau181_ttau_P AS `ptau181_ttau-P`, ptau231_ttau_P AS `ptau231_ttau-P`, 
                      C4A_P AS `C4A-P`, C4B_P AS `C4B-P`, PPP2CB_P AS `PPP2CB-P`, PPP2CA_P AS `PPP2CA-P`
                  FROM `presibo1`.`network_eqtl_master_view`;"
    dbGetQuery(conn, query)
  })
  
  output$viewtbl12_1 <- renderDataTable({
    current_data <- dfSigNet2()
    # Apply scientific notation to the specified columns
    formatted_data <- force_scientific(current_data, 
                                       columns = c("Meta3All-P", "Meta2-BRAAK-P",
                                                   "Meta2-CERAD-P", "PSD95-P",
                                                   "Ab42-P", "ptau181-P",
                                                   "ptau231-P", "ptau181_ttau-P",
                                                   "ptau231_ttau-P", "C4A-P",
                                                   "C4B-P", "PPP2CB-P", "PPP2CA-P"
                                                   ), 
                                       digits = 3)
    datatable(formatted_data)
  })
  
  # output$downloadSigNet2 <- downloadHandler(
  #   filename = function() { paste("Signature_Network", "xlsx", sep = ".")},
  #   content = function(file) {write_xlsx(dfSigNet2(), path = file)}
  # )
  
  ## Module igraph
  
  ## graph generator function 
  module_igraph <- function(){
    conn <- dbConnect(
      MySQL(),
      dbname = "presibo1",
      host = dbconf$server,
      username = dbconf$uid,
      password = dbconf$pwd
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    
    # 1. get top N gene members - query top N gene members data
    N = 20
    row_count = input$viewtbl0_rows_selected
    selected_pn_number = dfSigNet()[row_count,1]
    
    var_query_mem <- "SELECT * FROM `presibo1`.`all_networks_view`
                		WHERE presibo_network_id = ?id
                		ORDER BY module_kme DESC
                		LIMIT ?n;" 
    query_mem <- sqlInterpolate(conn, var_query_mem, 
                                     id = selected_pn_number,
                                     n = N)
    
    df_gene_mem <- dbGetQuery(conn, query_mem)
    df_top_nodes <- df_gene_mem
    
    
    # # 2. get top nodes tom edges
    # df_edge <- read.csv("/restricted/projectnb/ai4ad/sahelijo/network/results/wgcna/networks/Ast-M10_edge.tsv", sep="\t")
    # df_edge_subset <- df_edge[df_edge$from %in% df_top_nodes$ensgid & df_edge$to %in% df_top_nodes$ensgid, ]
    
    ## nodes specs
    nodes <- df_top_nodes[, "gene_id"]
    # from_nodes <- df_edge_subset$alt_from
    # to_nodes <- df_edge_subset$alt_to
    # edge_weights <- (rescale(df_edge_subset$tom, to = c(1,10)))
    # edges <- data.frame(from = from_nodes, to = to_nodes, weight = edge_weights)
    
    df_node_sizes <- df_top_nodes[, c("gene_id", "module_kme")]
    rownames(df_node_sizes) <- df_node_sizes$gene_id
    
    # # graph (if edge data present)
    # g <- graph_from_data_frame(edges, directed = F)
    # g <- igraph::simplify(g, remove.multiple = T)
    
    # graph (if edge data missing)
    g <- make_full_graph(N)
    V(g)$name <- nodes
    
    # assign node sizes
    V(g)$size <- round(rescale(df_node_sizes[V(g)$name, "module_kme"], to = c(1,20)))
    # E(g)$weight <- rescale(E(g)$weight, to = c(1,20))
    
    return(g)
  }
  
  # plot igraph on Generate Graph button press
  observeEvent(input$genGraph, {
    # get PN number
    row_count = input$viewtbl0_rows_selected
    selected_pn_number = dfSigNet()[row_count,1]
    # render plot
    output$igraphModulePlot <- renderPlot({
      if(!is.na(selected_pn_number)){
        g <- module_igraph()  # Get the reactive graph data
        plot(g, main = paste("PreSiBO Network ID:", selected_pn_number), 
             vertex_color = "lightblue", vertex.label.cex = 0.8)
      }
    })
  })
  
  # Reset the plot on new search
  observeEvent(input$netSearch, {
      output$igraphModulePlot <- NULL
  })
  
  ##############Network Guided Genetic Search (Network Subgroups)##############
  
  dfNetGene <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = 'presibo1',
      host = dbconf$server,
      username = dbconf$uid,
      password = dbconf$pwd
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    
    p=1
    ## DO NOT REMOVE
    ## uncomment when more p-value PRSs are available
    # if(input$source2 == "P<0.05"){
    #   p = 0.05
    # } else if(input$source2 == "P<0.001"){
    #   p = 0.001
    # } else if(input$source2 == "P<5E-08"){
    #   p = 5e-8
    # } else {
    #   p = 1
    # }
    z=-9999
    
    initQuery <- "CREATE OR REPLACE VIEW `presibo1`.`all_networks_view` AS
                  	SELECT presibo_network_id, module_kme, gene_id, ensgid
                  	FROM `presibo1`.`presibo_Jaeyoon_ADNI_Brain_Network`;"
    dbSendQuery(conn, initQuery)
    
    
    varInitQuery2 <- "CREATE OR REPLACE VIEW `presibo1`.`filtered_network_ref_view` AS
                        SELECT presibo_network_id AS `PreSiBO Network ID`, 
                        module_id AS `Module ID`, 
                        omics_source AS `Omics Source`, 
                        discovery_study AS `Discovery Study`, 
                        discovery_tissue AS `Discovery Tissue`, 
                        validation_study AS `Validation Study`, 
                        validation_tissue AS `Validation Tissue`, 
                        tissue_or_cell_type AS `Tissue/Cell Type`,
                        module_color AS `Module Color`, 
                        zsummary AS `Z Summary`, 
                        module_pval_ad_vs_asymad AS `P-val (ADvsAsymAD)`, 
                        module_pval_ad AS `P-val (AD)`
                    	FROM `presibo1`.`network_reference_table`
                    		WHERE presibo_network_id IN
                    			(SELECT presibo_network_id
                    			FROM `presibo1`.`all_networks_view`
                                WHERE gene_id = ?id);"
    initQuery2 <- sqlInterpolate(conn, varInitQuery2, id = input$geneProfSearch)
    dbSendQuery(conn, initQuery2)
    
    if(input$source1 == "All"){
      varQuery <- "SELECT * FROM `presibo1`.`filtered_network_ref_view`;"
      query <- varQuery
    } else if(input$source1 == "Brain-Brain Transcriptome" & p == 0.001 ){
      varQuery <- "SELECT * FROM `presibo1`.`filtered_network_ref_view`
                                  WHERE presibo_network_id >= 'PN000079' 
                                  AND presibo_network_id <= 'PN000107';"
      query <- varQuery
    } else {
      query <- "SELECT * FROM `presibo1`.`filtered_network_ref_view`"
    }
    
    print(query)
    dbGetQuery(conn, query)
  })
  
  output$viewtbl <- renderDataTable({
    current_data <- dfNetGene()
    # Apply scientific notation to the specified columns
    formatted_data <- force_scientific(current_data, 
                                       columns = c("P-val (ADvsAsymAD)", 
                                                   "P-val (AD)"), 
                                       digits = 5)
    datatable(formatted_data, selection = list(mode = 'single', target = "row", 
                                            selected = 1), 
    ) %>% formatStyle(2, cursor = 'pointer')
  })
  
  # output$downloadNetGene <- downloadHandler(
  #   filename = function() { paste("Network_Genetic", "xlsx", sep = ".")},
  #   content = function(file) {write_xlsx(dfNetGene(), path = file)}
  # )
  
  output$ngs_output2 <- renderText({
    row_count = input$viewtbl_rows_selected
    paste("PRS associations for selected network", dfNetGene()[row_count, 2])
  })
  
  dfNetGene2 <- reactive({
    # renderPrint(input$viewtbl_rows_selected)
    conn <- dbConnect(
      MySQL(),
      dbname = 'presibo1',
      host = dbconf$server,
      username = dbconf$uid,
      password = dbconf$pwd
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    
    p = 1
    if(input$netGene2 != ""){
      p = input$netGene2
    }
    
    varQuery <- "SELECT presibo_network_id AS `PreSiBO Network ID`, 
                  prs_study AS `PRS Study`, 
                  network_subgroup AS `Network Subgroup`, 
                  network_study AS `Network Study`, 
              		network_data AS `Network Data`, 
              		adjustment AS `Adjustment`, 
              		prs_gwas_source AS `PRS GWAS Source`, 
              		network_color AS `Network Color`, 
              		outcome AS `Outcome`, 
              		beta AS `BETA`, 
              		se AS `SE`, 
              		p AS `P`
              FROM presibo1.module_prs_associations
              WHERE presibo_network_id = ?id
              AND p < ?pval;"
    query <- sqlInterpolate(conn, varQuery, 
                            id = dfNetGene()[input$viewtbl_rows_selected, 1],
                            pval = p)
    print(query)
    dbGetQuery(conn, query)
  })
  
  output$viewtbl2_1 <- renderDataTable({
    current_data <- dfNetGene2()
    # Apply scientific notation to the specified columns
    formatted_data <- force_scientific(current_data, 
                                       columns = c("P"), 
                                       digits = 5)
    datatable(formatted_data)
  })
  
  # output$downloadNetGene2 <- downloadHandler(
  #   filename = function() { paste("Signature_Network", "xlsx", sep = ".")},
  #   content = function(file) {write_xlsx(dfNetGene2(), path = file)}
  # )
  

  
  ##############Network Drug Search- Top table##############  
  dfNetDrug <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = 'presibo1',
      host = dbconf$server,
      username = dbconf$uid,
      password = dbconf$pwd
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    
    initQuery <- "CREATE OR REPLACE VIEW `presibo1`.`all_networks_view` AS
                  	SELECT presibo_network_id, module_kme, gene_id, ensgid
                  	FROM `presibo1`.`presibo_Jaeyoon_ADNI_Brain_Network`;"
    print(initQuery)
    dbSendQuery(conn, initQuery)
    
    
    
    varInitQuery2 <- "CREATE OR REPLACE VIEW `presibo1`.`selected_networks_ref_view` AS
                      	SELECT presibo_network_id AS `PreSiBO Network ID`, 
                        module_id AS `Module ID`, 
                        omics_source AS `Omics Source`, 
                        discovery_study AS `Discovery Study`, 
                        discovery_tissue AS `Discovery Tissue`, 
                        validation_study AS `Validation Study`, 
                        validation_tissue AS `Validation Tissue`, 
                        tissue_or_cell_type AS `Tissue/Cell Type`,
                        module_color AS `Module Color`, 
                        zsummary AS `Z Summary`, 
                        module_pval_ad_vs_asymad AS `P-val (ADvsAsymAD)`, 
                        module_pval_ad AS `P-val (AD)`
                      FROM `presibo1`.`network_reference_table`
                  			WHERE presibo_network_id IN
                  				(SELECT presibo_network_id
                  				FROM `presibo1`.`all_networks_view`
                  				WHERE gene_id = ?id);"
    initQuery2 <- sqlInterpolate(conn, varInitQuery2, id = input$netSearch)
    print(initQuery2)
    dbSendQuery(conn, initQuery2)
    
    if(input$source == "Brain-Brain Transcriptome"){
      query <- "SELECT * FROM `presibo1`.`selected_networks_ref_view`
                  WHERE `Omics Source` = 'Bulk_RNA_seq'
                  AND (`Discovery Tissue` = 'brain' AND `Validation Tissue` = 'brain');"
    } else if(input$source == "All"){
        query <-"SELECT * FROM `presibo1`.`selected_networks_ref_view`"
    }
    
    print(query)
    dbGetQuery(conn, query)
  })
  
  output$viewtable1 <- renderDataTable({
    current_data <- dfNetDrug()
    # Apply scientific notation to the specified columns
    formatted_data <- force_scientific(current_data, 
                                       columns = c("P-val (ADvsAsymAD)", 
                                                   "P-val (AD)"), 
                                       digits = 5)
    datatable(formatted_data, selection = list(mode = 'single', target = "row", 
                                            selected = 1), 
              ) %>% formatStyle(2, cursor = 'pointer')
  })
  
  output$ngd_output1 <- renderText({
    if (input$source != '') {
      paste(input$source)
    }
  })
  
  output$ngd_output2 <- renderText({ 
    row_count = input$viewtable1_rows_selected
    paste("Approved drugs targeting genes in", dfNetDrug()[row_count, 2])
  })
  
  # output$downloadNetDrug <- downloadHandler(
  #   filename = function() { paste("Network_Drug", "xlsx", sep = ".")},
  #   content = function(file) {write_xlsx(dfNetDrug(), path = file)}
  # )
  
##############Network Drug Search- Bottom table############## 
  
  dfNetDrug2 <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = 'presibo1',
      host = dbconf$server,
      username = dbconf$uid,
      password = dbconf$pwd
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    
    varQuery <- "SELECT
                  pert_iname AS `Drug Name`,
                  clinical_phase AS `Clinical Phase`,
                  moa AS `Mechanism of Action`,
                  disease_area AS `Disease Area`,
                  indication AS `Indication`,
                  ensgid AS `ENSGID`,
                  gene_id AS `Gene ID`
                 FROM `presibo1`.`AI4AD_Gene_GGDD_BIDRH_all_drugs`
                  WHERE ensgid IN 
                  (SELECT ensgid FROM `presibo1`.`all_networks_view`
                  		WHERE presibo_network_id = ?id)
                  AND clinical_phase='Launched';"
    selected_pn_number <- dfNetDrug()[input$viewtable1_rows_selected, 1]
    query <- sqlInterpolate(conn, varQuery, id = selected_pn_number)
    # print(query)
    dbGetQuery(conn, query)
  })
  
  output$viewtable12_1 <- renderDataTable({
    datatable(dfNetDrug2())
  })
  
  # output$downloadNetDrug2 <- downloadHandler(
  #   filename = function() { paste("Signature_Network", "xlsx", sep = ".")},
  #   content = function(file) {write_xlsx(dfNetDrug2(), path = file)}
  # )
  
  ##### Network Guided Drugs Disease Areas Plot #####
  output$diseaseAreaDonut <- renderPlotly({
    l_da <- dfNetDrug2()[, "Disease Area"]
    
    # Check if data is valid
    if (is.null(l_da)) {
      print("Empty plotly")
      return(plotly_empty())
    }
    
    # aggregate data here
    df_da <- as.data.frame(table(l_da))
    
    # Generate the donut chart
    plot_ly(
      df_da,
      labels = ~l_da,
      values = ~Freq,
      type = 'pie',
      hole = 0.5
    ) %>%
      layout(
        title = "Drug Disease Areas",
        annotations = list(
          text = "",
          font = list(size = 20),
          showarrow = FALSE,
          x = 0.5,
          y = 0.5
        )
      )
  })
  
  
  ##############Drug Search##############
  dfInd <- reactive({
    conn <- dbConnect(
      MySQL(),
      # encoding = 'UTF-8',
      dbname = 'presibo1',
      host = dbconf$server,
      username = dbconf$uid,
      password = dbconf$pwd
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    
    
    # if(input$searchInd != ""){
    #     varQuery <- "SELECT presibo_drug_id, pert_iname AS drug_name, clinical_phase, moa, disease_area, indication, gene_id
    #                 FROM AI4AD_Gene_GGDD_BIDRH_all_drugs
    #                 WHERE 
    #                 CONCAT(presibo_drug_id, pert_iname, clinical_phase, moa, disease_area, indication, gene_id)
    #                 LIKE ?id1;"
    #     
    #     query <- sqlInterpolate(conn, varQuery, id1 = paste0("%", input$searchInd, "%"))
    # } else {
    #     query <- "SELECT presibo_drug_id, pert_iname AS drug_name, clinical_phase, moa, disease_area, indication, gene_id
    #                 FROM AI4AD_Gene_GGDD_BIDRH_all_drugs;"
    # }
    
    # coalesce replaces null with empty string. Makes sure concat does not concat with NULL
    varQuery <- "SELECT presibo_drug_id AS `PreSiBO Drug ID`, 
                        pert_iname AS `Drug Name`, 
                        clinical_phase AS `Clinical Test Phase`, 
                        moa AS `Mechanism of Action`, 
                        disease_area AS `Disease Area`, 
                        indication AS `Indication`, 
                        gene_id AS `Gene Target`
                  FROM AI4AD_Gene_GGDD_BIDRH_all_drugs
                  WHERE 
                  CONCAT(
					          presibo_drug_id,
                    COALESCE(pert_iname, ''),
                    COALESCE(clinical_phase, ''),
                    COALESCE(moa, ''),
                    COALESCE(disease_area, ''),
                    COALESCE(indication, ''),
                    COALESCE(gene_id, '')
					        )
                  LIKE ?id1;"
    query <- sqlInterpolate(conn, varQuery, id1 = paste0("%", str_trim(input$searchInd), "%"))
    
    print(query)
    dbGetQuery(conn, query)
  })
  
  output$viewtable2 <- renderDataTable({
    datatable(dfInd(), options = list(
        dom = 'ltip',
        pageLength = 25
      )
    )
  })
  
  # output$downloadIndic <- downloadHandler(
  #   filename = function() { paste("Drug", "xlsx", sep = ".")},
  #   content = function(file) {write_xlsx(dfInd(), path = file)}
  # )
  
  
  
}