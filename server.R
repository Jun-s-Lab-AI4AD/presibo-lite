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

config_file = "config_dp.yml"
# define some credentials
creds <- config::get(file = config_file)
login_creds <- creds$login

credentials <- data.frame(
  user = login_creds$user1,
  password = login_creds$pwd1,
  stringsAsFactors = FALSE
)

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
      
      varQuery <- "SELECT t1.variant_id, t1.snp_id, t1.a1, t1.a2, t1.freq1, t1.beta, t1.se, t1.p,
                        t2.gene_id, t2.geneid_left, t2.geneid_right, t2.dist_left, t2.dist_right
                        FROM ?id t1
                        INNER JOIN `view_variant_reference_gene` t2
                        USING (variant_id)
                        WHERE t1.p <= ?id1;"
      varQuery2 <- "SELECT t1.variant_id, t1.snp_id, t1.a1, t1.a2, t1.freq1, t1.effect as beta, t1.se, t1.p,
                        t2.gene_id, t2.geneid_left, t2.geneid_right, t2.dist_left, t2.dist_right
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
      else if(input$var == "Biomarker-PET-FDG-Angular"){
          query <- sqlInterpolate(conn, varQuery2,
                                  id = dbQuoteIdentifier(conn, 'Nho_ADNI_GWAS_Variant_PET_FDG_ANGULAR_ALL_AgeSexAdj'),
                                  id1 = input$predFilter
          )
      }
      else if(input$var == "Biomarker-PET-FDG-Cingulate"){
        query <- sqlInterpolate(conn, varQuery2,
                                id = dbQuoteIdentifier(conn, 'Nho_ADNI_GWAS_Variant_PET_FDG_CINGULATE_ALL_AgeSexAdj'),
                                id1 = input$predFilter
        )
      }
      else if(input$var == "Biomarker-PET-FDG-Temporal"){
        query <- sqlInterpolate(conn, varQuery2,
                                id = dbQuoteIdentifier(conn, 'Nho_ADNI_GWAS_Variant_PET_FDG_TEMPORAL_ALL_AgeSexAdj'),
                                id1 = input$predFilter
        )
      }
      else if(input$var == "Biomarker-MRI-EntCtx"){
        query <- sqlInterpolate(conn, varQuery2,
                                id = dbQuoteIdentifier(conn, 'Nho_ADNI_GWAS_Variant_MRI_EntCtx_ALL_AgeSexEduICVAdj'),
                                id1 = input$predFilter
        )
      }
      else if(input$var == "Biomarker-MRI-HippVol"){
        query <- sqlInterpolate(conn, varQuery2,
                                id = dbQuoteIdentifier(conn, 'Nho_ADNI_GWAS_Variant_MRI_HippVol_ALL_AgeSexEduICVAdj'),
                                id1 = input$predFilter
        )
      }
      else if(input$var == "Biomarker-MRI-ParietalCtx"){
        query <- sqlInterpolate(conn, varQuery2,
                                id = dbQuoteIdentifier(conn, 'Nho_ADNI_GWAS_Variant_MRI_ParietalCtx_ALL_AgeSexEduICVAdj'),
                                id1 = input$predFilter
        )
      }
      else if(input$var == "Biomarker-MRI-TemporalCtx"){
        query <- sqlInterpolate(conn, varQuery2,
                                id = dbQuoteIdentifier(conn, 'Nho_ADNI_GWAS_Variant_MRI_TemporalCtx_ALL_AgeSexEduICVAdj'),
                                id1 = input$predFilter
        )
      }
      print(query)
    # } # end of if (strsplit(input$targetSearch, ":") == input$targetSearch)
    dbGetQuery(conn, query)
  })
  
  output$tbl <- renderDataTable({
    datatable(dfPred())
  })
  
  output$downloadPred <- downloadHandler(
    filename = function() { paste("Predictor", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfPred(), path = file)}
  )
  
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
                          		FROM `presibo1`.`eqtl_master_view`
                          UNION
                          SELECT gene_id, "astrocyte", "all", ast_avg_logFC, 
                          		ast_p_val
                          		FROM `presibo1`.`eqtl_master_view`
                          UNION
                          SELECT gene_id, "endothelial", "all", end_avg_logFC,
                          		end_p_val
                          		FROM `presibo1`.`eqtl_master_view`
                          UNION
                          SELECT gene_id, "excitory neuron", "all",
                          		exn_avg_logFC, exn_p_val
                          		FROM `presibo1`.`eqtl_master_view`
                          UNION
                          SELECT gene_id, "inhibitory neuron", "all",
                          		inn_avg_logFC, inn_p_val
                          		FROM `presibo1`.`eqtl_master_view`
                          UNION
                          SELECT gene_id, "microglia", "all",
                          		mic_avg_logFC, mic_p_val
                          		FROM `presibo1`.`eqtl_master_view`
                          UNION
                          SELECT gene_id, "oligodendrocyte", "all",
                          		oli_avg_logFC, oli_p_val
                          		FROM `presibo1`.`eqtl_master_view`
                          UNION
                          SELECT gene_id, "OPC", "all",
                          		opc_avg_logFC, opc_p_val
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
    datatable(dfSig())
  })
  
  output$downloadSig <- downloadHandler(
    filename = function() { paste("Signature", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfSig(), path = file)}
  )
  
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
        query <- "SELECT * FROM `presibo1`.`signature_master_view`;"
    }
    
    dbGetQuery(conn, query)
  })
  
  output$view2 <- renderDataTable({
    datatable(dfSig2())
  })
  
  output$downloadSig2 <- downloadHandler(
    filename = function() { paste("Signature", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfSig2(), path = file)}
  )
  
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
    
    varQuery <- 'SELECT presibo_network_id, module_id, omics_source, 
                  discovery_study, discovery_tissue, 
                  validation_study, validation_tissue, 
              		tissue_or_cell_type, module_color, zsummary, 
              		module_pval_ad_vs_asymad, module_pval_ad
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
    datatable(dfSigNet(), selection = list(mode = 'single', target = "row", 
                                           selected = 1
    ), 
    ) %>% formatStyle(2, cursor = 'pointer')
  })
  
  output$downloadSigNet <- downloadHandler(
    filename = function() { paste("Signature_Network", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfSigNet(), path = file)}
  )
  
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
    

    query <- "SELECT gene_id AS `Gene ID`, meta3_all_P AS `Meta3All-P`, ast_p_val AS `Astrocyte-P`, end_p_val AS `Endothelial-P`, 
              		    exn_p_val AS `ExcitoryNeuron-P`, inn_p_val AS `InhibitoryNeuron-P`, mic_p_val AS `Microglia-P`, 
                      oli_p_val AS `Oligodendrocyte-P`, opc_p_val AS `OPC-P`, meta2_braak_P AS `Meta2-BRAAK-P`,
                      meta2_cerad_P AS `Meta2-CERAD-P`, PSD95_P AS `PPP2CA-P`, Ab42_P AS `Ab42-P`, ptau181_P AS `ptau181-P`, 
                      ptau231_P AS `ptau231-P`, ptau181_ttau_P AS `ptau181_ttau-P`, ptau231_ttau_P AS `ptau231_ttau-P`, 
                      C4A_P AS `C4A-P`, C4B_P AS `C4B-P`, PPP2CB_P AS `PPP2CB-P`, PPP2CA_P AS `PPP2CA-P`
                  FROM `presibo1`.`network_eqtl_master_view`;"
    dbGetQuery(conn, query)
  })
  
  output$viewtbl12_1 <- renderDataTable({
    datatable(dfSigNet2())
  })
  
  output$downloadSigNet2 <- downloadHandler(
    filename = function() { paste("Signature_Network", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfSigNet2(), path = file)}
  )
  
  ##############Network Guided Genetic Search##############
  output$ngs_output1 <- renderText({
    paste(input$source1)
  })
  
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
    if(input$source2 == "P<0.05"){
      p = 0.05
    } else if(input$source2 == "P<0.001"){
      p = 0.001
    } else if(input$source2 == "P<5E-08"){
      p = 5e-8
    } else {
      p = 1
    }
    z=-9999
    
    initQuery <- "CREATE OR REPLACE VIEW `presibo1`.`all_networks_view` AS
                  	SELECT presibo_network_id, module_kme, gene_id, ensgid
                  	FROM `presibo1`.`presibo_Jaeyoon_ADNI_Brain_Network`;"
    dbSendQuery(conn, initQuery)
    
    varInitQuery2 <- "CREATE OR REPLACE VIEW `presibo1`.`filtered_network_ref_view` AS
                        SELECT presibo_network_id, module_id, omics_source, discovery_study, 
                        discovery_tissue, validation_study, validation_tissue, 
                    		tissue_or_cell_type, module_color, zsummary, 
                    		module_pval_ad_vs_asymad, module_pval_ad
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
    datatable(dfNetGene(), selection = list(mode = 'single', target = "row", 
                                            selected = 1), 
    ) %>% formatStyle(2, cursor = 'pointer')
  })
  
  output$downloadNetGene <- downloadHandler(
    filename = function() { paste("Network_Genetic", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfNetGene(), path = file)}
  )
  
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
    
    # if (input$viewtbl_rows_selected == 1) {
    #   query <- paste0("SELECT * FROM module_genes_m17;")
    #   if (input$sigGeneSearch2 != "") {
    #     query <- paste0("SELECT * FROM module_genes_m17 
    #                     WHERE CONCAT(ENSGID, `Gene Name`, `Tissue P`, `Ast P`, `End P`, `Exn P`, `Inn P`, `Mic P`, `Oli P`, `Opc P`) 
    #                     LIKE '%", input$sigGeneSearch2, "%';")
    #   }
    #   dbGetQuery(conn, query)
    # }
    # else if (input$viewtbl_rows_selected == 2) {
    #   query <- paste0("SELECT * FROM module_genes_m2;")
    #   if (input$sigGeneSearch2 != "") {
    #     query <- paste0("SELECT * FROM module_genes_m2 
    #                     WHERE CONCAT(ENSGID, `Gene Name`, `Tissue P`, `Ast P`, `End P`, `Exn P`, `Inn P`, `Mic P`, `Oli P`, `Opc P`) 
    #                     LIKE '%", input$sigGeneSearch2, "%';")
    #   }
    #   dbGetQuery(conn, query)
    # }
    # else if (input$viewtbl_rows_selected == 3) {
    #   query <- paste0("SELECT * FROM module_genes_m45;")
    #   if (input$sigGeneSearch2 != "") {
    #     query <- paste0("SELECT * FROM module_genes_m45 
    #                     WHERE CONCAT(ENSGID, `Gene Name`, `Tissue P`, `Ast P`, `End P`, `Exn P`, `Inn P`, `Mic P`, `Oli P`, `Opc P`) 
    #                     LIKE '%", input$sigGeneSearch2, "%';")
    #   }
    #   dbGetQuery(conn, query)
    # }
    
    p = 1
    if(input$netGene2 != ""){
      p = input$netGene2
    }
    
    varQuery <- "SELECT presibo_network_id, prs_study, network_subgroup, network_study, 
              		network_data, adjustment, prs_gwas_source, network_color, 
              		outcome, beta, se, p
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
    datatable(dfNetGene2())
  })
  
  output$downloadNetGene2 <- downloadHandler(
    filename = function() { paste("Signature_Network", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfNetGene2(), path = file)}
  )
  

  
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
                  	SELECT presibo_network_id, module_kme, gene_id
                  	FROM `presibo1`.`presibo_Jaeyoon_ADNI_Brain_Network`;"
    print(initQuery)
    dbSendQuery(conn, initQuery)
    
    varInitQuery2 <- "CREATE OR REPLACE VIEW `presibo1`.`selected_networks_ref_view` AS
                      	SELECT presibo_network_id, module_id, omics_source, discovery_study, discovery_tissue, validation_study, validation_tissue, 
                      			tissue_or_cell_type, module_color, zsummary, 
                      			module_pval_ad_vs_asymad, module_pval_ad
                      			FROM `presibo1`.`network_reference_table`
                      			WHERE presibo_network_id IN
                      				(SELECT presibo_network_id
                      				FROM `presibo1`.`all_networks_view`
                      				WHERE gene_id = ?id);"
    initQuery2 <- sqlInterpolate(conn, varInitQuery2, id = input$netSearch)
    print(initQuery2)
    dbSendQuery(conn, initQuery2)
    
    if(input$source == "Brain-Brain Cell-Level (Astrocyte) Transcriptome"){
        query <- "SELECT * FROM `presibo1`.`selected_networks_ref_view`
                  WHERE omics_source = 'transcriptome'
                  AND tissue_or_cell_type = 'Ast';"
    } else if(input$source == "Brain-Brain Cell-Level (Excitatory Neuron) Transcriptome"){
        query <- "SELECT * FROM `presibo1`.`selected_networks_ref_view`
                    WHERE omics_source = 'transcriptome'
                    AND tissue_or_cell_type = 'Ex';"
    } else if(input$source == "Brain-Brain Cell-Level (Inhibitory Neuron) Transcriptome"){
        query <- "SELECT * FROM `presibo1`.`selected_networks_ref_view`
                    WHERE omics_source = 'transcriptome'
                    AND tissue_or_cell_type = 'In';"
    } else if(input$source == "Brain-Brain Cell-Level (Microglia) Transcriptome"){
        query <- "SELECT * FROM `presibo1`.`selected_networks_ref_view`
                      WHERE omics_source = 'transcriptome'
                      AND tissue_or_cell_type = 'Mic';"
    } else if(input$source == "Brain-Brain Cell-Level (Oligodendrocyte) Transcriptome"){
        query <- "SELECT * FROM `presibo1`.`selected_networks_ref_view`
                        WHERE omics_source = 'transcriptome'
                        AND tissue_or_cell_type = 'Oli';"
    } else if(input$source == "Brain-Brain Cell-Level (OPC) Transcriptome"){
        query <- "SELECT * FROM `presibo1`.`selected_networks_ref_view`
                        WHERE omics_source = 'transcriptome'
                        AND tissue_or_cell_type = 'Opc';"
    } else if(input$source == "Brain-Blood Transcriptome"){
        query <- "SELECT * FROM `presibo1`.`selected_networks_ref_view`
                  WHERE omics_source = 'Bulk_RNA_seq'
                  AND ((discovery_tissue = 'brain' AND validation_tissue = 'blood') OR (discovery_tissue = 'blood' AND validation_tissue = 'brain'));"
    } else if(input$source == "Brain-Brain Proteome"){
        query <- "SELECT * FROM `presibo1`.`selected_networks_ref_view`
                  WHERE omics_source = 'proteome'
                  AND (discovery_tissue = 'brain' AND validation_tissue = 'brain');"
    } else if(input$source == "All"){
        query <-"SELECT * FROM `presibo1`.`selected_networks_ref_view`"
    }
    
    print(query)
    dbGetQuery(conn, query)
  })
  
  output$viewtable1 <- renderDataTable({
    datatable(dfNetDrug(), selection = list(mode = 'single', target = "row", 
                                            selected = 1), 
              ) %>% formatStyle(2, cursor = 'pointer')
  })
  
  #output$viewtable1 = renderDataTable(dfNetDrug(), server = FALSE, selection = 'single')
  #output$viewtable12 = renderPrint(input$viewtable1_rows_selected)
  
  output$ngd_output1 <- renderText({
    if (input$source != '') {
      paste(input$source)
    }
  })
  
  output$ngd_output2 <- renderText({ 
    row_count = input$viewtable1_rows_selected
    paste("Approved drugs targeting genes in", dfNetDrug()[row_count, 2])
  })
  
  output$downloadNetDrug <- downloadHandler(
    filename = function() { paste("Network_Drug", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfNetDrug(), path = file)}
  )
  
  dfNetDrug2 <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = 'presibo1',
      host = dbconf$server,
      username = dbconf$uid,
      password = dbconf$pwd
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    
    varQuery <- "SELECT * FROM `presibo1`.`AI4AD_Gene_GGDD_BIDRH_all_drugs`
                  WHERE ensgid IN 
                  (SELECT ensgid FROM `presibo1`.`all_networks_view`
                  		WHERE presibo_network_id = ?id);"
    selected_pn_number <- dfNetDrug()[input$viewtable1_rows_selected, 1]
    query <- sqlInterpolate(conn, varQuery, id = selected_pn_number)
    
    dbGetQuery(conn, query)
  })
  
  output$viewtable12_1 <- renderDataTable({
    datatable(dfNetDrug2())
  })
  
  output$downloadNetDrug2 <- downloadHandler(
    filename = function() { paste("Signature_Network", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfNetDrug2(), path = file)}
  )
  
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
    varQuery <- "SELECT presibo_drug_id, pert_iname AS drug_name, clinical_phase, moa, disease_area, indication, gene_id
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
  
  output$downloadIndic <- downloadHandler(
    filename = function() { paste("Drug", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfInd(), path = file)}
  )
  
  
  
}