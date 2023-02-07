

Differential_ExpressionUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      h2 ("To start the differential expression analysis, first select  the data type and then define the groups of your interest"),
      hr(),
      box(title = p("Parameters to Define the Groups of Cells", actionButton(ns("HELP_DEG_INPUT"), "", icon = icon("question-circle"), class = "btn-xs")),
          width = 3, height = NULL,
          solidHeader = TRUE, status = "primary",
          
          #parameters for first factorial
          uiOutput(ns("select_meta_fac1")),
          uiOutput(ns("range_meta_fac1")),
          
          #parameters for 2nd factorial
          uiOutput(ns("select_meta_fac2")),
          uiOutput(ns("range_meta_fac2")),
          #for reductions
          uiOutput(ns("select_reduc")),
          
          
          uiOutput(ns("dea_vis_btn")),#("button_dea")
          hr()
      ),
      
      conditionalPanel(condition = "input.dea_vis_btn2", ns = ns,
                       box(title = p("Parameters for Differential Expression Analysis", actionButton(ns("HELP_DEG_PARAMS"), "", icon = icon("question-circle"), class = "btn-xs")),
                           width = 3, height = NULL,  
                           solidHeader = TRUE, status = "primary", 
                           uiOutput(ns("min_pct")),
                           uiOutput(ns("nb_logfc")),
                           uiOutput(ns("select_method_Markers")),
                           uiOutput(ns("checkbox_only_pos")),
                           useShinyalert(),
                           
                           uiOutput(ns("dea_run_btn"))

                       ),
                       
                       box(title = p("Visualization of the Groups of Cells", actionButton(ns("HELP_DEG_VIS"), "", icon = icon("question-circle"), class = "btn-xs")),
                           width = 6, height = NULL,  
                           solidHeader = TRUE, status = "primary",
                           plotOutput(ns("dea_vis_plot")) %>% withSpinner(color="#0dc5c1"),
                           br(),
                           div(style = 'padding-left: 8em; font-size: 12pt; color: black;', 
                               textOutput(ns("state_vis_out"))),
                           br(),
                           downPlotUI(id = ns("dea_vis_export"))
                           
                           
                           
                       )
                       
      )#conditional panel
      
      
      
    ),# end of first row
    
    
    
    fluidRow(
      br(),
      br(),
      
      conditionalPanel(condition = "input.dea_vis_btn2", ns = ns,
                       
      conditionalPanel(condition = "input.dea_run_btn2", ns = ns,
                       
                       box(title = p("Differentially Expressed Genes", actionButton(ns("HELP_DEG_TABLE"), "", icon = icon("question-circle"), class = "btn-xs")),
                           height = NULL, solidHeader = TRUE, 
                           status = "primary", width = 14,
                           
                       tabsetPanel(
                         
                         id = ns("panel_deg"),
                         
                         tabPanel(
                           "Table of Differentially Expressed Genes",
                           box(width = NULL, height = NULL,  
                               solidHeader = TRUE, status = "primary", 
                               br(),
                               div(style = 'padding-left: 6em; font -size: 16pt; color: black;', 
                                   htmlOutput(ns("dea_table_stat"))),
                               br(),
                               div(style = 'padding-left: 6em; font -size: 12pt; color: black;', 
                                   textOutput(ns("dea_stat_para"))),
                               br(),
                                   
                               # div(style = 'overflow-x: scroll', dataTableOutput(ns("DEA_genes"))
                               div(style = '', tableOutput(ns("DEA_genes"))
                                  %>% withSpinner(color="#0dc5c1")),
                               br(),
                               downUI(id = ns("DEA_genes_export"))
                               
                           )
                         ),
                         tabPanel("Feature Plots of Top Differentially Expressed Genes",
                                  
                                  box(width = NULL, height = NULL,  
                                      solidHeader = TRUE, status = "primary", 
                                      uiOutput(ns("group_choice")),
                                      br(),
                                      div(style = 'padding-left: 6em; font-size: 12pt; color: black;', 
                                          textOutput(ns("fea_plot_stat"))),
                                      br(),
                                      
                                      plotOutput(ns("TopDEA"), height = 800) %>% withSpinner(color="#0dc5c1"),
                                      br(),
                                     
                                      downPlotUI(ns("fea_plot_export")),
                                      
                                  )
                         ),
                         tabPanel("GO Biological Process",
                                  box(width = NULL, height = NULL,  
                                      solidHeader = TRUE, status = "primary", 
                                      plotOutput(ns("GO_BP"), height = 800) %>% withSpinner(color="#0dc5c1"),
                                      br(),
                                      
                                      downPlotUI(ns("fea_GO_BP_export")),
                                      downloadButton(ns("download_GO_BP"), label = "Download Table", class = "btn-primary")
                                  )
                         ),
                         tabPanel("GO Molecular Function",
                                  box(width = NULL, height = NULL,  
                                      solidHeader = TRUE, status = "primary", 
                                      plotOutput(ns("GO_MF"), height = 800) %>% withSpinner(color="#0dc5c1"),
                                      br(),
                                      
                                      downPlotUI(ns("fea_GO_MF_export")),
                                      downloadButton(ns("download_GO_MF"), label = "Download Table", class = "btn-primary")
                                  )
                         ),
                         tabPanel("GO Cellular Component",
                                  box(width = NULL, height = NULL,  
                                      solidHeader = TRUE, status = "primary", 
                                      plotOutput(ns("GO_CC"), height = 800) %>% withSpinner(color="#0dc5c1"),
                                      br(),
                                      
                                      downPlotUI(ns("fea_GO_CC_export")),
                                      downloadButton(ns("download_GO_CC"), label = "Download Table", class = "btn-primary")
                                  )
                         ),
                         tabPanel("KEGG",
                                  box(width = NULL, height = NULL,  
                                      solidHeader = TRUE, status = "primary", 
                                      plotOutput(ns("KEGG"), height = 800) %>% withSpinner(color="#0dc5c1"),
                                      br(),
                                      
                                      downPlotUI(ns("fea_KEGG_export")),
                                      downloadButton(ns("download_KEGG"), label = "Download Table", class = "btn-primary")
                                  )
                         )
                       )
                         
                       )
      )# conditional panel
      )# first condiational panel
    )#2nd row
    
    
    
  )# taglist
  
}



Differential_Expression <- function(input, output, session, rval) {
  
    
    params <- reactiveValues(df_dea = NULL,
                             catch_param_vis = NULL,
                             catch_param_dea = NULL,
                             check=TRUE,
                             comment_out= NULL,
                             OrgDb = "org.Hs.eg.db",
                             organism = "hsa") # save the differential expressed genes
    
    
    
    
    
    
    
    ######### Help buttons ############
    
    
    observeEvent(input[["HELP_DEG_INPUT"]], {
      
      showModal(modalDialog(
        title = HTML(help_infos[which(help_infos$key == "HELP_DEG_INPUT"),"title"]),
        HTML(help_infos[which(help_infos$key == "HELP_DEG_INPUT"),"value"]),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    })
    
    
    
    
    observeEvent(input[["HELP_DEG_PARAMS"]], {
      
      showModal(modalDialog(
        title = HTML(help_infos[which(help_infos$key == "HELP_DEG_PARAMS"),"title"]),
        HTML(help_infos[which(help_infos$key == "HELP_DEG_PARAMS"),"value"]),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    })
    
    
    
    observeEvent(input[["HELP_DEG_VIS"]], {
      
      showModal(modalDialog(
        title = HTML(help_infos[which(help_infos$key == "HELP_DEG_VIS"),"title"]),
        HTML(help_infos[which(help_infos$key == "HELP_DEG_VIS"),"value"]),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    })
    
    
    
    observeEvent(input[["HELP_DEG_TABLE"]], {
      
      showModal(modalDialog(
        title = HTML(help_infos[which(help_infos$key == "HELP_DEG_TABLE"),"title"]),
        HTML(help_infos[which(help_infos$key == "HELP_DEG_TABLE"),"value"]),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    })
    
    
    
    
    
    
    
    
    # Reactive function to recuperate numeric column of the object
    
    numericCol <- reactive({
      
      if(!is.null(rval$seurat)){
        
        return(colnames(select_if(rval$seurat@meta.data, is.numeric)))
        
      }
      
    })
    
    
    # Reactive function to recuperate factor column of the object
    
    factorCol <- reactive({
      
      if(!is.null(rval$seurat)){
        
        return(colnames(select_if(rval$seurat@meta.data, is.factor)))#[-c("orig.ident")]
        
      }
      
    })
    
    
    getGenes <- reactive({
      
      if(!is.null(rval$seurat)){
        
        genes <- rownames(rval$seurat)
        
        return(genes)
        
      }
      
    })
    
    
    ##### starting creating the inputs
    
    
    # selecting the first factorial data
    output$select_meta_fac1<- renderUI({
      
      ns <- session$ns
      
      if(!is.null(rval$seurat)){
        selectizeInput(ns("selectMetafac1"), "Choose Annotation for Group 1", 
                       factorCol(), selected = NULL, multiple = T, 
                       options = list(maxItems = 1, placeholder = 'select 1 metadata max'))
        
      }
      
    }) 
    
    
    
    output$range_meta_fac1<- renderUI({
      
       ns <- session$ns
      
      if(!is.null(input$selectMetafac1)){
        
        selectizeInput(ns("selectLevelsFac1"), "Keep Labels for Group 1",
                       choices = levels(droplevels(rval$seurat@meta.data[,input$selectMetafac1[1]])),
                       selected = levels(droplevels(rval$seurat@meta.data[,input$selectMetafac1[1]])),
                       multiple = T, options = NULL)
        
      }
    })
    
    
    
    
    # selecting the first factorial data
    output$select_meta_fac2<- renderUI({
      
      ns <- session$ns
      
      if(!is.null(rval$seurat) & !is.null(input$selectMetafac1) & !is.null(input$selectLevelsFac1) ){ 
        
        selectizeInput(ns("selectMetafac2"), "Choose Annotation for Group 2",
                       factorCol(), selected = NULL, multiple = T,
                       options = list(maxItems = 1, placeholder = 'select 1 metadata max'))
        
      }
      
    })
    
    
    
    output$range_meta_fac2<- renderUI({
      #req(input$selectLevelsFac1)
      
      ns <- session$ns
      
      if(!is.null(input$selectMetafac2) & length(input$selectMetafac2) == 1){ 
        
        if(!is.null(input$selectLevelsFac1)){
          
          selectizeInput(ns("selectLevelsFac2"), "Keep Labels for Group 2",
                         choices = levels(droplevels(rval$seurat@meta.data[,input$selectMetafac2[1]])),
                         selected = levels(droplevels(rval$seurat@meta.data[,input$selectMetafac2[1]])),
                         multiple = T, options = NULL)
          
          
        }
      }
      
    })
    
    
    # fetch reductions from seurat object when reduction is selected
    ####create an input using the reductions
    
    output$select_reduc <- renderUI({
      ns <- session$ns
      
      if(!is.null(input$selectMetafac1) & !is.null(input$selectMetafac2)){
      
#      if(!is.null(input$selectLevelsFac1) & !is.null(input$selectLevelsFac2)){
        reduc <- Reductions(rval$seurat)
        
        if(length(reduc) > 0){
          selectInput(ns("sel_rd_vis"), label = "Choose Embedding for Display", 
                      choices = reduc, selected = tail(reduc,1))
        }else{NULL}
        
      }
      
    })
    
    
    #visuallize button
    output$dea_vis_btn <- renderUI({
      
       ns <- session$ns
      
      if(!is.null(input$selectMetafac1) | !is.null(input$selectMetafac2)){
        
        actionButton(ns("dea_vis_btn2"), label = "Next")
        
      }
      
    })
    ####################################################################
    output$select_method_Markers <- renderUI({
      
      ns <- session$ns
      
      if(!is.null(rval$seurat)){
        selectInput(ns("method_Markers"), label = "Choose Method to Identify Differentially Expressed Genes", 
                    choices = list("wilcox" = "wilcox", "bimod" = "bimod", "t" = "t", "negbinom" = "negbinom", "poisson" = "poisson", "LR" = "LR"), 
                    selected = "wilcox")
      }else{
        NULL
      }
      
    })
    
    
    
    output$min_pct <- renderUI({
      
      ns <- session$ns
      
      # numericInput(ns("min"), p("Set the Minimal Fraction of Cells Expressing Genes" , actionButton(("helpBtMinPct"), "", 
      #                                                          icon = icon("question-circle"), 
      #                                                          class = "btn-xs")), 
      #              value = 0.25, min = 0, max = 1, step = 0.1)
      
      numericInput(ns("min"), p("Set the Minimal Fraction of Cells Expressing Genes"), 
                   value = 0.25, min = 0, max = 1, step = 0.1)
    })
    
    
    observeEvent(input$helpBtMinPct, {
      
      showModal(modalDialog(
        title = "Min.Pct",
        "Only test genes that are detected in a minimum fraction of min.pct cells in either of
      the two populations. Meant to speed up the function by not testing genes that are very
      infrequently expressed.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
    
    
    # 
    
    
    
    output$nb_logfc <- renderUI({
      
      ns <- session$ns
      # numericInput(ns("logfc"), p("Set the Minimal logFC of Gene Expression Difference", actionButton(("helpBtlogfc"), "", 
      #                                                 icon = icon("question-circle"), 
      #                                                 class = "btn-xs")), 
      #              value = 0.25, min = 0, step = 0.1)
      
      numericInput(ns("logfc"), p("Set the Minimal logFC of Gene Expression Difference"), 
                   value = 0.25, min = 0, step = 0.1)
      
      
    })
    
    
    observeEvent(input$helpBtlogfc, {
      
      showModal(modalDialog(
        title = "logfc threshold",
        "Limit testing to genes which show, on average, at least X-fold
      difference (log-scale) between the two groups of cells. Default is 0.25
      Increasing logfc.threshold speeds up the function, but can miss weaker signals.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
    
    
    
    
    output$checkbox_only_pos <- renderUI({
      
      ns <- session$ns
      # checkboxInput(ns("only_pos"), p("   Select only positive markers", actionButton(("helpBtOnlyPos"), "", 
      #                                                                               icon = icon("question-circle"), class = "btn-xs")), value = TRUE)
      
      checkboxInput(ns("only_pos"), p("   Select only positive markers"), value = TRUE)
    })
    
    observeEvent(input$helpBtOnlyPos, {
      
      showModal(modalDialog(
        title = "Only positive Markers",
        "Only return positive markers",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
    
    
    #############################################################################
    
    
    
    # Run dea analysis button
    output$dea_run_btn <- renderUI({
      
      ns <- session$ns
      
        actionButton(ns("dea_run_btn2"), label = "Run Analysis")

    })    
    
    
    
     ##########################################################################
    #visualization plot
    dea_vis_ploting <- eventReactive(input$dea_vis_btn2, {
      ns <- session$ns
      
      reduc <- input$sel_rd_vis # reductions


      fac_sel1 <- input$selectMetafac1
      fac_sel2 <- input$selectMetafac2
      
      para_sel1 <- input$selectLevelsFac1
      para_sel2 <- input$selectLevelsFac2

      group1 <- which(rval$seurat@meta.data[,fac_sel1] %in% para_sel1 )
      group2 <- which(rval$seurat@meta.data[,fac_sel2] %in% para_sel2 )
      common_cells <- intersect(group1, group2)
      
      
      rval$seurat@meta.data$selected_groups <- "Others"
      rval$seurat@meta.data$selected_groups[group1]<- "Group1"
      rval$seurat@meta.data$selected_groups[group2]<- "Group2"
      

      if(length(common_cells) > 0){
        rval$seurat@meta.data$selected_groups[common_cells]<- "Common"

        shinyalert("Warning!", paste("Groups with common cells have been selected. Common cells (N)=",
                                     length(common_cells), sep = ""), type = "warning")

        groups <- c("Group1", "Group2")
        sel_groups <- (unique(rval$seurat@meta.data[, "selected_groups"]))


        if(length(intersect(groups, sel_groups)) < 2){
          shinyalert("Warning!", "One of the group is a subset of other group. Please choose other datasets",
                     type = "warning")

        }else{ # Asif
          NULL
        }
    


      }
      
      # to write in file as comment
      para_st_1 <- paste(para_sel1, collapse = "-")
      para_st_2 <- paste(para_sel2, collapse = "-")
      
      # params$catch_param_vis <- paste(fac_sel1, " (Group1): ", para_st_1,"; ", fac_sel2, " (Group2): ",  
      #                                 para_st_2, sep = "")
      # 
      
      params$catch_param_vis <- paste("Group 1 (", fac_sel1, " [", para_st_1,"]) and Group 2 (", fac_sel2, " [",  
                                       para_st_2, "])", sep = "")
       
      
   
      
      vis_plot <- DimPlot(rval$seurat, reduction = reduc,
                          dims = c(1,2), group.by = 'selected_groups', pt.size =0.5, label = F)
      
      if(length(common_cells > 0)){
          output$state_vis_out <- renderText({
            aa <- paste("Common cells (will be excluded) (N): ", length(common_cells), "; Group 1 (", paste(para_sel1, collapse = ","), "): ",
                        length(group1), "; Group 2 (", paste(para_sel2, collapse = ","), "): ",length(group2))


          })
      }else{
        
           output$state_vis_out <- renderText({
             aa <- paste("Group 1 (", paste(para_sel1, collapse = ","), "): ",
                         length(group1), "; Group 2 (", paste(para_sel2, collapse = ","), "): ",length(group2))
           })

        
      }
      
      params$check <- FALSE
      
      vis_plot
      
      

    })
    
    
    output$dea_vis_plot <- renderPlot({
      dea_vis_ploting()
      
    })
    
    #export the visualization plot
    callModule(downPlotServer, id = "dea_vis_export", data = dea_vis_ploting, 
               out_file = "DEG_defined_groups")
    
    #############################################################################
    
    observeEvent(input$dea_run_btn2, {
      
      # ns <- session$ns

      updateTabsetPanel(session = session, inputId = "panel_deg",
                        selected = "Table of Differentially Expressed Genes")
      
    })
    
    
    #differentially expressed genes output
    DEA_table <- eventReactive(input$dea_run_btn2, {
      
      
      params$df_dea <- NULL
      
      if(rval$seurat@meta.data['species'] == "mouse"){
        
        params$OrgDb = "org.Mm.eg.db"
        params$organism = "mmu"
      }
     
      # factorial data
       fac_sel1 <- input$selectMetafac1
       fac_sel2 <- input$selectMetafac2
      # pick the levels of factorial data
       para_sel1 <- input$selectLevelsFac1
       para_sel2 <- input$selectLevelsFac2

      Idents(rval$seurat) <- rval$seurat@meta.data$selected_groups

      if("Group1" %in% levels(Idents(rval$seurat)) & "Group2" %in% levels(Idents(rval$seurat)) & 
         table(Idents(rval$seurat))["Group1"] > 3 & table(Idents(rval$seurat))["Group2"] > 3){
        
        
        # Create modal progress during markers finding
        show_modal_spinner(text = 'Markers finding... Please wait...', spin = 'circle')
        
        temp_df_dea <- FindMarkers(object = rval$seurat, ident.1 = "Group1",
                                   ident.2 = "Group2",
                                   group.by = "selected_groups",
                                   test.use = input$method_Markers, only.pos = input$only_pos,
                                   min.pct = input$min, logfc.threshold = input$logfc)
        
        # to write in file as comment
        # 
        remove_modal_spinner()
        
        
      }else{
        
        showModal(modalDialog(
          "Re-run the differential expression analysis for newly defined groups",
          size = "s",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        
      }

      if(nrow(temp_df_dea) <= 0){
        
        showModal(modalDialog(
          "No features pass logfc threshold",
          size = "s",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        
      }else{
      
            params$catch_param_dea <- paste("Chosen Statistical Method: ", input$method_Markers, "; If only positive genes were chosen: ", 
                             input$only_pos, "; Minimal Fraction of Cells Expressing Genes: ", input$min, "; Minimal logFC: "
                             , input$logfc, sep = "")

      # Calculate diff percentage for each marker
      temp_df_dea$diff.pct = (temp_df_dea$pct.1)-(temp_df_dea$pct.2)
      
      # # Reorganize dataframe of markers
      temp_df_dea = temp_df_dea[,c(5,1,2,3,4,6)]
      
      
      # Remove p_val et round p_val adjust with 2 decimals and scientific format
      temp_df_dea$p_val= as.numeric(scientific(temp_df_dea$p_val, digits = 3))
      temp_df_dea$p_val_adj=as.numeric(scientific(temp_df_dea$p_val_adj, digits = 3))  
      
      # Round other columns
      temp_df_dea[,3:6]=round(temp_df_dea[,3:6],3)

      #to display the table stat
      params$check <- TRUE
      params$df_dea <- temp_df_dea
      }
      
    })


    output$DEA_genes <- function() {
      
      if(!is.null(DEA_table())){
        if(nrow(DEA_table()) > 200){
          kable(DEA_table()[1:200,], "html", caption = "Only top 200 genes display in table. Download table if you want to see all genes.") %>%
            kable_styling(bootstrap_options = c("striped", "hover")) %>%
            scroll_box(width = "100%", height = "450px")
        }else{
          kable(DEA_table(), "html") %>%
            kable_styling(bootstrap_options = c("striped", "hover")) %>%
            scroll_box(width = "100%", height = "450px")
        }
      }
    }
    
    
    # output$DEA_genes <- renderDataTable({
    #   if(nrow(DEA_table()) > 200){
    #     datatable(DEA_table()[1:200,],  caption = "Only top 200 genes display in table. Download table if you want to see all genes.")
    #   }else{
    #     DEA_table()
    #   }
    # })



    #to show the tables statics text in DEA table (supportive text)
    observeEvent(input$dea_run_btn2, {
      
      output$dea_table_stat <- renderUI({
        if(params$check){
          
          commt_val <- paste("Differentially Expressed Genes for ", 
                             params$catch_param_vis, " ; "
                             , params$catch_param_dea, sep = "")
          params$comment_out <- commt_val
          
          #write the full table to file
          callModule(downServer, id = "DEA_genes_export", data = DEA_table, out_file = "DEA_genes_",
                     comm = params$comment_out)
          
          
          # params$comment_out <- paste("Differentially expressed genes for ", 
          #                             params$catch_param_vis, sep = "")
         # bb <- params$comment_out
          str1 <- paste("Differentially expressed genes for ", 
                        params$catch_param_vis, sep = "")
          str2 <- params$catch_param_dea
          HTML(paste(str1, str2, sep = '<br/><br/>'))
          
        }else{
          "Re-run the differential expression analysis for newly defined groups"
        }
        
      })
      
      # #print the parametres used
      # output$dea_table_stat <- renderText({
      #   if(params$check){
      #   params$comment_out <- params$catch_param_dea
      #   cc <- params$comment_out
      # }else{
      #   "Re-run the differential expression analysis for newly defind groups"
      # }
      # })
      # 
      
      
    })
    

    #plotting teh feature plot
    DEA_featurePlot <- eventReactive(params$df_dea, {

      df <- params$df_dea
      df <- df[order(as.numeric(df$p_val_adj), abs(df$avg_log2FC), decreasing = c(F,T)),]
      genes <- rownames(df)
      dims = c(1,2)
      

      #############################################################
      if(length(genes) > 10){
        FeaturePlot(rval$seurat, reduction = input$sel_rd_vis, dims = dims, features = genes[1:9], ncol = 3,
                    pt.size = 0.5) 
      }else if(length(genes) == 0){
        NULL
      }else{
        FeaturePlot(rval$seurat, reduction = input$sel_rd_vis, dims = dims, features = genes, ncol = 3
                    , pt.size = 0.5) 
      }

    })


    output$TopDEA <- renderPlot({
      DEA_featurePlot()

    })
    
    
    
    GO_BP_Plot <- eventReactive(params$df_dea, {
      
      df <- params$df_dea
      df <- df[order(as.numeric(df$p_val_adj), abs(df$avg_log2FC), decreasing = c(F,T)),]
      genes <- rownames(df)

      eg = bitr(genes, fromType="SYMBOL", toType="ENTREZID", OrgDb= params$OrgDb,  drop = FALSE)
      
      # remove potential ENTREZ IDs duplicated
      eg=eg[!duplicated(eg["SYMBOL"]),]
      eg=na.omit(eg)
      
      if(length(eg) > 0){
        
        show_modal_spinner(text = 'Enrichment processing... Please wait...', spin = 'circle')
        
        
        # perform GO enrichment (Biological Process)
        ego_BP <- enrichGO(gene          = eg[,2],
                           OrgDb         = params$OrgDb,
                           ont           = "BP",
                           pAdjustMethod = "BH",
                           pvalueCutoff  = 0.05,
                           qvalueCutoff  = 0.05,
                           readable      = TRUE)
        
        
        remove_modal_spinner()
        
      }
      
      
      if(!is.null(ego_BP)){
        if(nrow(ego_BP@result) > 0){
          params$ego_BP <- ego_BP@result
          dotplot(ego_BP, showCategory=30)
        }else{
          showModal(modalDialog(
            "No significant result",
            size = "s",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
        }
      }else{
        params$ego_BP <- NULL
        showModal(modalDialog(
          "No significant result",
          size = "s",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }

    })
    
    
    output$GO_BP <- renderPlot({
      GO_BP_Plot()
      
    })
    
    
    output$download_GO_BP <- downloadHandler(
      
      filename = function() {
        paste("GO_Biological_Process_Table", Sys.Date(), ".csv", sep="")
      },
      
      content = function(file) {
        #####################
        
        con <- file(file, open="wt")#t
        #writeLines(paste("#", comm), con)
        writeLines(paste("#", "Results were obtained from ShiVA (URL)"), con)
        
        ###########
        # print(dim(data()))
        
        write.csv(params$ego_BP, con) # add parentheses to data arg if reactive
        close(con)
      }
    )
    
    
    
    
    
    GO_MF_Plot <- eventReactive(params$df_dea, {
      
      df <- params$df_dea
      df <- df[order(as.numeric(df$p_val_adj), abs(df$avg_log2FC), decreasing = c(F,T)),]
      genes <- rownames(df)
      
      eg = bitr(genes, fromType="SYMBOL", toType="ENTREZID", OrgDb= params$OrgDb,  drop = FALSE)
      
      # remove potential ENTREZ IDs duplicated
      eg=eg[!duplicated(eg["SYMBOL"]),]
      eg=na.omit(eg)
      
      if(length(eg) > 0){
        
        show_modal_spinner(text = 'Enrichment processing... Please wait...', spin = 'circle')
        
        
        # perform GO enrichment (Molecular Function)
        ego_MF <- enrichGO(gene          = eg[,2],
                           OrgDb         = params$OrgDb,
                           ont           = "MF",
                           pAdjustMethod = "BH",
                           pvalueCutoff  = 0.05,
                           qvalueCutoff  = 0.05,
                           readable      = TRUE)
        
        
        remove_modal_spinner()
        
      }
      
      
      if(!is.null(ego_MF)){
        if(nrow(ego_MF@result) > 0){
          params$ego_MF <- ego_MF@result
          dotplot(ego_MF, showCategory=30)
        }else{
          showModal(modalDialog(
            "No significant result",
            size = "s",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
        }
      }else{
        params$ego_MF <- NULL
        showModal(modalDialog(
          "No significant result",
          size = "s",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
      
      
    })
    
    
    output$GO_MF <- renderPlot({
      GO_MF_Plot()
      
    })
    
    
    
    output$download_GO_MF <- downloadHandler(
      
      filename = function() {
        paste("GO_Molecular_Function_Table", Sys.Date(), ".csv", sep="")
      },
      
      content = function(file) {
        #####################
        
        con <- file(file, open="wt")#t
        #writeLines(paste("#", comm), con)
        writeLines(paste("#", "Results were obtained from ShiVA (URL)"), con)
        
        ###########
        # print(dim(data()))
        
        write.csv(params$ego_MF, con) # add parentheses to data arg if reactive
        close(con)
      }
    )
    
    
    
    GO_CC_Plot <- eventReactive(params$df_dea, {
      
      df <- params$df_dea
      df <- df[order(as.numeric(df$p_val_adj), abs(df$avg_log2FC), decreasing = c(F,T)),]
      genes <- rownames(df)
      
      eg = bitr(genes, fromType="SYMBOL", toType="ENTREZID", OrgDb= params$OrgDb,  drop = FALSE)
      
      # remove potential ENTREZ IDs duplicated
      eg=eg[!duplicated(eg["SYMBOL"]),]
      eg=na.omit(eg)
      
      if(length(eg) > 0){
        
        show_modal_spinner(text = 'Enrichment processing... Please wait...', spin = 'circle')
        
        
        # perform GO enrichment (Cellular component)
        ego_CC <- enrichGO(gene          = eg[,2],
                           OrgDb         = params$OrgDb,
                           ont           = "CC",
                           pAdjustMethod = "BH",
                           pvalueCutoff  = 0.05,
                           qvalueCutoff  = 0.05,
                           readable      = TRUE)
        
        
        remove_modal_spinner()
        
      }
      
      
      if(!is.null(ego_CC)){
        if(nrow(ego_CC@result) > 0){
          params$ego_CC <- ego_CC@result
          dotplot(ego_CC, showCategory=30)
        }else{
          showModal(modalDialog(
            "No significant result",
            size = "s",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
        }
      }else{
        params$ego_CC <- NULL
        showModal(modalDialog(
          "No significant result",
          size = "s",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
      

    })
    
    
    output$GO_CC <- renderPlot({
      GO_CC_Plot()
      
    })
    
    
    output$download_GO_CC <- downloadHandler(
      
      filename = function() {
        paste("GO_Cellular_Component_Table", Sys.Date(), ".csv", sep="")
      },
      
      content = function(file) {
        #####################
        
        con <- file(file, open="wt")#t
        #writeLines(paste("#", comm), con)
        writeLines(paste("#", "Results were obtained from ShiVA (URL)"), con)
        
        ###########
        # print(dim(data()))
        
        write.csv(params$ego_CC, con) # add parentheses to data arg if reactive
        close(con)
      }
    )
    
    
    
    KEGG_Plot <- eventReactive(params$df_dea, {
      
      df <- params$df_dea
      df <- df[order(as.numeric(df$p_val_adj), abs(df$avg_log2FC), decreasing = c(F,T)),]
      genes <- rownames(df)
      
      eg = bitr(genes, fromType="SYMBOL", toType="ENTREZID", OrgDb= params$OrgDb,  drop = FALSE)
      
      # remove potential ENTREZ IDs duplicated
      eg=eg[!duplicated(eg["SYMBOL"]),]
      eg=na.omit(eg)
      
      if(length(eg) > 0){
        
        show_modal_spinner(text = 'Enrichment processing... Please wait...', spin = 'circle')
        
        
        # perform GO enrichment (Cellular component)
        KEGG <-  enrichKEGG(gene         = eg[,2],
                            organism     = params$organism,
                            pvalueCutoff = 0.05)
        
        
        remove_modal_spinner()
        
        
      }

      if(!is.null(KEGG)){
        if(nrow(KEGG@result) > 0){
          params$KEGG <- KEGG@result
          dotplot(KEGG, showCategory=30)
        }else{
          showModal(modalDialog(
            "No significant result",
            size = "s",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
        }
      }else{
        params$KEGG <- NULL
        showModal(modalDialog(
          "No significant result",
          size = "s",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }

      
    })
    
    
    output$KEGG <- renderPlot({
      KEGG_Plot()
      
    })
    
    
    output$download_KEGG <- downloadHandler(
      
      filename = function() {
        paste("KEGG_Table", Sys.Date(), ".csv", sep="")
      },
      
      content = function(file) {
        #####################
        
        con <- file(file, open="wt")#t
        #writeLines(paste("#", comm), con)
        writeLines(paste("#", "Results were obtained from ShiVA (URL)"), con)
        
        ###########
        # print(dim(data()))
        
        write.csv(params$KEGG, con) # add parentheses to data arg if reactive
        close(con)
      }
    )
    
    
    
  
    #export the feature plot
    callModule(downPlotServer, id = "fea_plot_export", data = DEA_featurePlot, 
               out_file = "top_differentially_expressed_genes")
    
    
    #export the feature plot
    callModule(downPlotServer, id = "fea_GO_BP_export", data = GO_BP_Plot, 
               out_file = "GO_Biological_Process_plot")
    
    callModule(downPlotServer, id = "fea_GO_MF_export", data = GO_MF_Plot, 
               out_file = "GO_Molecular_Function_plot")
    
    callModule(downPlotServer, id = "fea_GO_CC_export", data = GO_CC_Plot, 
               out_file = "GO_Cellular_Component_plot")
    
    callModule(downPlotServer, id = "fea_KEGG_export", data = KEGG_Plot, 
               out_file = "KEGG_plot")
    
    
    #to show the tables statisc text in feature plot (sipportive text)
    observeEvent(input$dea_run_btn2, {
      
      output$fea_plot_stat <- renderText({
        if(params$check){
          bb <- paste("Differentially expressed genes for ", params$catch_param_vis,
                      "; ", params$catch_param_dea, sep = "")
         
          }else{
          "Re-run the differential expression analysis for newly defind groups"
        }
        
      })
      
      
    })
  
   
    
  
  return(rval)
}



