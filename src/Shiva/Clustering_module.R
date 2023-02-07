utils::globalVariables("GvHD")

#' Import data and build a seurat
#' @param id shiny id
#' @importFrom shinydashboard box
#' @importFrom DT DTOutput
#' @import shiny
#' @import Seurat
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' library(shiny)
#' library(shinydashboard)
#' Import module server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval a reactivevalues object (can be empty)
#' @param seurat
#' @return The input reactivevalues object 'rval' with updated elements :
#' @importFrom DT renderDT
#' @importFrom tools file_ext
#' @importFrom utils read.table data
#' @export
#' @rdname ClusteringUI
#' 
#' 
library(scales) 

ClusteringUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    
    conditionalPanel(condition = "output.check_pca", ns = ns,
                     box(title = p("PCA to Use", actionButton(ns("HELP_CLUSTERING_PCA"), "", icon = icon("question-circle"), class = "btn-xs")),
                         width = 6, height = NULL, solidHeader = TRUE, 
                         status = "primary", collapsible = T,
                         uiOutput(ns("select_rd")),
                         plotOutput(ns("ScreePlot"), height = 190) %>% withSpinner(color="#0dc5c1")
                     ),
                     
                     box(title = p("Parameters for Clustering", actionButton(ns("HELP_CLUSTERING_PARAMS"), "", icon = icon("question-circle"), class = "btn-xs")),
                         width = 3, height = NULL, solidHeader = TRUE,
                         status = "primary", collapsible = T,
                         uiOutput(ns("nbrDimension")),
                         uiOutput(ns("select_method_Clustering")),
                         uiOutput(ns("choose_resolution")),
                         actionButton(ns("run_clustering"), label = "Run Clustering")
                         
                     ),
                     
                     conditionalPanel(condition = "output.check_clustering", ns = ns,
                                      box(title = p("Parameters for Marker Genes Analysis", actionButton(ns("HELP_MARKERS_PARAMS"), "", icon = icon("question-circle"), class = "btn-xs")),
                                          width = 3, height = NULL, solidHeader = TRUE,
                                          status = "primary", collapsible = T,
                                          uiOutput(ns("min_pct")),
                                          uiOutput(ns("nb_logfc")),
                                          uiOutput(ns("select_method_Markers")),
                                          uiOutput(ns("checkbox_only_pos")),
                                          actionButton(ns("run_findAllMarkers"), label = "Run Marker Genes Identification")
                                      )  
                     ),
                     
                     
                     conditionalPanel(condition = "output.check_clustering", ns = ns,
                                      box(title = p("Clusters Visualization",actionButton(ns("HELP_CLUSTERING_PLOT"), "", icon = icon("question-circle"), class = "btn-xs")),
                                          width = 5,
                                          solidHeader = TRUE, status = "primary", 
                                          uiOutput(ns("select_all_rd")),
                                          plotOutput(ns("plotclustering")) %>% withSpinner(color="#0dc5c1"),
                                          column(6,uiOutput(ns("select_PC1"))),
                                          column(6,uiOutput(ns("select_PC2"))),
                                          downPlotUI(ns("clustering_plot"))

                                      )
                     ),
                     
                     conditionalPanel(condition = "output.check_FindAllMarkers", ns = ns,
                                      box(title = p("Marker Genes Visualization", actionButton(ns("HELP_MARKERS_RESULTS"), "", icon = icon("question-circle"), class = "btn-xs")),
                                          height = NULL, solidHeader = TRUE, 
                                          status = "primary", width = 7,
                                          tabsetPanel(
                                            tabPanel("Table of All Marker Genes",
                                                     br(),
                                                     br(),
                                                     # div(style = 'overflow-x: scroll', dataTableOutput(ns("tablemarkers"))%>% 
                                                     div(style = '', tableOutput(ns("tablemarkers"))%>% 
                                                     withSpinner(color="#0dc5c1")),
                                                     br(),
                                                     downUI(id = ns("markerGenesAll"))
                                            ),
                                            tabPanel("Feature Plots of Top Gene Markers by Cluster",
                                                     uiOutput(ns("select_cluster")),
                                                     plotOutput(ns("mark_by_clust"), height = 800) %>% withSpinner(color="#0dc5c1"),
                                                     br(),
                                                     downPlotUI(ns("clusPlotDown"))
                                            ),
                                            tabPanel("Table of Marker Genes by Cluster", 
                                                     uiOutput(ns("select_cluster2")),
                                                     # div(style = 'overflow-x: scroll', dataTableOutput(ns("tablemarkers2"))%>% 
                                                     div(style = '', tableOutput(ns("tablemarkers2"))%>% 
                                                     withSpinner(color="#0dc5c1")),
                                                     br(),
                                                     #@Asif
                                                     downUI(id = ns("markerGenes"))
                                                     
                                            )
                                          )
                                      )
                     )
                     
                     
    )
  )
}




Clustering <- function(input, output, session, rval) {
  
  # Create variable for mpdule
  params <- reactiveValues(cluster_name = NULL,
                           df_name = NULL,
                           current_seu_name = NULL,
                           method_clustering = NULL,
                           nbrDimension = NULL,
                           rd = NULL,
                           resolution = NULL,
                           method_Markers = NULL, 
                           only_pos = NULL,
                           min = NULL,
                           logfc = NULL)
  
  
  
  
  ######### Help buttons ############
  
  
  observeEvent(input[["HELP_CLUSTERING_PCA"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_CLUSTERING_PCA"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_CLUSTERING_PCA"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_CLUSTERING_PARAMS"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_CLUSTERING_PARAMS"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_CLUSTERING_PARAMS"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  observeEvent(input[["HELP_MARKERS_PARAMS"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_MARKERS_PARAMS"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_MARKERS_PARAMS"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  observeEvent(input[["HELP_CLUSTERING_PLOT"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_CLUSTERING_PLOT"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_CLUSTERING_PLOT"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  observeEvent(input[["HELP_MARKERS_RESULTS"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_MARKERS_RESULTS"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_MARKERS_RESULTS"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  
  
  
  
  output$select_rd<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      reduc <- names(rval$parameters[[rval$seurat_selected]])[str_detect(tolower(names(rval$parameters[[rval$seurat_selected]])), "^pca")]
      
      if(length(reduc) > 0){
        selectInput(ns("rd"), label = "Choose PCA to use",
                    choices = reduc, width = "30%")
      }else{
        params$cluster_name = NULL
      }
      
    }else{
      NULL
    }
    
  })
  
  
  output$select_all_rd<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      reduc <- Reductions(rval$seurat)
      
      if(length(reduc) > 0){
        selectInput(ns("all_rd"), label = "Choose Embedding to Display",
                    choices = reduc, selected = tail(reduc,1), width = "50%")
      }else{
        params$cluster_name = NULL
      }
      
    }else{
      NULL
    }
    
  })
  
  
  output$nbrDimension<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$rd)){
      numericInput(ns("nbrDimension"), label="Set the Number of PC to Use", value = 10, min = 0, max = length(nbPCACol()), step = 1)
    }else{
      NULL
    }
    
  })
  
  
  output$select_method_Clustering <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      selectInput(ns("method_clustering"), label = "Choose Method to Identify Clusters", 
                  choices = list("Louvain" = "Louvain", "SLM" = "SLM", "Leiden" = "Leiden"), 
                  selected = "Louvain")
    }else{
      NULL
    }
    
  })
  
  
  
  
  observeEvent(input$helpBtResolution, {
    
    showModal(modalDialog(
      title = "Resolution",
      "Value of the resolution parameter, use a value above (below) 1.0 
      if you want to obtain a larger (smaller) number of communities.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
  
  
  
  output$choose_resolution <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$rd)){
      numericInput(ns("resolution"), p("Set the Resolution Value", actionButton(ns("helpBtResolution"), "", icon = icon("question-circle"), class = "btn-xs")), value = 0.8, min = 0, max = 2, step = 0.1)
    }else{
      NULL
    }
    
  })
  
  
  
  observeEvent(input$run_clustering, {
    
    if(input$nbrDimension < 1 || input$nbrDimension > length(nbPCACol()) || is.na(input$nbrDimension)){
      
      showModal(modalDialog(
        paste0("The number of PCs must be between 1 and ",length(nbPCACol())),
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    }
    
    
    
    if(input$resolution < 0 || input$resolution > 2 || is.na(input$resolution)){
      
      showModal(modalDialog(
        "The resolution must be between 0 and 2",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    }
    
    
    
    
    
    validate(
      need(!is.na(input$nbrDimension), ''),
      need(input$nbrDimension >= 1, ''),
      need(input$nbrDimension <= length(nbPCACol()), ''),
      need(!is.na(input$resolution), ''),
      need(input$resolution >= 0, ''),
      need(input$resolution <= 2, '')
    )
    
    
    # Create modal progress during clustering
    show_modal_spinner(text = 'Clustering... Please wait...', spin = 'circle')
    
    rval$seurat <- FindNeighbors(rval$seurat, reduction = input$rd, assay = rval$norm_assay, dims = 1:input$nbrDimension)
    
    if (input$method_clustering == "Louvain"){
      
      rval$seurat <- FindClusters(rval$seurat, resolution = input$resolution, algorithm = 1)
      
    } else if (input$method_clustering == "SLM"){
      
      rval$seurat <- FindClusters(rval$seurat, resolution = input$resolution, algorithm = 3)
      
    } else if (input$method_clustering == "Leiden") {
      
      # NULL
      rval$seurat <- FindClusters(rval$seurat, resolution = input$resolution, algorithm = 4, method = "igraph")
    }
    
    
    name <- paste(rval$norm_assay,"clust",input$rd,input$nbrDimension,input$method_clustering ,input$resolution,sep = "_")
    
    if("seurat_clusters" %in% colnames(rval$seurat@meta.data)){
      
      rval$seurat[["seurat_clusters"]] <- NULL
    }
    
    
    if(name %in% colnames(rval$seurat@meta.data)){
      
      rval$seurat[[name]] <- NULL
    }
    
    
    if(paste0(rval$norm_assay,"_snn_res.",input$resolution) %in% colnames(rval$seurat@meta.data)){
      
      rval$seurat@meta.data[paste0(rval$norm_assay,"_snn_res.",input$resolution)] <- NULL
      
    }else{
      
      rval$seurat@meta.data[ncol(rval$seurat@meta.data)] <- NULL
    }
    
    
    rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "cluster")
    rval$parameters[[rval$seurat_selected]][[name]] = c("pca" = input$rd, "npcs" = input$nbrDimension, "method" = input$method_clustering, "res" = input$resolution)
    
    
    
    rval$seurat[[name]] <- Idents(rval$seurat)
    
    # names(rval$seurat@meta.data)[ncol(rval$seurat@meta.data)] <- paste(rval$norm_assay,input$rd,input$nbrDimension,input$method_clustering ,input$resolution,sep = "_")
    
    params$cluster_name <- name
    params$df_name <- NULL
    params$current_seu_name <- rval$seurat_selected
    params$method_clustering <- input$method_clustering
    params$nbrDimension <- input$nbrDimension
    params$rd <- input$rd
    params$resolution <- input$resolution
    
    saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))
    
    # Close modal progress when clustering is done
    remove_modal_spinner()
    
  })
  
 clus_plot <- reactive({

   if(!is.null(params$cluster_name) & !is.null(input$all_rd)){
     #Asif
     rval$parameters[[rval$seurat_selected]]["sel_dim_plot"] <- input$all_rd
     
      
      if(grepl("^pca", input$all_rd, ignore.case = T) == TRUE){
        if(!is.null(input$axisPC1) & !is.null(input$axisPC2)){
          DimPlot(rval$seurat, reduction = input$all_rd, dims = c(as.numeric(input$axisPC1),as.numeric(input$axisPC2)), group.by = params$cluster_name, pt.size =0.5, label = TRUE)
        }
      }else{
        DimPlot(rval$seurat, reduction = input$all_rd, group.by = params$cluster_name, pt.size =0.5, label = TRUE)
      }
      
    }


  })

output$plotclustering <- renderPlot(
  clus_plot()

   
  )

## caliing the module
  callModule(downPlotServer, id = "clustering_plot", data = clus_plot, 
    out_file = "clustering_plot")

  
  
  
  nbPCA <- reactive({
    
    if(!is.null(input$all_rd)){
      
      return(ncol(Embeddings(rval$seurat, reduction = input$all_rd)))
      
    }
    
  })
  
  
  
  output$select_PC1<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$all_rd)){
      
      if(grepl("^pca", input$all_rd, ignore.case = T) == TRUE){
        choicesPCA <- 1:nbPCA()
        
        selectInput(ns("axisPC1"), "PC in x",
                    choicesPCA, selected = choicesPCA[1])
      }else{
        NULL
      }
      
      
    }else{
      NULL
    }
    
  })
  
  
  output$select_PC2<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$all_rd)){
      
      if(grepl("^pca", input$all_rd, ignore.case = T) == TRUE){
        choicesPCA <- 1:nbPCA()
        
        selectInput(ns("axisPC2"), "PC in y",
                    choicesPCA, selected = choicesPCA[2])
      }else{
        NULL
      }
      
      
    }else{
      NULL
    }
    
  })
  
  
  
  nbPCACol <- reactive({
    
    if(!is.null(input$rd)){
      return(colnames(Embeddings(rval$seurat, reduction = input$rd)))
    }
    
  })
  
pce_elbow <- reactive({

  if(!is.null(input$rd)){
      if(length(nbPCACol()) >= 50){
        ndims = 50
      }else{
        ndims = length(nbPCACol())
      }
      
      ElbowPlot(rval$seurat, ndims = ndims, reduction = input$rd)
    }



  })

output$ScreePlot <- renderPlot({

  pce_elbow()
    
    
  })
  
  
  
  
  
  
  ##########################   Markers #########################
  
  
  output$select_method_Markers <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      selectInput(ns("method_Markers"), label = "Choose Method to Identify Marker Genes", 
                  choices = list("wilcox" = "wilcox", "bimod" = "bimod", "t" = "t", "negbinom" = "negbinom", "poisson" = "poisson", "LR" = "LR"), 
                  selected = "wilcox")
    }else{
      NULL
    }
    
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
  
  
  output$min_pct <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$rd)){
      numericInput(ns("min"), p("Set the Minimal Fraction of Cells Expressing Marker Genes" , actionButton(ns("helpBtMinPct"), "", icon = icon("question-circle"), class = "btn-xs")), value = 0.25, min = 0, max = 1, step = 0.1)
    }else{
      NULL
    }
    
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
  
  output$nb_logfc <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$rd)){
      numericInput(ns("logfc"), p("Set the Minimal logFC of Gene Expression Difference", actionButton(ns("helpBtlogfc"), "", icon = icon("question-circle"), class = "btn-xs")), value = 0.25, min = 0, step = 0.1)
    }else{
      NULL
    }
    
  })
  
  
  
  
  observeEvent(input$helpBtOnlyPos, {
    
    showModal(modalDialog(
      title = "Only positive Markers",
      "Only return positive markers",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
  
  
  
  output$checkbox_only_pos <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      checkboxInput(ns("only_pos"), p("   Select Only Positive Markers", actionButton(ns("helpBtOnlyPos"), "", icon = icon("question-circle"), class = "btn-xs")), value = TRUE)
      
    }else{
      NULL
    }
  })
  
  
  
  
  
  observeEvent(input$run_findAllMarkers, {
    
    if(input$min < 0 || input$min > 1|| is.na(input$min)){
      
      showModal(modalDialog(
        "The min.pct must be between 0 and 1",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    }
    
    
    
    if(input$logfc < 0 || is.na(input$logfc)){
      
      showModal(modalDialog(
        "The logfc must be higher than 0",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    }
    
    
    
    validate(
      need(!is.na(input$min), ''),
      need(input$min >= 0, ''),
      need(input$min <= 1, ''),
      need(!is.na(input$logfc), ''),
      need(input$logfc >= 0, '')
      # need(input$logfc <= 1, '')
    )
    
    # Create modal progress during markers finding
    show_modal_spinner(text = 'Marker Genes Identification... Please Wait...', spin = 'circle')
    
    Idents(rval$seurat) <- params$cluster_name
    
    
    if (input$method_Markers == "wilcox"){
      
      df.markers <- FindAllMarkers(rval$seurat, assay = rval$norm_assay, test.use = input$method_Markers, only.pos = input$only_pos, min.pct = input$min, logfc.threshold = input$logfc)
      
      
    } else if (input$method_Markers == "bimod"){
      
      df.markers <- FindAllMarkers(rval$seurat, assay = rval$norm_assay, test.use = input$method_Markers, only.pos = input$only_pos, min.pct = input$min, logfc.threshold = input$logfc)
      
      
    } else if (input$method_Markers == "t"){
      
      df.markers <- FindAllMarkers(rval$seurat, assay = rval$norm_assay, test.use = input$method_Markers, only.pos = input$only_pos, min.pct = input$min, logfc.threshold = input$logfc)
      
      
    } else if (input$method_Markers == "negbinom"){
      
      df.markers <- FindAllMarkers(rval$seurat, assay = rval$norm_assay, test.use = input$method_Markers, only.pos = input$only_pos, min.pct = input$min, logfc.threshold = input$logfc)
      
      
    } else if (input$method_Markers == "poisson"){
      
      df.markers <- FindAllMarkers(rval$seurat, assay = rval$norm_assay, test.use = input$method_Markers, only.pos = input$only_pos, min.pct = input$min, logfc.threshold = input$logfc)
      
      
      
    } else {
      
      df.markers <- FindAllMarkers(rval$seurat, assay = rval$norm_assay, test.use = input$method_Markers, only.pos = input$only_pos, min.pct = input$min, logfc.threshold = input$logfc)
      
    }
    # # # Reorganize dataframe of markers
    # df.markers = df.markers[,c(7,1,5,2,3,4,6)]
    
    # Close modal progress when markers finded
    remove_modal_spinner()
    
    if(nrow(df.markers) <= 0){
      
      showModal(modalDialog(
        "No features pass logfc threshold",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    }else{
      
      # Calculate diff percentage for each marker
      df.markers$diff.pct = (df.markers$pct.1)-(df.markers$pct.2)
      
      # # Reorganize dataframe of markers
      df.markers = df.markers[,c(7,5,2,3,4,8,6)]
      
      
      # Remove p_val et round p_val adjust with 2 decimals and scientific format
      df.markers$p_val=NULL
      df.markers$p_val_adj=scientific(df.markers$p_val_adj, digits = 3)
      
      # Round other columns
      df.markers[,3:6]=round(df.markers[,3:6],2)
      
      # Conserve only with signif p.val_adj
      df.markers <- df.markers[which(as.numeric(df.markers$p_val_adj)<0.05),]
      
      params$df_name <- df.markers 
      params$method_Markers <- input$method_Markers
      params$min <- input$min
      params$logfc <- input$logfc
      params$only_pos <- input$only_pos
      
      rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "markers")
      
    }
    
    
  })
  
  
    #@Asif
  ############################################################

output$select_cluster2<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(params$df_name)){
      
      clust2 <- levels(params$df_name$cluster)
      
      selectInput(ns("clusters2"), label = "Choose Cluster",
                  choices = clust2, width = "25%")
      
    }else{
      NULL
    }
    
  })

tablemarkers2_ploting <- reactive({

     if(!is.null(params$df_name[,"cluster"])){

      df <- params$df_name[which(params$df_name$cluster %in% input$clusters2 & params$df_name$avg_log2FC > 0),]

      df <- df[order(as.numeric(df$p_val_adj), abs(df$avg_log2FC), decreasing = c(F,T)),]

      if(grepl("^pca", input$all_rd, ignore.case = T) == TRUE){
        dims = c(as.numeric(input$axisPC1),as.numeric(input$axisPC2))
      }else{
        dims = c(1,2)
      }

      return(df)


      }
})



output$tablemarkers2 <- function() {
  
  if(nrow(tablemarkers2_ploting()) > 30){
    kable(tablemarkers2_ploting()[1:30,], "html", row.names = F,caption = "Only top 30 genes display in table. Download table if you want to see all genes.") %>%
      kable_styling(bootstrap_options = c("striped", "hover")) %>%
      scroll_box(width = "100%", height = "450px")
  }else{
    kable(tablemarkers2_ploting(), "html", row.names = F) %>%
      kable_styling(bootstrap_options = c("striped", "hover")) %>%
      scroll_box(width = "100%", height = "450px")
  }

}


# output$tablemarkers2 <- renderDataTable({
#   if(nrow(tablemarkers2_ploting()) > 30){
#     datatable(tablemarkers2_ploting()[1:30,],  caption = "Only top 30 genes display in table. Download table if you want to see all genes.")
#   }else{
#     tablemarkers2_ploting()
#   }
# })


#write the table to file
callModule(downServer, id = "markerGenes", data = tablemarkers2_ploting, out_file = "markerGenes_")


#to download all the genes
# genesAll <- reactive({
#   df_genesAll <- params$df_name
# 
# 
# })


  
  ##### @ASIF

feature_plot <- reactive({
   if(!is.null(params$df_name)){
      
      df <- params$df_name[which(params$df_name$cluster %in% input$clusters & params$df_name$avg_log2FC > 0),]
      df <- df[order(as.numeric(df$p_val_adj), abs(df$avg_log2FC), decreasing = c(F,T)),]
      genes <- df$gene
      
      if(grepl("^pca", input$all_rd, ignore.case = T) == TRUE){
        dims = c(as.numeric(input$axisPC1),as.numeric(input$axisPC2))
      }else{
        dims = c(1,2)
      }
      
      
      if(length(genes) > 12){
        FeaturePlot(rval$seurat, reduction = input$all_rd, dims = dims, features = genes[1:12], ncol = 3)
      }else if(length(genes) == 0){
        NULL
      }else{
        FeaturePlot(rval$seurat, reduction = input$all_rd, dims = dims, features = genes, ncol = 3)
      }
      
      
    }

  })
output$mark_by_clust <- renderPlot(
  feature_plot()
   
  )

## caliing the module
  callModule(downPlotServer, id = "clusPlotDown", data = feature_plot, out_file = paste("FeaturePlot",input$clusters, sep="_"))

  
  tablemarkers_ploting <- reactive({
    if(!is.null(params$df_name[,"cluster"])){
      
      df <- params$df_name
      
      df <- df[order(as.numeric(df$p_val_adj), abs(df$avg_log2FC), decreasing = c(F,T)),]
      
      return(df)
      
    }
    
  })

      
      # datatable(params$df_name, rowname = FALSE, caption = "Markers by cluster", 
      #           filter = "top",extensions = 'Buttons', 
      #           options = list(dom = 'Blfrtip', 
      #                          buttons = list(list(extend = 'csv', filename= 'DEG_by_cluster'), 
      #                                         list(extend = 'excel', filename= 'DEG_by_cluster')),
      #                          pageLength = 10, lengthMenu = list(c(10,20,50,100,-1), 
      #                                                             c('10', '20', '50', '100', 'All')))) %>% 
      #   formatStyle('cluster',color = "black",backgroundColor = styleEqual(levels(params$df_name[,"cluster"]), hue_pal()(length(levels(params$df_name[,"cluster"])))))%>%
      #   formatStyle('avg_log2FC',color = "black", backgroundColor = styleInterval(0, c('red', 'green')))
    
  
  
  
  output$tablemarkers <- function() {
    
    if(nrow(tablemarkers_ploting()) > 200){
      kable(tablemarkers_ploting()[1:200,], "html", row.names = F, caption = "Only top 200 genes display in table. Download table if you want to see all genes.") %>%
        kable_styling(bootstrap_options = c("striped", "hover")) %>%
        scroll_box(width = "100%", height = "500px")
    }else{
      kable(tablemarkers_ploting(), "html", row.names = F) %>%
        kable_styling(bootstrap_options = c("striped", "hover")) %>%
        scroll_box(width = "100%", height = "500px")
    }
    
  }
  
  
  
  # output$tablemarkers <- renderDataTable({
  #   if(nrow(tablemarkers_ploting()) > 200){
  #     datatable(tablemarkers_ploting()[1:200,],  caption = "Only top 200 genes display in table. Download table if you want to see all genes.")
  #   }else{
  #     tablemarkers_ploting()
  #   }
  # })
  # 
  
  
  
  #write the full table to file
  callModule(downServer, id = "markerGenesAll", data = tablemarkers_ploting, out_file = "markerGenesAll_")
  
  
  output$mark_by_clust <- renderPlot(
    if(!is.null(params$df_name)){
      
      df <- params$df_name[which(params$df_name$cluster %in% input$clusters & params$df_name$avg_log2FC > 0),]
      df <- df[order(as.numeric(df$p_val_adj), abs(df$avg_log2FC), decreasing = c(F,T)),]
      genes <- df$gene
      
      if(grepl("^pca", input$all_rd, ignore.case = T) == TRUE){
        dims = c(as.numeric(input$axisPC1),as.numeric(input$axisPC2))
      }else{
        dims = c(1,2)
      }
      
      
      if(length(genes) > 12){
        FeaturePlot(rval$seurat, reduction = input$all_rd, dims = dims, features = genes[1:12], ncol = 3)
      }else if(length(genes) == 0){
        NULL
      }else{
        FeaturePlot(rval$seurat, reduction = input$all_rd, dims = dims, features = genes, ncol = 3)
      }
      
      
    }
  )
  
  
  
  output$select_cluster<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(params$df_name)){
      
      clust <- levels(params$df_name$cluster)
      
      selectInput(ns("clusters"), label = "Choose cluster",
                  choices = clust, width = "25%")
      
    }else{
      NULL
    }
    
  })
  
    





  
  output$check_clustering <- reactive({
    
    if(!is.null(params$current_seu_name)){
      
      if(!is.null(params$cluster_name) & params$current_seu_name == rval$seurat_selected){
        
        if(params$cluster_name %in% colnames(rval$seurat@meta.data)){
          
          return(!is.null(params$cluster_name))
        }
        
      }
    }
    
    
  })
  
  outputOptions(output, 'check_clustering', suspendWhenHidden=FALSE)
  
  
  
  output$check_FindAllMarkers<- reactive({
    
    
    if(!is.null(params$current_seu_name)){
      
      if(!is.null(params$df_name) & params$current_seu_name == rval$seurat_selected){
        
        return(!is.null(params$df_name))
      }
      
    }
    
    
  })
  
  outputOptions(output, 'check_FindAllMarkers', suspendWhenHidden=FALSE)
  
  
  
  output$check_pca <- reactive({
    if(!is.null(rval$seurat)){
      
      reduc <- Reductions(rval$seurat)
      reduc <- reduc[grepl("^pca", reduc, ignore.case = T)]
      
      if(length(reduc) < 1){
        reduc <- NULL
      }
      
      if(!is.null(rval$genes_list[[rval$seurat_selected]]) & !is.null(reduc)){
        return(!is.null(rval$genes_list[[rval$seurat_selected]]))
      }else{
        params$cluster_name = NULL
        
      }
    }
  })
  
  outputOptions(output, 'check_pca', suspendWhenHidden=FALSE)
  
  
  
  
  
  return(rval)
}



