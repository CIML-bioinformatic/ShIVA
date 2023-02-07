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
#' @rdname TSNE_UMAPUI
#' 

TSNE_UMAPUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    
    conditionalPanel(condition = "output.check_pca", ns = ns,
                     box(title = p("PCA to Use", actionButton(ns("HELP_REDUCTION_PCA"), "", icon = icon("question-circle"), class = "btn-xs")),
                         width = 6, height = NULL, solidHeader = TRUE, 
                         status = "primary", collapsible = T,
                         uiOutput(ns("select_rd")),
                         plotOutput(ns("ScreePlot"), height = 185) %>% withSpinner(color="#0dc5c1")
                         
                     ),
                     
                     box(title = p("Parameters for t-SNE Embedding", actionButton(ns("HELP_TSNE_PARAMS"), "", icon = icon("question-circle"), class = "btn-xs")),
                         width = 3, height = NULL, solidHeader = TRUE,
                         status = "primary", collapsible = T,
                         uiOutput(ns("nbrDimension")),
                         uiOutput(ns("nb_perp_tsne")),
                         textInput(ns("tsne_name"), "Enter a Name for the t-SNE Embedding", "tsne", width = '75%'),
                         actionButton(ns("run_rd_tsne"), label = "Run t-SNE")
                         
                     ),
                     
                     box(title = p("Parameters for UMAP Embedding", actionButton(ns("HELP_UMAP_PARAMS"), "", icon = icon("question-circle"), class = "btn-xs")),
                         width = 3, height = NULL, solidHeader = TRUE, 
                         status = "primary", collapsible = T,
                         uiOutput(ns("nbrDimension_umap")),
                         uiOutput(ns("nb_neig_umap")),
                         textInput(ns("umap_name"), "Enter a Name for the UMAP Embedding", "umap", width = '75%'),
                         actionButton(ns("run_rd_umap"), label = "Run UMAP")
                         
                     )
    ),
    
    conditionalPanel(condition = "output.check_tsne", ns = ns,
                     box(title = "Visualization of the t-SNE Embedding",
                         width = 6,
                         solidHeader = TRUE, status = "primary", 
                         plotOutput(ns("plottsne")) %>% withSpinner(color="#0dc5c1"),
                         downPlotUI(ns("tsne_plot"))

                     )
    ),
    
    conditionalPanel(condition = "output.check_umap", ns = ns,
                     box(title = "Visualization of the UMAP Embedding",
                         width = 6,
                         solidHeader = TRUE, status = "primary",
                         plotOutput(ns("plotumap")) %>% withSpinner(color="#0dc5c1"),
                         downPlotUI(ns("umpa_plot"))

                     )
    )
  )
}




TSNE_UMAP <- function(input, output, session, rval) {
  
  # Create variable for module
  params <- reactiveValues(tsne_name = NULL,
                           umap_name = NULL,
                           current_seu_name_tsne = NULL,
                           current_seu_name_umap = NULL,
                           nb_perp_tsne = NULL,
                           rd = NULL,
                           nbrDimension = NULL,
                           tsne_name = NULL,
                           dims = NULL,
                           n.neighbors = NULL,
                           reduction.name = NULL)
  
  
  
  
  
  ######### Help buttons ############
  
  
  observeEvent(input[["HELP_REDUCTION_PCA"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_REDUCTION_PCA"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_REDUCTION_PCA"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_TSNE_PARAMS"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_TSNE_PARAMS"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_TSNE_PARAMS"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  observeEvent(input[["HELP_UMAP_PARAMS"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_UMAP_PARAMS"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_UMAP_PARAMS"),"value"]),
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
        params$tsne_name = NULL
        params$umap_name = NULL
      } 
      
    }else{
      NULL
    }   
    
  }) 
  
  
  output$nbrDimension<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$rd)){
      numericInput(ns("nbrDimension"), label="Set the number of PC to use", value = 10, min = 0, max = length(nbPCACol()), step = 1)
    }else{
      NULL
    }
    
  }) 
  
  
  
  
  
  observeEvent(input$helpBtPerpTsne, {
    
    showModal(modalDialog(
      title = "Perplexity",
      "In t-SNE, the perplexity may be viewed as a knob that sets 
      the number of effective nearest neighbors.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
  
  
  
  
  
  output$nb_perp_tsne<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$rd)){
      numericInput(ns("nb_perp_tsne"), p("Set the perplexity value", actionButton(ns("helpBtPerpTsne"), "", icon = icon("question-circle"), class = "btn-xs")), value = 30, min = 1, max = 100, step = 1)
    }else{
      NULL
    }
    
  }) 
  
  
  
  
  
  observeEvent(input$run_rd_tsne, {
    
    if(input$nbrDimension < 2 || input$nbrDimension > length(nbPCACol()) || is.na(input$nbrDimension)){
      
      showModal(modalDialog(
        paste0("The number of PCs must be between 2 and ",length(nbPCACol())),
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    }
    
    
    
    if(input$nb_perp_tsne < 1 || input$nb_perp_tsne > 100 || is.na(input$nb_perp_tsne)){
      
      showModal(modalDialog(
        "The perplexity must be between 1 and 100",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    }
    
    
    if(input$tsne_name == ''){
      showModal(modalDialog(
        "Please enter a tSNE name.",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    
    if(!grepl("[^A-Za-z0-9_]", input$tsne_name) == FALSE || grepl("^tsne", input$tsne_name, ignore.case = T) == FALSE){
      showModal(modalDialog(
        "Please enter a tSNE name containing 'tsne' and only alphanumeric or _ characters.",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    
    if(input$tsne_name %in% Reductions(rval$seurat)){
      showModal(modalDialog(
        "This name already exist.",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    
    validate(
      need(str_count(input$tsne_name, "\\S+") == 1, ''),
      need(grepl("^tsne", input$tsne_name, ignore.case = T) == TRUE, ''),
      need(input$tsne_name != '', ''),
      need(!input$tsne_name %in% Reductions(rval$seurat), ''),
      need(!is.na(input$nbrDimension), ''),
      need(input$nbrDimension >= 2, ''),
      need(input$nbrDimension <= length(nbPCACol()), ''),
      need(!is.na(input$nb_perp_tsne), ''),
      need(input$nb_perp_tsne >= 1, ''),
      need(input$nb_perp_tsne <= 100, ''),
      need(!grepl("[^A-Za-z0-9_]", input$tsne_name) == TRUE, '')
    )
    
    
    # Create modal progress during tsne
    show_modal_spinner(text = 'Computing t-SNE... Please Wait...', spin = 'circle')
    
    # seu <- RunPCA(seu, features = VariableFeatures(object = seu))
    rval$seurat <- RunTSNE(object = rval$seurat,
                           assay = rval$norm_assay,
                           perplexcity = input$nb_perp_tsne,
                           reduction = input$rd,
                           dims = 1:input$nbrDimension,
                           reduction.name = input$tsne_name,
                           seed.use = 1234)
    
    rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "tsne")
    rval$parameters[[rval$seurat_selected]][[input$tsne_name]] = c("pca" = input$rd, "npcs" = input$nbrDimension, "perp" = input$nb_perp_tsne)
    
    
    params$tsne_name <- input$tsne_name
    params$current_seu_name_tsne <- rval$seurat_selected
    params$nb_perp_tsne = input$nb_perp_tsne
    params$rd_tsne = input$rd
    params$nbrDimension_tsne = input$nbrDimension
    params$tsne_name = input$tsne_name
    
    saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))
    
    # Close modal progress when tsne is done
    remove_modal_spinner()
    
    
  })
  
  tnse_plott <- reactive({
     if(!is.null(params$tsne_name)){
      DimPlot(rval$seurat, reduction = params$tsne_name, 
              group.by = 'Project',  pt.size =0.5, shape.by = 'Project') +
        theme(legend.position="none") +
        xlab("tSNE_1")+
        ylab("tSNE_2")+
         #change
        ggtitle(params$tsne_name) + scale_shape_manual(values = c(19))
    }
    })

  output$plottsne <- renderPlot(
    tnse_plott()
   
  )

  ## caliing the module
  callModule(downPlotServer, id = "tsne_plot", data = tnse_plott, 
    out_file = "TSNE_plot")
  
  
  ###################### UMAP ######################
  
  
  output$nbrDimension_umap <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$rd)){
      numericInput(ns("nbrDimension_umap"), label="Number of PCs to use", value = 10, min = 0, max = length(nbPCACol()), step = 1)
    }else{
      NULL
    }
    
  })
  
  
  
  observeEvent(input$helpBtNeigUmap, {
    
    showModal(modalDialog(
      title = "N neighbors",
      "This determines the number of neighboring points 
      used in local approximations of manifold structure. 
      Larger values will result in more global structure
      being preserved at the loss of detailed local structure.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
  
  
  output$nb_neig_umap<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$rd)){
      numericInput(ns("nb_neig_umap"), p("Set the Number of Neighbors to Use", actionButton(ns("helpBtNeigUmap"), "", icon = icon("question-circle"), class = "btn-xs")), value = 30, min = 5, max = 50, step = 1)
    }else{
      NULL
    }
    
  })
  
  
  
  
  observeEvent(input$run_rd_umap, {
    
    
    if(input$nbrDimension_umap < 2 || input$nbrDimension_umap > length(nbPCACol()) || is.na(input$nbrDimension_umap)){
      
      showModal(modalDialog(
        paste0("The number of PCs must be between 2 and ",length(nbPCACol())),
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    }
    
    
    if(input$nb_neig_umap < 5 || input$nb_neig_umap > 50 || is.na(input$nb_neig_umap)){
      
      showModal(modalDialog(
        "The n_neighbors must be between 5 and 50",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    }
    
    
    if(input$umap_name == ''){
      showModal(modalDialog(
        "Please enter a tSNE name.",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    
    if(!grepl("[^A-Za-z0-9_]", input$umap_name) == FALSE || grepl("^umap", input$umap_name, ignore.case = T) == FALSE){
      showModal(modalDialog(
        "Please enter a UMAP name containing 'umap' and only alphanumeric or _ characters.",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    
    if(input$umap_name %in% Reductions(rval$seurat)){
      showModal(modalDialog(
        "This name already exist.",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    
    
    validate(
      need(str_count(input$umap_name, "\\S+") == 1, ''),
      need(grepl("^umap", input$umap_name, ignore.case = T) == TRUE, ''),
      need(input$umap_name != '', ''),
      need(!input$umap_name %in% Reductions(rval$seurat), ''),
      need(!is.na(input$nbrDimension_umap), ''),
      need(input$nbrDimension_umap >= 2, ''),
      need(input$nbrDimension_umap <= length(nbPCACol()), ''),
      need(!is.na(input$nb_neig_umap), ''),
      need(input$nb_neig_umap >= 5, ''),
      need(input$nb_neig_umap <= 50, ''),
      need(!grepl("[^A-Za-z0-9_]", input$umap_name) == TRUE, '')
    )
    
    
    # Create modal progress during UMAP
    show_modal_spinner(text = 'Computing UMAP... Please Wait...', spin = 'circle')
    
    # seu <- RunPCA(seu, features = VariableFeatures(object = seu))
    rval$seurat <- RunUMAP(rval$seurat, 
                           assay = rval$norm_assay,
                           reduction = input$rd,
                           dims = 1:input$nbrDimension_umap,
                           n.neighbors = input$nb_neig_umap,
                           reduction.name = input$umap_name,
                           seed.use = 1234)
    
    rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "umap")
    rval$parameters[[rval$seurat_selected]][[input$umap_name]] = c("pca" = input$rd, "npcs" = input$nbrDimension_umap, "neig" = input$nb_neig_umap)
    
    params$umap_name <- input$umap_name
    params$current_seu_name_umap <- rval$seurat_selected
    params$rd_umap = input$rd
    params$nbrDimension_umap = input$nbrDimension_umap
    params$nb_neig_umap = input$nb_neig_umap
    params$umap_name = input$umap_name
    
    saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))
    
    # Close modal progress when UMAP is done
    remove_modal_spinner()
    
    
  })
  
  
  umap_plott <- reactive({
     if(!is.null(params$umap_name)){
      DimPlot(rval$seurat, reduction = params$umap_name, group.by = 'Project',  pt.size =0.5, shape.by = 'Project') +
        theme(legend.position="none") +
        xlab("UMAP_1")+
        ylab("UMAP_2")+
        ggtitle(params$umap_name) + scale_shape_manual(values = c(19))
    }
    })
 output$plotumap <- renderPlot(
  umap_plott()
   
  )

 ## caliing the module
  callModule(downPlotServer, id = "umpa_plot", data = umap_plott, 
    out_file = "umap_plot")
  
  
  
  nbPCACol <- reactive({
    
    if(!is.null(input$rd)){
      return(colnames(Embeddings(rval$seurat, reduction = input$rd)))
    }
    
  })
  
tsnePCA <-reactive({


    if(!is.null(input$rd)){
      if(length(nbPCACol()) >= 50){
        ndims = 50
      }else{
        ndims = length(nbPCACol())
      }
      
      ElbowPlot(rval$seurat, ndims = ndims, reduction = input$rd)
    }
  
  
  })

output$ScreePlot <- renderPlot(
  tsnePCA()
  )
  
  
  
  output$check_tsne <- reactive({
    
    if(!is.null(params$current_seu_name_tsne)){
      
      if(!is.null(params$tsne_name) & params$current_seu_name_tsne == rval$seurat_selected){
        
        if(params$tsne_name %in% Reductions(rval$seurat)){
          
          return(!is.null(params$tsne_name))
          
        }
      }
    }
    
  })
  
  outputOptions(output, 'check_tsne', suspendWhenHidden=FALSE)
  
  
  output$check_umap <- reactive({
    
    if(!is.null(params$current_seu_name_umap)){
      
      if(!is.null(params$umap_name) & params$current_seu_name_umap == rval$seurat_selected){
        
        if(params$umap_name %in% Reductions(rval$seurat)){
          
          return(!is.null(params$umap_name))
          
        }
      }
    }
  })
  
  outputOptions(output, 'check_umap', suspendWhenHidden=FALSE)
  
  
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
        params$tsne_name = NULL
        params$umap_name = NULL
      }
    }
  })
  
  outputOptions(output, 'check_pca', suspendWhenHidden=FALSE)
  
  
  
  
  
  return(rval)
}


