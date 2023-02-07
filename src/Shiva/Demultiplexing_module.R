#' Import data and build a seurat
#' @param id shiny id
#' @importFrom shinydashboard box
#' @importFrom DT DTOutput
#' @import shiny
#' @import Seurat
#' @export
#' @examples
#' \dontrun{
#' library(shiny)
#' library(shinydashboard)
#' 
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
#' @rdname DemultiplexingUI
#' 
#' 
#' 

DemultiplexingUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      
      box(title = p("HTO Counts Normalization", actionButton(ns("HELP_HTO_NORMALISATION"), "", icon = icon("question-circle"), class = "btn-xs")),
          width = 3, height = NULL, collapsible = TRUE,  
          solidHeader = TRUE, status = "primary", 
          selectInput(ns("norm_hto"), "Select Normalization Method",  
                      c("Centred log-ratio" = "clr_m",
                        "Log Normalization" = "ln_m",
                        "Relative counts" = "rc_m")),
          actionButton(ns("go_norm_hto"), label = "Normalize")
      ),
      
      conditionalPanel(condition = "output.norm_hto", ns = ns, 
                       
                       box(title = p("Sample Demultiplexing Using MULTI-seq", actionButton(ns("HELP_HTO_DEMULTIPLEX"), "", icon = icon("question-circle"), class = "btn-xs")),
                           width = 4, height = NULL, collapsible = TRUE,
                           solidHeader = TRUE, status = "primary", 
                           radioButtons(ns("autoThresh_hto"), label = "Automatic Computation of Threshold Defining the Best Quantile ?",
                                        c("Yes" = "auto_yes", "No" = "auto_no"), inline = TRUE),
                           uiOutput(ns("quantile_hto")),
                           actionButton(ns("MultiseqDemux"), label = "Launch Demultiplexing")
                       )
      )
    ),
    
    fluidRow(
      conditionalPanel(condition = "output.is_demux", ns = ns, 
                       box(
                         title = p("Demultiplexing Results", actionButton(ns("HELP_HTO_RESULT"), "", icon = icon("question-circle"), class = "btn-xs")),
                         status = "primary", solidHeader = TRUE,
                         collapsible = FALSE, width = 3,
                         tableOutput(ns("datatable_demux")), # Panel to display the table
                         actionButton(ns("filter_hto"), label = "Filter Out Multiplets and Negatives")
                       ),
                       
                       conditionalPanel(condition = "output.is_UMAP_hto", ns = ns, 
                                        box(
                                          title = p("HTO Data Visualization", actionButton(ns("HELP_HTO_PLOTS"), "", icon = icon("question-circle"), class = "btn-xs")),
                                          status = "primary", solidHeader = TRUE,
                                          collapsible = FALSE, width = 9,
                                          tabsetPanel(
                                            tabPanel("UMAP",
                                                     plotOutput(ns("UMAP_hto")) %>% withSpinner(color="#0dc5c1"),
                                                     br(),
                                                     downPlotUI(id = ns("UMAP_hto_export"))
                                            ),
                                            tabPanel("Ridge Plots",
                                                     plotOutput(ns("ridge_hto"), height = 650) %>% withSpinner(color="#0dc5c1"),
                                                     br(),
                                                     downPlotUI(id = ns("ridge_hto_export"))
                                            )
                                          )
                                        )
                       )
                       
      )
    )
  )
  
}


Demultiplexing <- function(input, output, session, rval) {
  
  
  
  params <- reactiveValues(quantile = "Auto",
                           norm = NULL)
  
  
  
  ######### Help buttons ############
  
  
  observeEvent(input[["HELP_HTO_NORMALISATION"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_HTO_NORMALISATION"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_HTO_NORMALISATION"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_HTO_DEMULTIPLEX"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_HTO_DEMULTIPLEX"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_HTO_DEMULTIPLEX"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_HTO_RESULT"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_HTO_RESULT"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_HTO_RESULT"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_HTO_PLOTS"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_HTO_PLOTS"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_HTO_PLOTS"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  
  # Normalization of data HTO when button clicked
  
  observeEvent(input$go_norm_hto, {
    
    if(!is.null(rval$seurat)){
      
      
      # Create modal progress during normalization
      show_modal_spinner(text = 'Normalization... Please wait...', spin = 'circle')
      
      # Check which method is selected
      if (input$norm_hto == "clr_m"){
        rval$seurat <-(NormalizeData(rval$seurat, normalization.method = "CLR", assay = "HTO", scale.factor = 10000, margin = 1,
                                     verbose = TRUE))
        params$norm = "CLR"
      } else if (input$norm_hto == "ln_m"){
        rval$seurat <- (NormalizeData(rval$seurat, normalization.method = "LogNormalize", assay = "HTO", scale.factor = 10000, margin = 1,
                                      verbose = TRUE))
        params$norm  = "Log Normalization"
      } else {
        rval$seurat <- (NormalizeData(rval$seurat, normalization.method = "RC", assay = "HTO", scale.factor = 10000, margin = 1,
                                      verbose = TRUE))
        params$norm  = "Relative counts"
      }
      
      
      
      # Run UMAP instead of compute distance matrix to run tSNE to limit usafe of memory
      rval$seurat  <- RunUMAP(rval$seurat , reduction = NULL, assay = "HTO",
                              features = rownames(rval$seurat[["HTO"]]), reduction.key = "htoUMAP_", reduction.name = "hto_UMAP", 
                              dims = NULL)
      
      # # Calculate a distance matrix using HTO
      # hto_dist_mtx <- as.matrix(dist(t(as.matrix(GetAssayData(rval$seurat, assay = "HTO", slot = "data")))))
      # 
      # # Adjust perplexcity
      # perp <- ncol(rval$seurat)^(1/2)
      # 
      # # Calculate tSNE embeddings with a distance matrix
      # rval$seurat <- RunTSNE(rval$seurat, reduction.name = "hto_tsne", distance.matrix = hto_dist_mtx, seed.use = 1234, perplexity = perp)
      
      # Close modal progress when tSNE calculation is done
      remove_modal_spinner()
      
    }
    
  })
  
  
  # Create slider bar to choose the quantile
  
  output$quantile_hto <- renderUI({
    
    ns <- session$ns
    
    # Create slider only if auto threshold set to "no"
    if(input$autoThresh_hto == "auto_no"){
      sliderInput(ns("quantileman"), label = ("Set the Quantile to Use"), min = 0, 
                  max = 1, value = 0.5, step = 0.01, width = '55%')
    }else{
      NULL
    }
    
  })
  
  
  
  
  
  observeEvent(input$MultiseqDemux, {
    
    # Create modal progress during Demultiplexing
    show_modal_spinner(text = 'Demultiplexing... Please wait...', spin = 'circle')
    
    if(input$autoThresh_hto == "auto_yes"){
      
      params$quantile = "Auto"
      
      rval$seurat <- MULTIseqDemux( rval$seurat,
                                    assay = "HTO",
                                    autoThresh = TRUE,
                                    verbose = TRUE)
      
      saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))
      
    }else{
      
      if(input$quantileman < 0 || input$quantileman > 1 || input$quantileman == ''){
        
        showModal(modalDialog(
          "The quantile must be between 0 and 1",
          size = "s",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        
      }else{
        
        validate(
          need(!is.na(input$quantileman), ''),
          need(input$quantileman>=0, ''),
          need(input$quantileman<=1, '')
        )
        
        quantile <- input$quantileman
        
        params$quantile = quantile
        
        rval$seurat <- MULTIseqDemux( rval$seurat,
                                      assay = "HTO",
                                      quantile = quantile,
                                      autoThresh = FALSE,
                                      verbose = TRUE)
        
        saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))
      }
      
    }
    
    if(!is.null(rval$hto_meta)){
      
      rownames(rval$hto_meta) <- str_replace_all(rownames(rval$hto_meta), pattern = "_", replacement = "-")
      
      for(col in colnames(rval$hto_meta)){
        rval$seurat@meta.data[,col] = "NA"
        for(row in rownames(rval$hto_meta)){
          rval$seurat@meta.data[which(rval$seurat@meta.data$MULTI_ID == row),col] = rval$hto_meta[row,col]
        }
        rval$seurat@meta.data[,col] <- as.factor(rval$seurat@meta.data[,col])
      }
      
    }
    
    # Close modal progress when Demultiplexing is done
    remove_modal_spinner()
    
    
  }) 
  
  
  
  
  
  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  htoModal <- function(failed = FALSE, ncells = 0, ns = NULL) {
    modalDialog(
      paste0("You will create a new project containing ", ncells, " cells."),
      size = "m",
      easyClose = FALSE,
      HTML("<br><br>"),
      textInput(ns("project_hto"), "Enter new project name", value = paste0(rval$seurat_selected,"_afterDemux")),
      
      if (failed)
        div(tags$b("Project name already exist or is invalid (only alphanumeric and _ characters)", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("filter_hto_ok"), "OK")
      )
    )
  }
  
  
  
  observeEvent(input$filter_hto, {
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      levels_hto <- setdiff(as.character(droplevels(rval$seurat@meta.data$MULTI_ID)),c("Doublet","Negative"))
      
      ncells <- sum(rval$seurat@meta.data$MULTI_ID %in% levels_hto)
      
      if(ncells >= 50){
        showModal(htoModal(ncells = ncells, ns = ns))
      }else {
        showModal(
          modalDialog(
            "No sufficient cells to conserve. Try to demultiplex with another quantile.",
            size = "m",
            easyClose = TRUE,
            footer = tagList(
              modalButton("OK")
            )
          )
        )
      }
    }
    
  }) 
  
  
  # When OK button is pressed, attempt to create new project. If successful,
  # remove the modal and create it. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$filter_hto_ok, {
    
    ns <- session$ns
    
    # Check that data object exists and is data frame.
    if (!(input$project_hto %in% names(rval$seurat_list)) && input$project_hto != '' && !grepl("[^A-Za-z0-9_]", input$project_hto) == TRUE) {
      
      removeModal()
      
      # Create modal progress during rds Saving
      show_modal_spinner(text = paste0('Saving ', rval$seurat_selected,'... Please wait...'), spin = 'circle')
      
      saveRDS(rval$seurat, paste0(rval$output,"/",rval$seurat_selected,".rds"))
      
      # Close modal progress when rds is saved
      remove_modal_spinner()
      
      
      # Create modal progress during object creation
      show_modal_spinner(text = paste0('Creating ', input$project_hto,'... Please wait...'), spin = 'circle')
      
      # rval$parameters[[input$project_hto]] <- c(rval$parameters[[input$project_hto]], "parent" = rval$seurat_selected)
      rval$parameters[[input$project_hto]][["parent"]] <- rval$seurat_selected
      rval$parameters[[input$project_hto]][["after_step"]] <- "demux"
      rval$parameters[[input$project_hto]]["hto_quantile"] = params$quantile
      rval$parameters[[input$project_hto]]["norm_hto"] = params$norm
      
      rm_cells <- sum(rval$seurat@meta.data$MULTI_ID %in% c("Doublet","Negative"))
      rval$parameters[[input$project_hto]]["rm_cells"] = rm_cells
      
      rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "demux")
      
      levels_hto <- setdiff(as.character(droplevels(rval$seurat@meta.data$MULTI_ID)),c("Doublet","Negative"))
      
      Idents(rval$seurat) <- "MULTI_ID"
      
      rval$seurat <- subset(rval$seurat, idents = levels_hto)
      
      rval$seurat_list[[input$project_hto]] <- input$project_hto
      
      rval$seurat_selected <- input$project_hto
      
      rval$genes_list[[rval$seurat_selected]][[paste0("All_genes_",rval$seurat_selected)]] = rownames(rval$seurat)
      
      saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))
      
      # Close modal progress when object is created
      remove_modal_spinner()
      
      
    } else {
      levels_hto <- setdiff(as.character(droplevels(rval$seurat@meta.data$MULTI_ID)),c("Doublet","Negative"))
      ncells <- sum(rval$seurat@meta.data$MULTI_ID %in% levels_hto)
      showModal(htoModal(failed = TRUE, ns = ns, ncells = ncells))
    }
    
  })
  
  
  
  
  # Display a DT table containing HTO (doublet, singlet, negative) and Frequence
  output$datatable_demux <- renderTable(
    if(!is.null(rval$seurat)){
      if("MULTI_ID" %in% colnames(rval$seurat@meta.data)){
        table(rval$seurat@meta.data["MULTI_ID"])
      }
    })
  
  # UMAP_hto plotting
  UMAP_hto_plotting <-  reactive({
    
    if(!is.null(rval$seurat)){
      if(("hto_UMAP" %in% Reductions(rval$seurat)) && ("MULTI_ID" %in% colnames(rval$seurat@meta.data))){
        DimPlot(rval$seurat, reduction = "hto_UMAP", group.by = "MULTI_ID") + ggtitle(paste0("Quantile used: ", params$quantile))
      }
    }
    
  })
  
  output$UMAP_hto <- renderPlot(
    UMAP_hto_plotting()
    )
  #export the UMAP_hto
  callModule(downPlotServer, id = "UMAP_hto_export", data = UMAP_hto_plotting,
             out_file = "UMAP_hto")
  
  
  
  #ridge_hto plotting
  ridge_hto_plotting <- reactive({
    
    if(!is.null(rval$seurat)){
      if(("hto_UMAP" %in% Reductions(rval$seurat)) && ("MULTI_ID" %in% colnames(rval$seurat@meta.data))){
        RidgePlot(rval$seurat, assay = "HTO", features = rownames(rval$seurat[["HTO"]]), group.by = "MULTI_ID", ncol = 3)
      }
    }
    
  })
  
  output$ridge_hto <- renderPlot(
    ridge_hto_plotting()
    )
  #export the ridge_hto_plotting
  callModule(downPlotServer, id = "ridge_hto_export", data = ridge_hto_plotting,
             out_file = "ridge_hto")
  
  # Check if NORM_HTO files imported
  output$norm_hto<- reactive({
    if(!is.null(rval$seurat)){
      return(isTRUE("NormalizeData.HTO" %in% Command(rval$seurat)))
    }
  })
  
  outputOptions(output, 'norm_hto', suspendWhenHidden=FALSE)
  
  
  # Check if demultiplexing is done
  output$is_demux<- reactive({
    if(!is.null(rval$seurat)){
      return(isTRUE("MULTI_ID" %in% colnames(rval$seurat@meta.data)))
    }
  })
  
  outputOptions(output, 'is_demux', suspendWhenHidden=FALSE)
  
  # Check if UMAP on HTO is done
  output$is_UMAP_hto<- reactive({
    if(!is.null(rval$seurat)){
      return(isTRUE("hto_UMAP" %in% Reductions(rval$seurat)))
    }
  })
  
  outputOptions(output, 'is_UMAP_hto', suspendWhenHidden=FALSE)
  
  
  output$no_UMAP_hto<- reactive({
    if(!is.null(rval$seurat)){
      return(isFALSE("hto_UMAP" %in% Reductions(rval$seurat)))
    }
  })
  
  outputOptions(output, 'no_UMAP_hto', suspendWhenHidden=FALSE)
  
  return(rval)
}
