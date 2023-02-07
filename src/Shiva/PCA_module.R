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
#' @rdname PCAUI
#' 

PCAUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(6, 
             
             conditionalPanel(condition = "output.check_vg", ns = ns,
                              box(title = p("PCA Parameters", actionButton(ns("HELP_PCA_PARAMS"), "", icon = icon("question-circle"), class = "btn-xs")),
                                  width = NULL, height = NULL, 
                                  solidHeader = TRUE, status = "primary", collapsible = T,
                                  uiOutput(ns("list_vg")),
                                  uiOutput(ns("nb_pca")),
                                  textInput(ns("pca_name"), "Enter a Name for the PCA Result", "pca", width = '50%'),
                                  uiOutput(ns("select_regres")),
                                  actionButton(ns("run_pca"), label = "Run PCA")
                              )
             ),
             conditionalPanel(condition = "output.check_pca", ns = ns,
                              box(title = p("Exploration of PC versus Metadata Correlation", actionButton(ns("HELP_PCA_SCATTER"), "", icon = icon("question-circle"), class = "btn-xs")),
                                  width = NULL, height = NULL, solidHeader = TRUE, status = "primary",
                                  plotOutput(ns("plotPCA")) %>% withSpinner(color="#0dc5c1"),
                                  column(6,uiOutput(ns("select_PCA"))),
                                  column(6,uiOutput(ns("select_numeric")))

                                  #@Asif
                                  ,downPlotUI(ns("pcaCorre"))     
                              )
             )
      ),
      
      column(6,
             conditionalPanel(condition = "output.check_pca", ns = ns,
                              box(title = p("Exploration of the Principal Components", actionButton(ns("HELP_PCA_PC"), "", icon = icon("question-circle"), class = "btn-xs")),
                                  height = NULL, solidHeader = TRUE, 
                                  status = "primary", width = NULL, collapsible = T,
                                  tabsetPanel(
                                    tabPanel("Scree plot",
                                             plotOutput(ns("ScreePlot"), height = 300) %>% withSpinner(color="#0dc5c1"),
     					     downPlotUI(ns("pcaElbow")) 	
                                    ),
                                    
                                    tabPanel("Heatmap",
                                             uiOutput(ns("select_PC_heat")),
                                             plotOutput(ns("DimHeatmap"), height = 300) %>% withSpinner(color="#0dc5c1"),
                                             radioButtons(ns("fileFormatDimHeatmap"), " ",  
                                                          choices = c("jpeg", "png", "pdf"), 
                                                          selected = "jpeg", inline = TRUE ),
                                             downloadButton(ns("down_DimHeatmap"), "Export", class = "btn btn-primary") 
                                    )
                                  )
                              ),
                              
                              box(title = p("PC versus Metadata Correlations", actionButton(ns("HELP_PCA_HEATMAP"), "", icon = icon("question-circle"), class = "btn-xs")),
                                  width = NULL, height = NULL, 
                                  solidHeader = TRUE, status = "primary", collapsible = T,
                                  plotOutput(ns("CorHeat"), height = 300) %>% withSpinner(color="#0dc5c1"),
                                  radioButtons(ns("fileFormat"), " ",  
                                               choices = c("jpeg", "png", "pdf"), 
                                               selected = "jpeg", inline = TRUE ),
				                          downloadButton(ns("down_CP"), "Export", class = "btn btn-primary")
                              )
             )
      )
    )
  )
}

PCA <- function(input, output, session, rval) {
  
  
  # Create variable for mpdule
  params <- reactiveValues(pca_name = NULL,
                           current_seu_name = NULL)
  
  
  
  
  
  ######### Help buttons ############
  
  
  observeEvent(input[["HELP_PCA_PARAMS"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_PCA_PARAMS"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_PCA_PARAMS"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_PCA_SCATTER"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_PCA_SCATTER"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_PCA_SCATTER"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  observeEvent(input[["HELP_PCA_PC"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_PCA_PC"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_PCA_PC"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  observeEvent(input[["HELP_PCA_HEATMAP"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_PCA_HEATMAP"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_PCA_HEATMAP"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  
  
  
  
  
  # Create select input containing the variable genes list
  output$list_vg <- renderUI({
    
    ns <- session$ns
    
    list <- setdiff(c(names(rval$genes_list[[rval$seurat_selected]]),names(rval$genes_list[["imported"]])),
                    c("s.genes","g2m.genes"))
    
    if(!is.null(rval$seurat)){
      if(!is.null(rval$genes_list[[rval$seurat_selected]])){
        selectInput(ns("list_vg"), label = "Choose Gene List",
                    choices = list, selected = tail(list,1))
      }else{
        params$pca_name = NULL
      } 
      
    }else{
      NULL
    }           
    
  })
  
  
  # Create numeric input to enter the number of PCs to calculate
  output$nb_pca<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      numericInput(ns("nb_pca"),
                   label="Set the Number of PC to Compute", value = 50, min = 1, max = 1000, step = 1, width = '50%')
    }else{
      NULL
    }
    
  })  
  
  
  observeEvent(input$run_pca, {
    
    
    if(input$nb_pca < 10 || input$nb_pca > 1000 || is.na(input$nb_pca)){
      
      showModal(modalDialog(
        "The number of PCs must be between 10 and 1000",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    }
    
    if(input$pca_name == ''){
      showModal(modalDialog(
        "Please enter a PCA name.",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    
    if(!grepl("[^A-Za-z0-9_]", input$pca_name) == FALSE || grepl("^pca", input$pca_name, ignore.case = T) == FALSE){
      showModal(modalDialog(
        "Please enter a PCA name containing 'pca' and only alphanumeric or _ characters.",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    
    if(input$pca_name %in% Reductions(rval$seurat)){
      showModal(modalDialog(
        "This name already exist.",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    
    validate(
      need(str_count(input$pca_name, "\\S+") == 1, ''),
      need(grepl("^pca", input$pca_name, ignore.case = T) == TRUE, ''),
      need(input$pca_name != '', ''),
      need(!input$pca_name %in% Reductions(rval$seurat), ''),
      need(!is.na(input$nb_pca), ''),
      need(input$nb_pca >= 10, ''),
      need(input$nb_pca <= 1000, ''),
      need(!grepl("[^A-Za-z0-9_]", input$pca_name) == TRUE, '')
    )
    
    
    # Create progress during import file process
    # if(!is.null(input$to_regres)){
    
    
    if(is.null(input$to_regres)){
      
      # Create modal progress during pca
      show_modal_spinner(text = 'Computing PCA... Please Wait...', spin = 'circle')
      
      all.genes <- rownames(rval$seurat)
      
      rval$seurat <- ScaleData(rval$seurat, features = all.genes, do.scale = FALSE, 
                               do.center = TRUE, vars.to.regress = input$to_regres)
      
      rval$parameters[[rval$seurat_selected]][[input$pca_name]] = c("genes" = input$list_vg, "npcs" = input$nb_pca)
      

    }else{
      
      # Create modal progress during regression
      show_modal_spinner(text = 'Regression... Please wait...', spin = 'circle')
      
      all.genes <- rownames(rval$seurat)
      
      rval$seurat <- ScaleData(rval$seurat, features = all.genes, do.scale = FALSE, 
                               do.center = TRUE, vars.to.regress = input$to_regres)
      
      rval$parameters[[rval$seurat_selected]][[input$pca_name]] = c("genes" = input$list_vg, "npcs" = input$nb_pca, "regress" = input$to_regres)
      
      # Close modal progress when regression is done
      remove_modal_spinner()
      
      
      # Create modal progress during pca
      show_modal_spinner(text = 'PCA calculation... Please wait...', spin = 'circle')
      
      
    }
    
    
    
    
    if(input$list_vg %in% names(rval$genes_list[[rval$seurat_selected]])){
      
      gene_list <- rval$genes_list[[rval$seurat_selected]][[input$list_vg]]
      
    }else if(input$list_vg %in% names(rval$genes_list[["imported"]])){
      
      gene_list <- rval$genes_list[["imported"]][[input$list_vg]]
      
    }else{
      
      gene_list <- NULL
    }
    
    
    
    rval$seurat <- RunPCA(rval$seurat, features = gene_list, 
                          npcs = input$nb_pca, reduction.name = input$pca_name,
                          assay = rval$norm_assay)
    
    rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "pca")

    params$pca_name <- input$pca_name
    params$current_seu_name <- rval$seurat_selected

    saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))
    
    # Close modal progress when pca is done
    remove_modal_spinner()
    
    
  })
  
  
  nbPCACol <- reactive({
    
    if(!is.null(params$pca_name) && params$current_seu_name == rval$seurat_selected){
      
      return(colnames(Embeddings(rval$seurat, reduction = params$pca_name)))
      
    }
    
  })
  
  
  # create select inout X Axis containing pc
  output$select_PCA<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(params$pca_name) && !is.null(rval$seurat)){
      
      choicesPCA <- nbPCACol()
      
      selectInput(ns("axisPCA"), "X Axis",
                  choicesPCA, selected = choicesPCA[1])
      
    }else{
      NULL
    }
    
  })
  
  
  numericCol <- reactive({
    
    if(!is.null(params$pca_name) && !is.null(rval$seurat)){
      
      return(colnames(select_if(rval$seurat@meta.data, is.numeric)))
      
    }
    
  })
  
  
  # create select input Y Axis containing the numeric metadata 
  output$select_numeric <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(params$pca_name) && !is.null(rval$seurat)){
      
      choicesNumeric <- numericCol()
      
      selectInput(ns("numeric"), " Y Axis",
                  choicesNumeric, selected = choicesNumeric[1])
      
    }else{
      NULL
    }
    
  })
  
  
  # Plot (FeatureScatter) of the pca

  PCA_ploting <- reactive({

    if(!is.null(rval$seurat) && !is.null(input$axisPCA) && !is.null(input$numeric)){
      cor = cor(FetchData(rval$seurat, vars = input$axisPCA), FetchData(rval$seurat, vars = input$numeric), method="spearman")
      FeatureScatter(rval$seurat, feature1 = input$axisPCA, feature2 = input$numeric, group.by = 'Project',  pt.size =0.5) +
        theme(legend.position="none") +
        ggtitle(paste0("Spearman correlation : ", round(cor,2)))
    }else{
      NULL
    }

  })
  
  output$plotPCA <- renderPlot(
    PCA_ploting()
    
  )
  
  #save the plot 
  callModule(downPlotServer, id = "pcaCorre", data = PCA_ploting, out_file = "pcaCorre_plot")


  #@Asif
  elbow_plot <- reactive({
    if(!is.null(params$pca_name)){
      
      if(length(nbPCACol()) >= 50){
        ndims = 50
      }else{
        ndims = length(nbPCACol())
      }
      
      ElbowPlot(rval$seurat, ndims = ndims, reduction = params$pca_name)
    }

  })
  
  # Plot (ElbowPLot) to see the most significant pc
  output$ScreePlot <- renderPlot({
    elbow_plot()
    
  })
  
  
  callModule(downPlotServer, id = "pcaElbow", data = elbow_plot, out_file = "pvaElbow_plot")


  
  
  # Create selectInput containing the PCs for the heatmap
  output$select_PC_heat <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(params$pca_name)){
      
      choicesPCA <- 1:length(nbPCACol())
      
      selectInput(ns("PCheat"), "PC",
                  choicesPCA, selected = choicesPCA[1], width = '25%')
      
    }else{
      NULL
    }
    
  })
  
  
  # Plot Heatmap

  DimHeatmap_ploting <- reactive({
    if(!is.null(params$pca_name) && !is.null(input$PCheat)){
      DimHeatmap(rval$seurat, reduction = params$pca_name, 
                 dims = as.numeric(input$PCheat), cells = 500, balanced = TRUE)
    }

  })

  output$DimHeatmap <- renderPlot({
    DimHeatmap_ploting()
    
    
  })
  
  # #write to file
  
  output$down_DimHeatmap <- downloadHandler(
    filename = function(){paste("DimHeatmap", input$fileFormatDimHeatmap, sep = '.')},
    
    content = function(file){
      
      if(input$fileFormatDimHeatmap=='jpeg'){
        jpeg(file, width = 1800, height = 1800, type = "cairo", quality = 90, units = "px")
        if(!is.null(params$pca_name) && !is.null(input$PCheat)){
          DimHeatmap(rval$seurat, reduction = params$pca_name, 
                     dims = as.numeric(input$PCheat), cells = 500, balanced = TRUE)
        }
        dev.off()
        
      }
      
      if(input$fileFormatDimHeatmap=='pdf'){
        pdf(file, width = 7, height = 7)
        if(!is.null(params$pca_name) && !is.null(input$PCheat)){
          DimHeatmap(rval$seurat, reduction = params$pca_name, 
                     dims = as.numeric(input$PCheat), cells = 500, balanced = TRUE)
        }
        dev.off()
        
      }

      if(input$fileFormatDimHeatmap=='png'){
        png(file, width = 1800, height = 1800, type = "cairo", units = "px")
        if(!is.null(params$pca_name) && !is.null(input$PCheat)){
          DimHeatmap(rval$seurat, reduction = params$pca_name, 
                     dims = as.numeric(input$PCheat), cells = 500, balanced = TRUE)
        }
        dev.off()
        
      }

    })
  
  
  # Table with correlations 
  dfHeatCor <- reactive({
    
    if(!is.null(params$pca_name) && params$current_seu_name == rval$seurat_selected){
      
      if(length(nbPCACol()) >= 20){
        ndims = 20
      }else{
        ndims = length(nbPCACol())
      }
      
      df <- Embeddings(rval$seurat, reduction = params$pca_name)[,1:ndims]
      df <- cbind(df, rval$seurat@meta.data[,numericCol()])
      
      df <- cor(df, method = "spearman")
      
      df <- df[numericCol(),nbPCACol()[1:ndims]]
      
      return(df)
    }
    
  })
  
  
  # Plot (corrplot) with correlation between PCs and metadata
  CorHeat_ploting <- reactive({
    if(!is.null(params$pca_name)){
      corrplot(dfHeatCor(), tl.col = "black", tl.srt = 45, type = "full")
    }

  })

  output$CorHeat <- renderPlot({
    CorHeat_ploting()

    })
  # #write to file

  output$down_CP <- downloadHandler(
    filename = function(){paste("corr_plot", input$fileFormat, sep = '.')},
    
    content = function(file){
      
      if(input$fileFormat=='jpeg'){
        jpeg(file, width = 1800, height = 1800, type = "cairo", quality = 90, units = "px")
        corrplot(dfHeatCor(), tl.col = "black", tl.srt = 45, type = "full")
        dev.off()
        
      }
      
      if(input$fileFormat=='pdf'){
        pdf(file, width = 7, height = 7)
        corrplot(dfHeatCor(), tl.col = "black", tl.srt = 45, type = "full")
        dev.off()
        
        }

      

      if(input$fileFormat=='png'){
        png(file, width = 1800, height = 1800, type = "cairo", units = "px")
        corrplot(dfHeatCor(), tl.col = "black", tl.srt = 45, type = "full")
        dev.off()
        
      }
      
      
    })
  #write to file
  callModule(downPlotServer, id = "pcaCorHeat", data = CorHeat_ploting, out_file = "pcaCorHeat_plot")
  
  

  
  # create selectizeinput for the Regression 
  output$select_regres <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(params$pca_name)){
      
      selectizeInput(
        inputId = ns("to_regres"), label = "Select Variable(s) to Regress Out",
        choices = numericCol(),
        selected = NULL, width = "50%", multiple = T
      )  
      
    }else{
      NULL
    }
    
  })
  
  
  
  
  
  # Check if variable genes are calculated
  output$check_pca <- reactive({
    
    if(!is.null(params$current_seu_name)){
      
      if(!is.null(params$pca_name) & params$current_seu_name == rval$seurat_selected){
        
        return(!is.null(params$pca_name))
      }
      
    }
    
  })
  
  outputOptions(output, 'check_pca', suspendWhenHidden=FALSE)
  
  
  output$check_vg <- reactive({
    if(!is.null(rval$seurat)){
      if(!is.null(rval$genes_list[[rval$seurat_selected]])){
        return(!is.null(rval$genes_list[[rval$seurat_selected]]))
      }else{
        params$pca_name = NULL
      }
    }
  })
  
  outputOptions(output, 'check_vg', suspendWhenHidden=FALSE)
  
  
  return(rval)
}


