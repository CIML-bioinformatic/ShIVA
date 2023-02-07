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
#' library(reshape)
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
#' @rdname Variable_GenesUI
#' 

Variable_GenesUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(width = 6, 
             
             conditionalPanel(condition = "output.check_seu", ns = ns,
                              box(title = p("Gene Selection", actionButton(ns("HELP_HVG_PARAMS"), "", icon = icon("question-circle"), class = "btn-xs")),
                                  height = NULL, solidHeader = TRUE, status = "primary",
                                  collapsible = T, width = NULL,
                                  uiOutput(ns("select_methode_vg")),
                                  uiOutput(ns("select_nb_vg")),
                                  actionButton(ns("button_vg"), label = "Identify Variable Genes")
                              )
             ),
             conditionalPanel(condition = "output.check_vg", ns = ns,
                              box(title = p("Visualization of Selected Genes", actionButton(ns("HELP_HVG_PLOTS"), "", icon = icon("question-circle"), class = "btn-xs")),
                                  height = NULL, solidHeader = TRUE,
                                  status = "primary", width = NULL,
                                  plotOutput(ns("vizualisation_vg"))%>% withSpinner(color="#0dc5c1"),
                                  downPlotUI(ns("varDown"))
                              )
             )
             
      ),
      
      column(width = 6,
             conditionalPanel(condition = "output.check_vg", ns = ns,
                              box(title = p("Table of Selected Genes", actionButton(ns("HELP_HVG_TABLE"), "", icon = icon("question-circle"), class = "btn-xs")),
                                  height = NULL, solidHeader = TRUE, 
                                  status = "primary", width = NULL,
                                  # div(style = 'overflow-x: scroll', dataTableOutput(ns("table_variable_genes"))%>% 
                                  div(style = '', tableOutput(ns("table_variable_genes"))%>% 
                                    withSpinner(color="#0dc5c1")),
                                  br(),
                                  downUI(id = ns("var_table"))
                                )
                                  
             )
      )
      
    )
  )
}

Variable_Genes <- function(input, output, session, rval) {
  
  # Create variable for mpdule
  params <- reactiveValues(seu_temp = NULL,
                           vg = NULL,
                           method = NULL,
                           current_seu_name = NULL)
  
  
  
  
  
  ######### Help buttons ############
  
  
  observeEvent(input[["HELP_HVG_PARAMS"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_HVG_PARAMS"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_HVG_PARAMS"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_HVG_PLOTS"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_HVG_PLOTS"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_HVG_PLOTS"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  observeEvent(input[["HELP_HVG_TABLE"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_HVG_TABLE"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_HVG_TABLE"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  
  
  # create selectInput to select method to find variable genes
  output$select_methode_vg <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      selectInput(ns("method_vg"), label = "Select Method", 
                  choices = list("vst" = "vst", "mean.var.plot" = "mean.var.plot", "dispersion" = "dispersion"), 
                  selected = "vst", width = '50%')
    }else{
      NULL
    }
    
  })
  
  
  # create numericInput to choose number of variable genes if method is vst or dispersion
  output$select_nb_vg <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) & !is.null(input$method_vg)){
      if(input$method_vg %in% c("vst","dispersion")){
        numericInput(ns("nb_vg"), label="Set the Maximal Number of Variable Genes", 
                     value = 2000, min = 0, max = nrow(rval$seurat), step = 1, width = "50%")
      }else{
        NULL
      }
    }else{
      NULL
    }
    
  })
  
  
  # Update values when click on button
  observeEvent(input$button_vg, {
    
    
    # Create modal progress during variable features finding
    show_modal_spinner(text = 'Selecting Genes... Please Wait...', spin = 'circle')
    
    # DefaultAssay(rval$seurat) <- "RNA"
    
    if(input$nb_vg < 10 || input$nb_vg > nrow(rval$seurat) || is.na(input$nb_vg)){
      
      showModal(modalDialog(
        paste0("The number of variable genes must be between 10 and ",nrow(rval$seurat)),
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    }else{
      
      validate(
        need(!is.na(input$nb_vg), ''),
        need(input$nb_vg >= 10, ''),
        need(input$nb_vg <= nrow(rval$seurat), '')
      )
      
      if(input$method_vg %in% c("vst","dispersion")){
        nb_vg <- input$nb_vg
      }else{
        nb_vg <- NULL
      }
      
      params$seu_temp <- FindVariableFeatures(rval$seurat, assay = "RNA", selection.method = input$method_vg, nfeatures = nb_vg)
      
      rval$genes_list[[rval$seurat_selected]][[paste(rval$seurat_selected, input$method_vg, length(VariableFeatures(params$seu_temp, assay = "RNA")), sep = "_")]] = VariableFeatures(params$seu_temp, assay = "RNA")
      
      rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "vg")
      
      params$vg <- VariableFeatures(params$seu_temp, assay = "RNA")
      
      params$method <- input$method_vg
      
      params$current_seu_name <- rval$seurat_selected
      
      rval$seurat <- params$seu_temp

      saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))
      
      
    }
    
    # Close modal progress when variable features are finded
    remove_modal_spinner()
    
  })
  
  
  
  # Create dataframe for DT table
  dt_genes <- reactive({
    
    ns <- session$ns
    
    if(!is.null(params$seu_temp)){
      
      Idents(params$seu_temp) <- "mean"

      vg <- VariableFeatures(params$seu_temp, assay = "RNA")
      
      # Calculate mean expression of variable genes
      df_mean <- data.frame(row.names = vg, 
                            mean = AverageExpression(params$seu_temp, assay = "RNA", features = vg))
      
      df_mean <- round(df_mean, 2)

      # Recuperate perc expr from dotplot function of variable genes
      perc <- DotPlot(params$seu_temp, features =  vg, assay = "RNA")
      perc$data <- perc$data[,c("features.plot","id","pct.exp")]
      perc <- as.data.frame(cast(perc$data, features.plot ~ id))
      rownames(perc) <- perc$features.plot
      perc$features.plot <- NULL
      perc = round(as.data.frame(perc),2)
      colnames(perc) <- "pct.cells.express"

      df <- merge(df_mean, perc, by=0, all=TRUE) 
      
      rownames(df) <- df$Row.names
      df$Row.names <- NULL

      # Recuperate value of method used for variable genes
      df_method <- data.frame(row.names = vg,
                              HVFInfo(params$seu_temp, assay = "RNA")[vg,3])
      
      colnames(df_method) <- colnames(HVFInfo(params$seu_temp , assay = "RNA"))[3]
      
      df_method <- round(df_method, 2)

      df <- merge(df, df_method, by=0, all=TRUE) 
      
      rownames(df) <- df$Row.names
      df$Row.names <- NULL
      
      df <- df[order(-df[,ncol(df)]),]
      
      return(df)
    }
  })
  
  
  
  
  output$table_variable_genes <- function() {
    
    
    if(nrow(dt_genes()) > 200){
      kable(dt_genes()[1:200,], "html", caption = "Only top 200 genes display in table. Download table if you want to see all genes.") %>%
        kable_styling(bootstrap_options = c("striped", "hover")) %>%
        scroll_box(width = "100%", height = "700px")
    }else{
      kable(dt_genes(), "html") %>%
        kable_styling(bootstrap_options = c("striped", "hover")) %>%
        scroll_box(width = "100%", height = "700px")
    }
  
  }
  
  # # Return DT table
  # output$table_variable_genes <- renderDataTable({
  #   if(nrow(dt_genes()) > 200){
  #     datatable(dt_genes()[1:200,],  caption = "Only top 200 genes display in table. Download table if you want to see all genes.", options = list(pageLength = 15))
  #   }else{
  #     datatable(dt_genes(),  options = list(pageLength = 15))
  #   }
  # })
  # 
  callModule(downServer, id = "var_table", data = dt_genes, 
    out_file = "varaiableGenes_", comm=NULL)


  
  # Return plot of variable genes
    var_plot <- reactive({
    ns <- session$ns

    if(!is.null(params$seu_temp)){
      
      
      if(length(VariableFeatures(params$seu_temp) > 15)){
        top <- head(VariableFeatures(params$seu_temp), 15)
      }else{
        top <- VariableFeatures(params$seu_temp)
      }
      
      LabelPoints(VariableFeaturePlot(params$seu_temp, selection.method = params$method, assay = "RNA"), points = top, repel = TRUE)
      
    }else{
      NULL
    }

    })
  output$vizualisation_vg <- renderPlot(

    var_plot() 
    )

  callModule(downPlotServer, id = "varDown", data = var_plot, out_file = "var_genes_plot")

  # Check if variable genes are calculated
  output$check_vg <- reactive({
    
    if(!is.null(rval$seurat) & !is.null(params$current_seu_name)){
      if(!is.null(rval$genes_list[[rval$seurat_selected]]) & length(rval$genes_list[[rval$seurat_selected]]) > 1 & params$current_seu_name == rval$seurat_selected){
        return(!is.null(rval$genes_list[[rval$seurat_selected]]))
      }
    }
  })
  
  outputOptions(output, 'check_vg', suspendWhenHidden=FALSE)
  
  
  output$check_seu <- reactive({
    return(!is.null(rval$seurat))
  })
  
  outputOptions(output, 'check_seu', suspendWhenHidden=FALSE)
  
  
  return(rval)
}

