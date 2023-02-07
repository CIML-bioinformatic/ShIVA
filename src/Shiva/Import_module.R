#' Import data and build a Seurat object
#' @param id shiny id
#' @importFrom shinydashboard box
#' @importFrom DT DTOutput
#' @import shiny
#' @export
#' @examples
#' \dontrun
#

ImportUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    h3("Import data to create a new project"),
    br(),
    
    fluidRow(
      
      box(title = p("Type of Data", actionButton(ns("HELP_DATA_IMPORT_MAIN"), "", icon = icon("question-circle"), class = "btn-xs")),
          width = 3, height = NULL,  
          solidHeader = TRUE, status = "primary", 
          radioButtons(ns("type_import"), 
                       label = p("Select the Type of Data to Import "),
                       c("CellRanger Output Files" = "cellranger", 
                         "Seurat R Object (*.rds)" = "rds", 
                         "ShIVA Project" = "flowr"
                         # "Former projects" = "former"
                         ))
      )
      
    ),
    
    fluidRow(
      
      conditionalPanel(condition = "input.type_import == 'cellranger'", ns = ns,
                       box(title = p("Import CellRanger Output Files", actionButton(ns("HELP_DATA_IMPORT_RNA"), "", icon = icon("question-circle"), class = "btn-xs")),
                           width = 4, height = NULL,  
                           solidHeader = TRUE, status = "primary", 
                           fileInput(inputId = ns("crfiles"),
                                     label = "Choose files ",
                                     multiple = TRUE)
                       ),
                       
                       conditionalPanel(condition = "output.crfilesUploaded", ns = ns,
                                        
                                        box(title = p("Import HTO files", actionButton(ns("HELP_DATA_IMPORT_HTO"), "", icon = icon("question-circle"), class = "btn-xs")),
                                            width = 4, height = NULL, 
                                            solidHeader = TRUE, status = "primary", 
                                            fileInput(inputId = ns("htofiles"),
                                                      label = "Choose files",
                                                      multiple = TRUE)
                                        ),               
                                        
                                        box(title = p("Import ADT files", actionButton(ns("HELP_DATA_IMPORT_ADT"), "", icon = icon("question-circle"), class = "btn-xs")),
                                            width = 4, height = NULL, 
                                            solidHeader = TRUE, status = "primary", 
                                            fileInput(inputId = ns("adtfiles"),
                                                      label = "Choose files", 
                                                      multiple = TRUE)
                                        ),     
                                        
                                        box(title = p("Project Setup (CellRanger Files)", actionButton(ns("HELP_CREATE_PROJECT"), "", icon = icon("question-circle"), class = "btn-xs")),
                                            width = 8, height = NULL,
                                            solidHeader = TRUE, status = "primary",
                                            uiOutput(ns("select_hto_adt")),
                                            br(),
                                            div(style = 'overflow-x: scroll', dataTableOutput(ns("features_table"))),
                                            br(),
                                            actionButton(ns("meta_proj"), label = "Add project information"),
                                            br(),
                                            br(),
                                            downUI(id = ns("downloadSummary")),
                                            br(),
                                            br(),
                                            textInput(ns("seu_name"), "Enter a Project Name", "Project", width = '25%'),
                                            selectInput(ns("species"), label = "Select Species", 
                                                        choices = list("Mouse" = "mouse", "Human" = "human", "Other" = "other"), 
                                                        selected = "mouse", width = '25%'),
                                            selectInput(ns("norm_method"), label = "Select Normalization Method",
                                                        choices = list("Log normalization" = "log", "SCTransform" = "SCT"),
                                                        selected = "log", width = '25%'),
                                            numericInput(ns("min_cells"), label = ("Include Features Detected in at Least this Many Cells"), 
                                                         value = 3,  min = 0, width = '55%'),
                                            numericInput(ns("min_features"), label = ("Include Cells where at Least this Many Features are Detected"), 
                                                         value = 200, min = 0, width = '55%'),
                                            actionButton(ns("load"), label = "Create Project")
                                        ),
                                        
                                        conditionalPanel(condition = "output.htofilesUploaded || output.adtfilesUploaded", ns = ns,
                                                         box(title = p("Shared Cellular Barcodes between Libraries", actionButton(ns("HELP_IDENTICAL_CELLS"), "", icon = icon("question-circle"), class = "btn-xs")),
                                                             width = 4, height = NULL,
                                                             solidHeader = TRUE, status = "primary",
                                                             plotOutput(ns("venn_cells"))
                                                         )
                                        )
                                        
                       )
      )
      
    ),
    
    
    conditionalPanel(condition = "input.type_import == 'rds'", ns = ns,
                     fluidRow(
                       box(title = p("Import Seurat R Object", actionButton(ns("HELP_DATA_IMPORT_SEURAT"), "", icon = icon("question-circle"), class = "btn-xs")),
                           width = 4, height = NULL,  
                           solidHeader = TRUE, status = "primary", 
                           fileInput(inputId = ns("rdsfile"),
                                     label = "Choose file",
                                     multiple = FALSE,
                                     accept = c(".rds",".Rds",".RDS"))
                       )
                     ),
                     
                     fluidRow(
                       conditionalPanel(condition = "output.rdsfilesUploaded", ns = ns,               
                                        box(title = p("Imported object", actionButton(ns("HELP_CREATE_PROJECT_SEURAT"), "", icon = icon("question-circle"), class = "btn-xs")),
                                            width = 6, height = NULL,
                                            solidHeader = TRUE, status = "primary",
                                            div(style = 'overflow-x: scroll', dataTableOutput(ns("seurat_table"))),
                                            br(),
                                            actionButton(ns("meta_proj_seu"), label = "Add project information"),
                                            br(),
                                            br(),
                                            textInput(ns("seu_name_rds"), "Enter a Project Name", "Project", width = '25%'),
                                            selectInput(ns("species_rds"), label = "Select Species", 
                                                        choices = list("Mouse" = "mouse", "Human" = "human", "Other" = "other"), 
                                                        selected = "mouse", width = '25%'),
                                            br(),
                                            actionButton(ns("load_rds"), label = "Create Project")
                                        )
                       )
                     )
    ),
    
    conditionalPanel(condition = "input.type_import == 'flowr'", ns = ns,
                     fluidRow(
                       box(title = p("Choose Project", actionButton(ns("HELP_DATA_IMPORT_FORMER"), "", icon = icon("question-circle"), class = "btn-xs")),
                           width = 4, height = NULL,  
                           solidHeader = TRUE, status = "primary", 
                           uiOutput(ns("select_folder")),
                           textOutput(ns("content_folder")),
                           br(),
                           uiOutput(ns("button_folder"))
                       ),
                       box(title = p("Import Rds file", actionButton(ns("HELP_DATA_IMPORT_SHIVA"), "", icon = icon("question-circle"), class = "btn-xs")),
                           width = 4, height = NULL,  
                           solidHeader = TRUE, status = "primary", 
                           fileInput(inputId = ns("flowrfile"),
                                     label = "Choose file",
                                     multiple = FALSE,
                                     accept = c(".rds"))
                       )
                     )
    ),
    
    
    # conditionalPanel(condition = "input.type_import == 'former'", ns = ns,
    #                  fluidRow(
    #                    box(title = "Choose former project",
    #                        width = 4, height = NULL,  
    #                        solidHeader = TRUE, status = "primary", 
    #                        uiOutput(ns("select_folder")),
    #                        textOutput(ns("content_folder")),
    #                        br(),
    #                        uiOutput(ns("button_folder"))
    #                    )
    #                  )
    # )
    
  )
}


#' Import module server function
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param rval a reactivevalues object (can be empty)
#' @return The input reactivevalues object 'rval' with updated elements :
#' \describe{
#'   \item{gating_set_list}{a named list with each element containing :}
#'   \describe{
#'     \item{gating_set}{: a GatingSet objects}
#'     \item{parent}{: the name of its parent GatingSet}
#'   }
#'   \item{gating_set}{selected GatingSet}
#'   \item{gating_set_selected}{Name of the selected GatingSet}
#' }
#' @import shiny
#' @importFrom flowWorkspace pData
#' @importFrom flowCore fsApply
#' @importFrom CytoML open_flowjo_xml open_diva_xml flowjo_to_gatingset fj_ws_get_sample_groups diva_get_sample_groups
#' @importFrom ncdfFlow read.ncdfFlowSet
#' @importFrom DT renderDT
#' @importFrom tools file_ext
#' @importFrom stringr
#' @importFrom utils read.table data
#' @export
#' @rdname ImportUI


Import <- function(input, output, session, rval) {
  
  # Create temporary variable
  rval_mod <- reactiveValues(cellranger_v = NULL,
                             rds_seu_names = NULL,
                             data_files = NULL,
                             hto_files = NULL,
                             long_hto_names = NULL,
                             adt_files = NULL,
                             rds = NULL,
                             hto_meta = NULL,
                             adt_meta = NULL,
                             proj_meta = NULL,
                             Cells_meta = NULL)
  
  
  ######### Help buttons ############
  
  
  observeEvent(input[["HELP_DATA_IMPORT_MAIN"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_DATA_IMPORT_MAIN"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_DATA_IMPORT_MAIN"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_DATA_IMPORT_RNA"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_DATA_IMPORT_RNA"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_DATA_IMPORT_RNA"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_DATA_IMPORT_HTO"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_DATA_IMPORT_HTO"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_DATA_IMPORT_HTO"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_DATA_IMPORT_ADT"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_DATA_IMPORT_ADT"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_DATA_IMPORT_ADT"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  observeEvent(input[["HELP_IDENTICAL_CELLS"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_IDENTICAL_CELLS"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_IDENTICAL_CELLS"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_DATA_IMPORT_SEURAT"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_DATA_IMPORT_SEURAT"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_DATA_IMPORT_SEURAT"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_DATA_IMPORT_FORMER"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_DATA_IMPORT_FORMER"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_DATA_IMPORT_FORMER"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_DATA_IMPORT_SHIVA"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_DATA_IMPORT_SHIVA"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_DATA_IMPORT_SHIVA"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_CREATE_PROJECT"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_CREATE_PROJECT"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_CREATE_PROJECT"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_CREATE_PROJECT_SEURAT"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_CREATE_PROJECT_SEURAT"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_CREATE_PROJECT_SEURAT"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  ###################################
  
  
  
  
  #### Import flowr rds File #### 
  
  observeEvent(input$flowrfile, {
    
    # Get input
    data <- input$flowrfile
    
    # Get file from input
    file.rename(from = data$datapath, to = paste(dirname(data$datapath[1]),"/", data$name, sep =""))
    data$datapath <- paste(dirname(data$datapath[1]),"/", data$name, sep ="")
    
    # Create modal progress during rds reading
    show_modal_spinner(text = 'Reading rds... Please wait...', spin = 'circle')
    
    # Reads rds file
    rds <- readRDS(data$datapath)
    
    # Close modal progress when rds is read
    remove_modal_spinner()
    
    
    # Check if project names in rds imported are in current session
    if(all(names(rds) %in% names(rval))){
      
      # Create modal progress during project craeting
      show_modal_spinner(text = 'Creating project... Please wait...', spin = 'circle')

      if(dir.exists( rds[["output"]])){
        rval$output <- rds[["output"]]
      }else{
        rval$output <- paste0(getwd(),"/Output/",rds$seurat_selected,"_",format(Sys.time(), "%F_%H:%M:%S"))
        dir.create(rval$output, showWarnings = FALSE)
      }

      # Save all project in rds to output dir
      for(val in setdiff(names(rds),"output")){
        
        if(val == "seurat_list"){
          
          all_seu <- setdiff(names(rds$seurat_list),rds$seurat_selected)
          
          for(seu in all_seu){
            
            rval$seurat_list[[seu]] <- seu
            saveRDS(rds[[val]][[seu]], paste0(rval$output,"/",seu,".rds"))
            
          }
          
          rval$seurat_list[[rds$seurat_selected]] <- rds$seurat_selected
          
        }else{
          
          rval[[val]] <- rds[[val]]
          
          
        }
        
        
      }
      
      rval$seurat@meta.data$Project = names(rval$seurat_list)[1]
      
      saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))
      
      # Close modal progress when project is created
      remove_modal_spinner()
      
    }else{
      showModal(modalDialog(
        title = "Incorrect rds file",
        "Your file doesn't contain a ShIVA object.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
  })
  
  
  
  output$select_folder <- renderUI({
    
    ns <- session$ns
    
    current <- substring(rval$output, regexpr("Output/", rval$output) + 7)
    
    folders <- setdiff(list.files(paste0(getwd(),"/Output/")),current)
    
    if(length(folders) > 0){
      selectInput(ns("folders"),  label = "Choose folder corresponding to project to import (Name_Date_Time)", folders)
      # (Name_yyyy-mm-dd_hh:mm:ss)
      # radioButtons(ns("folders"), label = "Choose folder corresponding to project to import",
      #              folders)
    }
    # else{
    #   showModal(modalDialog(
    #     title = "No folders",
    #     "No projects found. Use another import method.",
    #     easyClose = TRUE,
    #     footer = modalButton("OK")
    #   ))
    # }
    
  })
  
  
  output$content_folder <- renderText({
    
    ns <- session$ns

    if(!is.null(input$folders)){
      files <- list.files(paste0(getwd(),"/Output/",input$folders))
      if(length(files)>0 & "Project_elements.rds" %in% files){
        paste(files, collapse=", " ,sep="")
      }else{
        "No files in this folder."
      }
    }else{
      "No projects found. Use another import method."
    }
    
  })

  
  
  # Add button to save adt metadata when imported
  output$button_folder <- renderUI({
    
    ns <- session$ns
    
    req(input$folders)

    files <- list.files(paste0(getwd(),"/Output/",input$folders))
    if(length(files)>0 & "Project_elements.rds" %in% files){
      actionButton(ns("import_folder"), label = "Import")
    }else{
      NULL
    }
    
  })
  
  
  
  
  observeEvent(input$import_folder, {
  
    ns <- session$ns
    
    req(input$folders)
    
    files <- list.files(paste0(getwd(),"/Output/",input$folders))
  
    if(length(files)>0 & "Project_elements.rds" %in% files){
      
      # Create modal progress during rds reading
      show_modal_spinner(text = 'Reading files... Please wait...', spin = 'circle')
      
      # Reads rds file
      rds <- readRDS(paste0(getwd(),"/Output/",input$folders,"/Project_elements.rds"))
      
      unlink(rval$output, recursive = T)
      
      for(val in setdiff(names(rds), c("tab_elements","menu_elements","modules","output"))){
        rval[[val]] <- rds[[val]]
      }
      
      rval$seurat@meta.data$Project = names(rval$seurat_list)[1]
      
      rval$output <- paste0(getwd(),"/Output/",input$folders)
      
      remove_modal_spinner()

    }else{
      showModal(modalDialog(
        title = "Error",
        "Problem in import, please choose other way to import.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
  })
  
  
  
  
  #### Import Seurat rds File #### 
  
  observeEvent(input$rdsfile, {
    
    data <- input$rdsfile
    
    rval_mod$rds_seu_names <- data$name
    
    file.rename(from = data$datapath, to = paste(dirname(data$datapath[1]),"/", data$name, sep =""))
    data$datapath <- paste(dirname(data$datapath[1]),"/", data$name, sep ="")
    
    # Create modal progress during rds reading
    show_modal_spinner(text = 'Reading rds... Please wait...', spin = 'circle')
    
    rds <- readRDS(data$datapath)
    
    # Close modal progress when rds is read
    remove_modal_spinner()
    
    
    if(class(rds) == "Seurat"){
      if("RNA" %in% Assays(rds)){
        rval_mod$rds <- rds
      }else{
        showModal(modalDialog(
          title = "Incorrect rds file",
          "Your Seurat object doesn't contain RNA assay.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
    }else{
      showModal(modalDialog(
        title = "Incorrect rds file",
        "Your file doesn't contain a Seurat object.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
  })
  
  
  # Create DT based to visualize rval_mod rds values
  
  createDTrds <- reactive({
    
    ns <- session$ns
    
    # Test if rds files are imported
    if(!is.null(rval_mod$rds)){
      
      assays <- Assays(rval_mod$rds)
      features = sapply(rval_mod$rds@assays, nrow)
      cells = sapply(rval_mod$rds@assays, ncol)
      
      # Create dataframe containing information of imported data
      df <- data.frame(Assays = assays, Cellules = cells, 
                       Features = features, row.names = NULL)
      
      return(df)
    }
    
  })
  
  
  # Display DT table containing imported info from rds file
  output$seurat_table <- renderDataTable(
    createDTrds(), escape = FALSE, selection = 'none'
  )

 

  # Modal dialog to inform bad name
  observeEvent(input$load_rds, {
    
    if(input$seu_name_rds == '' || !grepl("[^A-Za-z0-9_]", input$seu_name_rds) == FALSE){
      showModal(modalDialog(
        "Please enter a project name containing only alphanumeric or _ characters.",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    
    # Check if name already exist in current session
    if(input$seu_name_rds %in% names(rval$seurat_list)){
      showModal(modalDialog(
        "This name already exist.",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    validate(
      need(input$seu_name_rds != '', ''),
      need(!input$seu_name_rds %in% names(rval$seurat_list), ''),
      need(!grepl("[^A-Za-z0-9_]", input$seu_name_rds) == TRUE, '')
    )
    
    
    if(!is.null(rval_mod$rds)){
      
      # Create modal progress during rds reading
      show_modal_spinner(text = paste0("Creating ", input$seu_name_rds, "... Please wait..."), spin = 'circle')
      
      # Add information from inpit to current project
      if(input$species_rds == "mouse"){
        rval_mod$rds@meta.data['species'] = "mouse"
        rval_mod$rds@meta.data['percent.mito'] <- PercentageFeatureSet(rval_mod$rds, pattern = "^mt-")
        rval_mod$rds@meta.data['percent.ribo'] <- PercentageFeatureSet(rval_mod$rds, pattern = "^Rp[ls]")
      }else if(input$species_rds == "human"){
        rval_mod$rds@meta.data['species'] = "human"
        rval_mod$rds@meta.data['percent.mito'] <- PercentageFeatureSet(rval_mod$rds, pattern = "^MT-")
        rval_mod$rds@meta.data['percent.ribo'] <- PercentageFeatureSet(rval_mod$rds, pattern = "^RP[LS]")
      }else{
        rval_mod$rds@meta.data['species'] = "other"
      }
      
      # If current project then save it when new is created
      if(!is.null(rval$seurat)){
        withProgress(message = paste0('Saving ', rval$seurat_selected), value = 0.75, {
          saveRDS(rval$seurat, paste0(rval$output,"/",rval$seurat_selected,".rds"))
        })
      }
      
      # Set current project to new created project
      rval$seurat <- rval_mod$rds
      
      # Add name of new project to list of project
      rval$seurat_list[[input$seu_name_rds]] <- input$seu_name_rds
      
      # Set current name of seurat selected from input name
      rval$seurat_selected <- input$seu_name_rds
      
      # Create new list with all genes in the new object
      rval$genes_list[[rval$seurat_selected]][[paste0("All_genes_",rval$seurat_selected)]] = rownames(rval$seurat)
      
      # Add Variable Features to genes list if exist
      if(length(VariableFeatures(rval$seurat)) > 0){
        rval$genes_list[[rval$seurat_selected]][["HVG_seurat_object"]] = VariableFeatures(rval$seurat)
      }
      
      # Add Reductions to parameters list if exist
      if(length(Reductions(rval$seurat)) > 0){
        for(reduc_pca in Reductions(rval$seurat)[str_detect(tolower(Reductions(rval$seurat)), "pca")]){
          rval$parameters[[rval$seurat_selected]][[reduc_pca]] = c("genes" = NA, "npcs" = NA, "regress" = NA)
        }
        for(reduc_tsne in Reductions(rval$seurat)[str_detect(tolower(Reductions(rval$seurat)), "tsne")]){
          rval$parameters[[rval$seurat_selected]][[reduc_tsne]] =  c("pca" = NA, "npcs" = NA, "perp" = NA)
        }
        for(reduc_umap in Reductions(rval$seurat)[str_detect(tolower(Reductions(rval$seurat)), "umap")]){
          rval$parameters[[rval$seurat_selected]][[reduc_umap]] = c("pca" = NA, "npcs" = NA, "neig" = NA)
        }
      }
      
      rval$seurat@meta.data$Project = input$seu_name_rds

      rval$output <- paste0(getwd(),"/Output/",input$seu_name_rds,"_",format(Sys.time(), "%F_%H:%M:%S"))
      # Create output dir when app is launch
      dir.create(rval$output, showWarnings = FALSE)
      
      # Close modal progress when project is created
      remove_modal_spinner()
      
    }
    
  })
  
  
  
  
  #### Import Cellranger output Files ####
  
  observeEvent(input$crfiles, {
    
    validate(
      need(input$crfiles$datapath, "Please select files to import")
    )
    
    rval_mod$rds_seu_names <- NULL
    
    data <- input$crfiles
    
    # Recuperate names of imported files
    names <- data$name
    
    # Test if the 3 files are imported by user
    if(length(names) == 3){
      
      # Create modal progress during import file process
      show_modal_spinner(text = 'Loading files... Please wait...', spin = 'circle')
      
      # Check names of imported files and identify version of cellranger
      if(all(names %in% c("features.tsv.gz", "barcodes.tsv.gz", "matrix.mtx.gz"))){
        
        file.rename(from = data$datapath, to = paste(dirname(data$datapath[1]),"/", data$name, sep =""))
        data$datapath <- paste(dirname(data$datapath[1]),"/", data$name, sep ="")
        
        # Check if files come from cellranger output
        if(ncol(read.delim(paste(dirname(data$datapath[1]),"/features.tsv.gz", sep =""), header = FALSE, stringsAsFactors = FALSE)) < 2){
          
          showModal(modalDialog(
            title = "File error !",
            "Check if your files come from cellranger count",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          
        }else{
          
          # Update reactive object with data
          rval_mod$data_files <-  Read10X(data.dir = dirname(data$datapath[1]))
          rval_mod$cellranger_v <- "v3"
        }
        
        # Close modal progress
        remove_modal_spinner()
        
      }else if(all(names %in% c("genes.tsv", "barcodes.tsv", "matrix.mtx"))){
        
        file.rename(from = data$datapath, to = paste(dirname(data$datapath[1]),"/", data$name, sep =""))
        data$datapath <- paste(dirname(data$datapath[1]),"/", data$name, sep ="")
        rval_mod$data_files <-  Read10X(data.dir = dirname(data$datapath[1]))        
        rval_mod$cellranger_v <- "v2"
        
        # Close modal progress
        remove_modal_spinner()
        
      }else{
        
        showModal(modalDialog(
          title = "Uncorrect files",
          "Files must be named :",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        
      }
      
    }else{
      
      showModal(modalDialog(
        title = "Missing files",
        paste("3 files needed", sep=""),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    }
  })
  
  
  
  
  output$select_hto_adt <- renderUI({
    
    ns <- session$ns
    
    req(rval_mod$data_files)
    
    if(is.list(rval_mod$data_files) & is.null(input$htofiles) & is.null(input$adtfiles)){
      if("Multiplexing Capture" %in% names(rval_mod$data_files)){
        NULL
      }else{
        if("Antibody Capture" %in% names(rval_mod$data_files)){
          selectInput(
            inputId = ns("hto_adt"),
            label = "Antibody Capture correspond to :",
            choices = c("ADT","HTO"),
            selected = 1, 
            width = '25%'
          )
        }
      }
    }
  })
  
  
  observeEvent(input$hto_adt, {
    
    if("Gene Expression" %in% names(rval_mod$data_files)){
      
      if("Multiplexing Capture" %in% names(rval_mod$data_files) & is.null(input$htofiles)){
        
        rval_mod$hto_files <- rval_mod$data_files[["Multiplexing Capture"]]
        rval_mod$long_hto_names <- setdiff(rownames(rval_mod$hto_files),'unmapped')
        
        if("Antibody Capture" %in% names(rval_mod$data_files) & is.null(input$adtfiles)){
          
          rval_mod$adt_files <- rval_mod$data_files[["Antibody Capture"]]
          
        }
      }else if(!is.null(input$hto_adt)){
        
        if(input$hto_adt == "HTO" & is.null(input$adtfiles)){
          
          rval_mod$hto_files <- rval_mod$data_files[["Antibody Capture"]]
          rval_mod$adt_files <- NULL
          
          rval_mod$long_hto_names <- setdiff(rownames(rval_mod$hto_files),'unmapped')
          rownames(rval_mod$hto_files)[which(rownames(rval_mod$hto_files) != "unmapped")] <- paste0(c(1:length(rownames(rval_mod$hto_files)[which(rownames(rval_mod$hto_files) != "unmapped")])),"_",substr(rownames(rval_mod$hto_files)[which(rownames(rval_mod$hto_files) != "unmapped")],1,8))
          
          
        }else if(input$hto_adt == "ADT" & is.null(input$htofiles)){
          
          rval_mod$adt_files <- rval_mod$data_files[["Antibody Capture"]]
          rval_mod$hto_files <- NULL
          
        }
        
      }
    }
  })
  
  

  
  observeEvent(rval_mod$data_files, {
    
    if("Gene Expression" %in% names(rval_mod$data_files)){
      
      if("Multiplexing Capture" %in% names(rval_mod$data_files) & is.null(input$htofiles)){
        
        rval_mod$hto_files <- rval_mod$data_files[["Multiplexing Capture"]]
        rval_mod$long_hto_names <- setdiff(rownames(rval_mod$hto_files),'unmapped')
        
        if("Antibody Capture" %in% names(rval_mod$data_files) & is.null(input$adtfiles)){
          
          rval_mod$adt_files <- rval_mod$data_files[["Antibody Capture"]]
          
        }
      }else if(!is.null(input$hto_adt)){
        
        if(input$hto_adt == "HTO" & is.null(input$adtfiles)){
          
          rval_mod$hto_files <- rval_mod$data_files[["Antibody Capture"]]
          rval_mod$adt_files <- NULL
          
          rval_mod$long_hto_names <- setdiff(rownames(rval_mod$hto_files),'unmapped')
          rownames(rval_mod$hto_files)[which(rownames(rval_mod$hto_files) != "unmapped")] <- paste0(c(1:length(rownames(rval_mod$hto_files)[which(rownames(rval_mod$hto_files) != "unmapped")])),"_",substr(rownames(rval_mod$hto_files)[which(rownames(rval_mod$hto_files) != "unmapped")],1,8))
          
          
        }else if(input$hto_adt == "ADT" & is.null(input$htofiles)){
          
          rval_mod$adt_files <- rval_mod$data_files[["Antibody Capture"]]
          rval_mod$hto_files <- NULL
          
        }
        
      }
    }
  })
  
  observeEvent(rval_mod$data_files, {
    
    if("Gene Expression" %in% names(rval_mod$data_files)){
      
      if("Multiplexing Capture" %in% names(rval_mod$data_files) & is.null(input$htofiles)){
        
        rval_mod$hto_files <- rval_mod$data_files[["Multiplexing Capture"]]
        rval_mod$long_hto_names <- setdiff(rownames(rval_mod$hto_files),'unmapped')
        
        if("Antibody Capture" %in% names(rval_mod$data_files) & is.null(input$adtfiles)){
          
          rval_mod$adt_files <- rval_mod$data_files[["Antibody Capture"]]
          
        }
      }else if(!is.null(input$hto_adt)){
        
        if(input$hto_adt == "HTO" & is.null(input$adtfiles)){
          
          rval_mod$hto_files <- rval_mod$data_files[["Antibody Capture"]]
          rval_mod$adt_files <- NULL
          
          rval_mod$long_hto_names <- setdiff(rownames(rval_mod$hto_files),'unmapped')
          rownames(rval_mod$hto_files)[which(rownames(rval_mod$hto_files) != "unmapped")] <- paste0(c(1:length(rownames(rval_mod$hto_files)[which(rownames(rval_mod$hto_files) != "unmapped")])),"_",substr(rownames(rval_mod$hto_files)[which(rownames(rval_mod$hto_files) != "unmapped")],1,8))
          
          
        }else if(input$hto_adt == "ADT" & is.null(input$htofiles)){
          
          rval_mod$adt_files <- rval_mod$data_files[["Antibody Capture"]]
          rval_mod$hto_files <- NULL
          
        }
        
      }
    }
  })
  
  
  
  
  
  
  #### Import HTO output Files ####
  
  observeEvent(input$htofiles, {
    
    validate(
      need(input$htofiles$datapath, "Please select files to import")
    )
    
    data <- input$htofiles
    
    # Recuperate names of imported files
    names <- data$name
    
    # Test if the 3 files are imported by user
    if(length(names) == 3){
      
      
      # Test name of imported files
      if(all(names %in% c("features.tsv.gz", "barcodes.tsv.gz", "matrix.mtx.gz"))){
        
        # Create modal progress during rds reading
        show_modal_spinner(text = "Loading files... Please wait...", spin = 'circle')
        
        file.rename(from = data$datapath, to = paste(dirname(data$datapath[1]),"/", data$name, sep =""))
        data$datapath <- paste(dirname(data$datapath[1]),"/", data$name, sep ="")
        rval_mod$hto_files <-  Read10X(data.dir = dirname(data$datapath[1]), gene.column = 1)
        
        rval_mod$long_hto_names <- setdiff(rownames(rval_mod$hto_files),"unmapped")
        
        rownames(rval_mod$hto_files)[which(rownames(rval_mod$hto_files) != "unmapped")] <- paste0(c(1:length(rownames(rval_mod$hto_files)[which(rownames(rval_mod$hto_files) != "unmapped")])),"_",substr(rownames(rval_mod$hto_files)[which(rownames(rval_mod$hto_files) != "unmapped")],1,8))
        
        # Close modal progress when files loaded
        remove_modal_spinner()
        
      }else{
        
        showModal(modalDialog(
          title = "Uncorrect files",
          "Files must be named :",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        
      }
      
      
      
    }else{
      showModal(modalDialog(
        title = "Missing files",
        paste("3 files needed", sep=""),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  
  
  
  #### Import ADT output Files ####
  
  observeEvent(input$adtfiles, {
    
    validate(
      need(input$adtfiles$datapath, "Please select files to import")
    )
    
    data <- input$adtfiles
    
    # Recuperate names of imported files
    names <- data$name
    
    if(length(names) == 3){
      
      # Test name of imported files
      if(all(names %in% c("features.tsv.gz", "barcodes.tsv.gz", "matrix.mtx.gz"))){
        
        # Create modal progress during rds reading
        show_modal_spinner(text = "Loading files... Please wait...", spin = 'circle')
        
        file.rename(from = data$datapath, to = paste(dirname(data$datapath[1]),"/", data$name, sep =""))
        data$datapath <- paste(dirname(data$datapath[1]),"/", data$name, sep ="")
        rval_mod$adt_files <-  Read10X(data.dir = dirname(data$datapath[1]), gene.column = 1)
        
        remove_modal_spinner()
        
      }else{
        
        showModal(modalDialog(
          title = "Uncorrect files",
          "Files must be named :",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        
      }
      
    }else{
      
      showModal(modalDialog(
        title = "Missing files",
        paste("3 files needed", sep=""),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    }
  })
  
  
  # Function to add button in DT row
  shinyInput <- function(FUN, len, id, ns, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(ns(id), i), ...))
    }
    inputs
  }
  
  
  # Create DT based to visualize rval_mod values
  
  createDT <- reactive({
    
    ns <- session$ns
    
    # Test if cell ranger files are imported
    if(!is.null(rval_mod$data_files)){
      
      # Test the version of cellranger files
      if(rval_mod$cellranger_v %in% "v2"){
        
        # Create dataframe containing information of imported data
        df <- data.frame(Features_type = "Gene Expression", Name="RNA", Cellules = ncol(rval_mod$data_files), 
                         Features = nrow(rval_mod$data_files), 
                         Edit = "" , Delete = "", 
                         Metadata = shinyInput(actionButton, 1, 'button_', ns = ns, label = "Add cell metadata", onclick = paste0('Shiny.setInputValue(\"' , ns("metadata_Cells"), '\", this.id, {priority: \"event\"})')),
                         row.names = NULL)
        
      }else{
        
        # With cellranger v3 we can have unique or multiple assays. If multiple assays, object is a list, else is a matrix
        if(is.list(rval_mod$data_files)){

          if("Gene Expression" %in% names(rval_mod$data_files)){
            
            # if("Multiplexing Capture" %in% names(rval_mod$data_files) & is.null(input$htofiles)){
            #   
            #   rval_mod$hto_files <- rval_mod$data_files[["Multiplexing Capture"]]
            #   rval_mod$long_hto_names <- setdiff(rownames(rval_mod$hto_files),'unmapped')
            #   
            #   if("Antibody Capture" %in% names(rval_mod$data_files) & is.null(input$adtfiles)){
            #     
            #     rval_mod$adt_files <- rval_mod$data_files[["Antibody Capture"]]
            #     
            #   }
            # }else if(!is.null(input$hto_adt)){
            #   
            #   if(input$hto_adt == "HTO" & is.null(input$adtfiles)){
            #     
            #     rval_mod$hto_files <- rval_mod$data_files[["Antibody Capture"]]
            #     rval_mod$adt_files <- NULL
            #     
            #     rval_mod$long_hto_names <- setdiff(rownames(rval_mod$hto_files),'unmapped')
            #     
            #     
            #   }else if(input$hto_adt == "ADT" & is.null(input$htofiles)){
            #     
            #     rval_mod$adt_files <- rval_mod$data_files[["Antibody Capture"]]
            #     rval_mod$hto_files <- NULL
            #     
            #   }
            #   
            # }

            df <- data.frame(Features_type = "Gene Expression", Name="RNA", 
                             Cellules = ncol(rval_mod$data_files[["Gene Expression"]]), Features = nrow(rval_mod$data_files[["Gene Expression"]]), 
                             Edit = "" , Delete = "", 
                             Metadata = shinyInput(actionButton, 1, 'button_', ns = ns, label = "Add cell metadata", onclick = paste0('Shiny.setInputValue(\"' , ns("metadata_Cells"), '\", this.id, {priority: \"event\"})')),
                             row.names = NULL)
            
          }else{
            
            rval_mod$data_files <- NULL
            
            showModal(modalDialog(
              "Gene Expression feature type was not found in your data",
              size = "s",
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
            
          }
          
          # # Recuperate names in case of multiple assays
          # names_assay <- toupper(word(names(rval_mod$data_files), 1))
          # 
          # # Create dataframe containing information of each assay
          # df <- data.frame(Features_type = names(rval_mod$data_files), Name=names_assay, 
          #                  Cellules = sapply(rval_mod$data_files, ncol), Features = sapply(rval_mod$data_files, nrow), 
          #                  Edit = "" , Delete = "", 
          #                  Metadata = shinyInput(actionButton, 1, 'button_', ns = ns, label = "Add cell metadata", onclick = paste0('Shiny.setInputValue(\"' , ns("metadata_Cells"), '\", this.id, {priority: \"event\"})')),
          #                  row.names = NULL)
          
        }else{
          
          # If only one assay create dataframe with the data
          df <- data.frame(Features_type = "Gene Expression", Name="RNA", 
                           Cellules = ncol(rval_mod$data_files), Features = nrow(rval_mod$data_files), 
                           Edit = "" , Delete = "", 
                           Metadata = shinyInput(actionButton, 1, 'button_', ns = ns, label = "Add cell metadata", onclick = paste0('Shiny.setInputValue(\"' , ns("metadata_Cells"), '\", this.id, {priority: \"event\"})')),
                           row.names = NULL)
        }
      }
      
      # Test if hto files were imported
      if(!is.null(rval_mod$hto_files)){

        # Create dataframe with hto info and add button to view and edit hto names
        df_hto <- data.frame(Features_type = "Cell hashing", Name="HTO", 
                             Cellules = ncol(rval_mod$hto_files), Features = nrow(rval_mod$hto_files[setdiff(rownames(rval_mod$hto_files),"unmapped"),]), 
                             Edit = shinyInput(actionButton, 1, 'button_', ns = ns, label = "Edit HTO names", onclick = paste0('Shiny.setInputValue(\"' , ns("see_HTO"), '\", this.id, {priority: \"event\"})')), 
                             Delete = shinyInput(actionButton, 1, 'button_', ns = ns, label = "Delete HTO sample", onclick = paste0('Shiny.setInputValue(\"' , ns("remove_HTO"), '\", this.id, {priority: \"event\"})')),
                             Metadata = shinyInput(actionButton, 1, 'button_', ns = ns, label = "Add HTO metadata", onclick = paste0('Shiny.setInputValue(\"' , ns("metadata_HTO"), '\", this.id, {priority: \"event\"})')),
                             row.names = NULL)

        # Merge dataframes from cellranger and hto
        df <- rbind(df, df_hto)

      }
      
      # Test if adt files were imported
      if(!is.null(rval_mod$adt_files)){
        
        # Create dataframe with adt info and add button to view and edit adt names
        df_adt <- data.frame(Features_type = "CITE-seq", Name="ADT", 
                             Cellules = ncol(rval_mod$adt_files), Features = nrow(rval_mod$adt_files[setdiff(rownames(rval_mod$adt_files),"unmapped"),]),  
                             Edit = shinyInput(actionButton, 1, 'button_', ns = ns, label = "Edit ADT names", onclick = paste0('Shiny.setInputValue(\"' , ns("see_ADT"), '\", this.id, {priority: \"event\"})')),        
                             Delete = shinyInput(actionButton, 1, 'button_', ns = ns, label = "Delete ADT sample", onclick = paste0('Shiny.setInputValue(\"' , ns("remove_ADT"), '\", this.id, {priority: \"event\"})')),
                             Metadata = shinyInput(actionButton, 1, 'button_', ns = ns, label = "Add ADT metadata", onclick = paste0('Shiny.setInputValue(\"' , ns("metadata_ADT"), '\", this.id, {priority: \"event\"})')),
                             row.names = NULL)
        
        # Merge dataframes from cellranger and adt
        df <- rbind(df, df_adt)
      }
      
      return(df)
      
    }
    
  })
  
  
  
  # Display DT table containing imported info and allow multiple selection
  output$features_table <- renderDataTable(
    createDT(), escape = FALSE, selection = 'none'
  )
  
                                             #@Asif
  #######################################################################
  #call the module test it
  callModule(downServer, id = "downloadSummary", data = createDT, out_file = "FeaturesSummary_")
  #callModule(downReServer, id = "downRe", data = createDT, typ="table", rval=rval)

  #end Asif
##############################################################


  # View HTO names on modal when click in the button in the table
  observeEvent(input$remove_HTO, {
    
    ns <- session$ns
    rval_mod$hto_files <- NULL
    
  })
  
  # View HTO names on modal when click in the button in the table
  observeEvent(input$remove_ADT, {
    
    ns <- session$ns
    rval_mod$adt_files <- NULL
    
  })
  
  
  
  
  
  # Open modal to import metadata link to Cells in csv format
  observeEvent(input$metadata_Cells, {
    
    ns <- session$ns
    
    showModal(modalDialog(
      title = "Import Cells metadata (csv)",
      fileInput(ns("file_Cells_meta"), "Choose CSV File",
                accept = c(".csv")
      ),
      checkboxInput(ns("header_Cells_meta"), "Header", TRUE),
      # Input: Select separator ----
      radioButtons(ns("sep_Cells_meta"), "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons(ns("quote_Cells_meta"), "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      uiOutput(ns("view_col_Cells")),
      div(style = 'overflow-x: scroll', tableOutput(ns("table_meta_Cells"))),
      span(textOutput(ns("warning_nb_Cells")), style="color:red"),
      easyClose = TRUE,
      footer = tagList(
        tags$div(uiOutput(ns("import_meta_Cells")), style = "display:inline-block"),
        tags$div(HTML("<br>"), style = "display:inline-block"),
        tags$div(HTML("<br>"), style = "display:inline-block"),
        tags$div(modalButton("Cancel"), style = "display:inline-block")
      )
    ))
    
  })
  
  
  # Create pick input to choose column to import
  output$view_col_Cells <- renderUI({
    
    ns <- session$ns
    
    req(input$file_Cells_meta)
    
    df <- read.csv(input$file_Cells_meta$datapath,
                   header = input$header_Cells_meta,
                   sep = input$sep_Cells_meta,
                   quote = input$quote_Cells_meta,
                   row.names = 1)
    
    if(!is.null(colnames(df))){
      pickerInput(
        inputId = ns("imported_col_Cells"),
        label = "Columns to import",
        choices = colnames(df),
        selected = colnames(df),
        options = list('actions-box' = TRUE, 'live-search'=TRUE, size = 10),
        multiple = T
      )
    }
    
  })
  
  
  # Display table on the csv imported
  output$table_meta_Cells <- renderTable({
    
    req(input$file_Cells_meta)
    req(input$imported_col_Cells)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file_Cells_meta$datapath,
                       header = input$header_Cells_meta,
                       sep = input$sep_Cells_meta,
                       quote = input$quote_Cells_meta,
                       row.names = 1)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(length(input$imported_col_Cells) > 0){
      if(input$imported_col_Cells[1] %in% colnames(df)){
        return(df[1:10,input$imported_col_Cells,drop=F])
      }
    }
    
    
  }, rownames = T, caption = paste0("View of 10 first lines"))
  
  
  # Add button to save Cells metadata when imported
  output$import_meta_Cells <- renderUI({
    
    ns <- session$ns
    
    req(input$file_Cells_meta)
    req(input$imported_col_Cells)
    req(length(input$imported_col_Cells) > 0)
    
    df <- read.csv(input$file_Cells_meta$datapath,
                   header = input$header_Cells_meta,
                   sep = input$sep_Cells_meta,
                   quote = input$quote_Cells_meta,
                   row.names = 1)
    
    if(is.list(rval_mod$data_files)){
      cells <- substr(colnames(rval_mod$data_files[['Gene Expression']]),1,16)
    }else{
      cells <- substr(colnames(rval_mod$data_files),1,16)
    }
    
    if(length(intersect(rownames(df),cells)) > 10){
      actionButton(ns("but_import_meta_Cells"), label = "Import")
    }
    
    
  })
  
  
  
  
  # Add text message if number of tho in metadata is different than imported
  output$warning_nb_Cells <- renderText({ 
    
    ns <- session$ns
    
    req(input$file_Cells_meta)
    req(input$imported_col_Cells)
    req(length(input$imported_col_Cells) > 0)
    
    df <- read.csv(input$file_Cells_meta$datapath,
                   header = input$header_Cells_meta,
                   sep = input$sep_Cells_meta,
                   quote = input$quote_Cells_meta,
                   row.names = 1)
    
    if(is.list(rval_mod$data_files)){
      cells <- substr(colnames(rval_mod$data_files[['Gene Expression']]),1,16)
    }else{
      cells <- substr(colnames(rval_mod$data_files),1,16)
    }
    
    if(length(intersect(rownames(df),cells)) == 0){
      
      "Error ! Cells name different from imported data."
      
    }else if(length(intersect(rownames(df),cells)) != length(cells)){

      paste0("Warning ! Number of lines in your file (",nrow(df),") is different than number of imported cells.")

    }

    
    
  })
  
  
  
  
  observeEvent(input$but_import_meta_Cells, {
    
    ns <- session$ns
    
    req(input$file_Cells_meta)
    req(input$imported_col_Cells)
    req(length(input$imported_col_Cells) > 0)
    
    df <- read.csv(input$file_Cells_meta$datapath,
                   header = input$header_Cells_meta,
                   sep = input$sep_Cells_meta,
                   quote = input$quote_Cells_meta,
                   row.names = 1)
    
    df <- df[,input$imported_col_Cells,drop=F]
    
    rval_mod$Cells_meta <- df
    
    removeModal()
    
  })
  
  
  
  
  
  # Open modal to import metadata link to adt in csv format
  observeEvent(c(input$meta_proj,input$meta_proj_seu), {
    
    ns <- session$ns
    
    validate(
      need(input$meta_proj_seu[[1]] > 0 || input$meta_proj[[1]] > 0, '')
    )
    
    showModal(modalDialog(
      title = "Import project metadata (csv)",
      fileInput(ns("file_proj_meta"), "Choose CSV File",
                accept = c(".csv")
      ),
      checkboxInput(ns("header_proj_meta"), "Header", TRUE),
      # Input: Select separator ----
      radioButtons(ns("sep_proj_meta"), "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons(ns("quote_proj_meta"), "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      div(style = 'overflow-x: scroll', tableOutput(ns("table_meta_proj"))),
      easyClose = TRUE,
      footer = tagList(
        tags$div(uiOutput(ns("import_meta_proj")), style = "display:inline-block"),
        tags$div(HTML("<br>"), style = "display:inline-block"),
        tags$div(HTML("<br>"), style = "display:inline-block"),
        tags$div(modalButton("Cancel"), style = "display:inline-block")
      )
    ))
    
  })
  
  
  
  # Display table on the csv imported
  output$table_meta_proj <- renderTable({
    
    req(input$file_proj_meta)

    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file_proj_meta$datapath,
                       header = input$header_proj_meta,
                       sep = input$sep_proj_meta,
                       quote = input$quote_proj_meta)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    return(df)
  })
  
  
  # Add button to save proj metadata when imported
  output$import_meta_proj <- renderUI({
    
    ns <- session$ns
    
    req(input$file_proj_meta)
    
    actionButton(ns("but_import_meta_proj"), label = "Import")
    
    
  })
  
  
  
  
  
  observeEvent(input$but_import_meta_proj, {
    
    ns <- session$ns
    
    req(input$file_proj_meta)
    
    
    df <- read.csv(input$file_proj_meta$datapath,
                   header = input$header_proj_meta,
                   sep = input$sep_proj_meta,
                   quote = input$quote_proj_meta)
    
    rval_mod$proj_meta <- df
    
    removeModal()
    
  })
  
  
  
  
  
  
  
  # Open modal to import metadata link to hto in csv format
  observeEvent(input$metadata_HTO, {
    
    ns <- session$ns
    
    showModal(modalDialog(
      title = "Import HTO metadata (csv)",
      fileInput(ns("file_hto_meta"), "Choose CSV File",
                accept = c(".csv")
      ),
      checkboxInput(ns("header_hto_meta"), "Header", TRUE),
      # Input: Select separator ----
      radioButtons(ns("sep_hto_meta"), "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons(ns("quote_hto_meta"), "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      uiOutput(ns("view_col_hto")),
      div(style = 'overflow-x: scroll', tableOutput(ns("table_meta_hto"))),
      span(textOutput(ns("warning_nb_hto")), style="color:red"),
      easyClose = TRUE,
      footer = tagList(
        tags$div(uiOutput(ns("import_meta_hto")), style = "display:inline-block"),
        tags$div(HTML("<br>"), style = "display:inline-block"),
        tags$div(HTML("<br>"), style = "display:inline-block"),
        tags$div(modalButton("Cancel"), style = "display:inline-block")
      )
    ))
    
  })
  
  
  # Create pick input to choose column to import
  output$view_col_hto <- renderUI({
    
    ns <- session$ns

    req(input$file_hto_meta)
    
    df <- read.csv(input$file_hto_meta$datapath,
                   header = input$header_hto_meta,
                   sep = input$sep_hto_meta,
                   quote = input$quote_hto_meta,
                   row.names = 1)
    
    if(!is.null(colnames(df))){
      pickerInput(
        inputId = ns("imported_col_hto"),
        label = "Columns to import",
        choices = colnames(df),
        selected = colnames(df),
        options = list('actions-box' = TRUE, 'live-search'=TRUE, size = 10),
        multiple = T
      )
    }
    
  })
  
  
  # Display table on the csv imported
  output$table_meta_hto <- renderTable({

    req(input$file_hto_meta)
    req(input$imported_col_hto)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file_hto_meta$datapath,
                       header = input$header_hto_meta,
                       sep = input$sep_hto_meta,
                       quote = input$quote_hto_meta,
                       row.names = 1)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(length(input$imported_col_hto) > 0){
      if(input$imported_col_hto[1] %in% colnames(df)){
        return(df[,input$imported_col_hto,drop=F])
      }
    }
    
  }, rownames = TRUE)
  
  
  # Add button to save hto metadata when imported
  output$import_meta_hto <- renderUI({
    
    ns <- session$ns

    req(input$file_hto_meta)
    req(input$imported_col_hto)
    req(length(input$imported_col_hto) > 0)
    
    df <- read.csv(input$file_hto_meta$datapath,
                   header = input$header_hto_meta,
                   sep = input$sep_hto_meta,
                   quote = input$quote_hto_meta,
                   row.names = 1)
    
    
    df <- df[,input$imported_col_hto,drop=F]
    
    req(setequal(rownames(df),setdiff(rownames(rval_mod$hto_files),"unmapped")) == T)

    actionButton(ns("but_import_meta_hto"), label = "Import")

    
  })
  
  
  
  
  # Add text message if number of tho in metadata is different than imported
  output$warning_nb_hto <- renderText({ 
    
    ns <- session$ns
    
    req(input$file_hto_meta)
    req(input$imported_col_hto)
    req(length(input$imported_col_hto) > 0)

    df <- read.csv(input$file_hto_meta$datapath,
                   header = input$header_hto_meta,
                   sep = input$sep_hto_meta,
                   quote = input$quote_hto_meta,
                   row.names = 1)

    if(setequal(rownames(df),setdiff(rownames(rval_mod$hto_files),"unmapped")) == F){
      "Warning ! Row names must be the same than HTO short names."
    }
    
    
  })
  
  
  
  
  observeEvent(input$but_import_meta_hto, {
    
    ns <- session$ns
    
    req(input$file_hto_meta)
    req(input$imported_col_hto)
    req(length(input$imported_col_hto) > 0)

    df <- read.csv(input$file_hto_meta$datapath,
                   header = input$header_hto_meta,
                   sep = input$sep_hto_meta,
                   quote = input$quote_hto_meta,
                   row.names = 1)
    
    df <- df[,input$imported_col_hto,drop=F]
    
    req(setequal(rownames(df),setdiff(rownames(rval_mod$hto_files),"unmapped")) == T)
    
    rval_mod$hto_meta <- df
    
    removeModal()
    
  })
  
  
  
  # View HTO names on modal when click in the button in the table
  observeEvent(input$see_HTO, {
    
    ns <- session$ns

    showModal(modalDialog(
      title = "HTO names (double click on the table to edit)",
      DTOutput(ns("hto_names")),
      uiOutput(ns("button_rename_hto")),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Done")
      )
    ))
    
  })
  
  # Display a DT table containing HTO names and allow edition
  output$hto_names <- renderDT(
    data.frame(HTO_short_names = setdiff(rownames(rval_mod$hto_files),"unmapped")),
    rownames = rval_mod$long_hto_names, editable = TRUE, selection = 'none', caption =  "Max. 10 characters for short names",
    options = list(ordering=F, searching = FALSE, lengthMenu = list(c(10,20), c('10', '20')))
  )
  
  # Update rownames of hto data when table is edited
  proxy_hto <- dataTableProxy('hto_names')
  
  observeEvent(input$hto_names_cell_edit, {
    
    ns <- session$ns
    info = input$hto_names_cell_edit
    value <- info$value
    
    # Check if new value is not empty or same as other hto name
    if(value == "" || value %in% rownames(rval_mod$hto_files)){
      
      if(info$col == 0){
        value <- rval_mod$long_hto_names[info$row]
      }else{
        value <- rownames(rval_mod$hto_files)[info$row]
      }
      
    }
    
    if(info$col == 0){
      rval_mod$long_hto_names[info$row] <<- DT::coerceValue(value, rval_mod$long_hto_names[info$row])
    }else{
      if(nchar(value) > 10){
        value <- substr(value,1,10)
      }
      rownames(rval_mod$hto_files)[info$row] <<- DT::coerceValue(value, rownames(rval_mod$hto_files)[info$row])
    }
    
    replaceData(proxy_hto, setdiff(rownames(rval_mod$hto_files),"unmapped"), rownames = rval_mod$long_hto_names, resetPaging = FALSE)
    
  })
  
  
  # Check if hto names contain barcode sequences
  barcodes_in_hto <- reactive(
    
    return(any(str_detect(c(setdiff(rownames(rval_mod$hto_files),"unmapped"),rval_mod$long_hto_names), ".*[:punct:]+[CGAT]{4,}.*")))
    
  )
  
  
  # Create button to remove barcode if hto contain them
  output$button_rename_hto <- renderUI({
    
    ns <- session$ns
    
    if(barcodes_in_hto() == TRUE){
      actionButton(ns("rename_auto_hto"), "Remove barcodes on HTO names")
    }
    
  })
  
  
  # Remove barcodes on hto names when clicking
  observeEvent(input$rename_auto_hto, {
    
    if(barcodes_in_hto() == TRUE){
      
      rownames(rval_mod$hto_files)[which(rownames(rval_mod$hto_files) != "unmapped")] <- gsub("(.*)([[:punct:]]+[CGAT]{4,})(.*)", paste0("\\1","\\3"), rownames(rval_mod$hto_files)[which(rownames(rval_mod$hto_files) != "unmapped")], perl = T)
      rval_mod$long_hto_names <- gsub("(.*)([[:punct:]]+[CGAT]{4,})(.*)", paste0("\\1","\\3"), rval_mod$long_hto_names , perl = T)      
    }
    
  })
  
  
  
  
  
  # Open modal to import metadata link to adt in csv format
  observeEvent(input$metadata_ADT, {
    
    ns <- session$ns
    
    showModal(modalDialog(
      title = "Import ADT metadata (csv)",
      fileInput(ns("file_adt_meta"), "Choose CSV File",
                accept = c(".csv")
      ),
      checkboxInput(ns("header_adt_meta"), "Header", TRUE),
      # Input: Select separator ----
      radioButtons(ns("sep_adt_meta"), "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons(ns("quote_adt_meta"), "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      uiOutput(ns("view_col_adt")),
      div(style = 'overflow-x: scroll', tableOutput(ns("table_meta_adt"))),
      span(textOutput(ns("warning_nb_adt")), style="color:red"),
      easyClose = TRUE,
      footer = tagList(
        tags$div(uiOutput(ns("import_meta_adt")), style = "display:inline-block"),
        tags$div(HTML("<br>"), style = "display:inline-block"),
        tags$div(HTML("<br>"), style = "display:inline-block"),
        tags$div(modalButton("Cancel"), style = "display:inline-block")
      )
    ))
    
  })
  
  
  # Create pick input to choose column to import
  output$view_col_adt <- renderUI({
    
    ns <- session$ns
    
    req(input$file_adt_meta)
    
    df <- read.csv(input$file_adt_meta$datapath,
                   header = input$header_adt_meta,
                   sep = input$sep_adt_meta,
                   quote = input$quote_adt_meta,
                   row.names = 1)
    
    if(!is.null(colnames(df))){
      pickerInput(
        inputId = ns("imported_col_adt"),
        label = "Columns to import",
        choices = colnames(df),
        selected = colnames(df),
        options = list('actions-box' = TRUE, 'live-search'=TRUE, size = 10),
        multiple = T
      )
    }
    
  })
  
  
  # Display table on the csv imported
  output$table_meta_adt <- renderTable({
    
    req(input$file_adt_meta)
    req(input$imported_col_adt)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file_adt_meta$datapath,
                       header = input$header_adt_meta,
                       sep = input$sep_adt_meta,
                       quote = input$quote_adt_meta,
                       row.names = 1)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(length(input$imported_col_adt) > 0){
      if(input$imported_col_adt[1] %in% colnames(df)){
        return(df[,input$imported_col_adt,drop=F])
      }
    }
    
    
  }, rownames = TRUE)
  
  
  # Add button to save adt metadata when imported
  output$import_meta_adt <- renderUI({
    
    ns <- session$ns
    
    req(input$file_adt_meta)
    req(input$imported_col_adt)
    req(length(input$imported_col_adt) > 0)
    
    actionButton(ns("but_import_meta_adt"), label = "Import")
    
    
  })
  
  
  
  
  # Add text message if number of tho in metadata is different than imported
  output$warning_nb_adt <- renderText({ 
    
    ns <- session$ns
    
    req(input$file_adt_meta)
    req(input$imported_col_adt)
    req(length(input$imported_col_adt) > 0)
    
    df <- read.csv(input$file_adt_meta$datapath,
                   header = input$header_adt_meta,
                   sep = input$sep_adt_meta,
                   quote = input$quote_adt_meta,
                   row.names = 1)
    
    if(nrow(df) != length(rval_mod$long_adt_names)){
      "Warning ! Number of lines in your file is different than number of imported ADT."
    }
    
    
  })
  
  
  
  
  observeEvent(input$but_import_meta_adt, {
    
    ns <- session$ns
    
    req(input$file_adt_meta)
    req(input$imported_col_adt)
    req(length(input$imported_col_adt) > 0)
    
    df <- read.csv(input$file_adt_meta$datapath,
                   header = input$header_adt_meta,
                   sep = input$sep_adt_meta,
                   quote = input$quote_adt_meta,
                   row.names = 1)
    
    df <- df[,input$imported_col_adt,drop=F]
    
    rval_mod$adt_meta <- df
    
    removeModal()
    
  })
  
  
  
  
  
  
  
  # View ADT names on modal when click in the button in the table
  observeEvent(input$see_ADT, {
    
    ns <- session$ns
    
    showModal(modalDialog(
      title = "ADT names (double click on the table to edit)",
      DTOutput(ns("adt_names")),
      uiOutput(ns("button_rename_adt")),
      easyClose = TRUE,
      footer = modalButton("Done")
    ))
    
  })
  
  # Display a DT table containing ADT names and allow edition
  output$adt_names <- renderDT(
    data.frame(ADT = setdiff(rownames(rval_mod$adt_files),"unmapped")), 
    rownames = F, editable = TRUE, selection = 'none', 
    options = list(ordering=F, searching = FALSE, lengthMenu = list(c(10,20), c('10', '20')))
  )
  
  
  # Update rownames of ADT data when table is edited
  proxy_adt <- dataTableProxy('adt_names')
  
  
  observeEvent(input$adt_names_cell_edit, {
    
    ns <- session$ns
    info = input$adt_names_cell_edit
    value <- info$value
    
    # Check if new value is not empty or same as other adt name
    if(value == "" || value %in% rownames(rval_mod$adt_files)){
      value <- rownames(rval_mod$adt_files)[info$row]
    }
    
    rownames(rval_mod$adt_files)[info$row] <<- DT::coerceValue(value, rownames(rval_mod$adt_files)[info$row])
    replaceData(proxy_adt, setdiff(rownames(rval_mod$adt_files),"unmapped"), resetPaging = FALSE)
    
  })
  
  
  
  # Check if adt names contain barcode sequences
  barcodes_in_adt <- reactive(
    
    return(any(str_detect(setdiff(rownames(rval_mod$adt_files),"unmapped"), ".*[:punct:]+[CGAT]{4,}.*")))
    
  )
  
  
  # Create button to remove barcode if adt contain them
  output$button_rename_adt <- renderUI({
    
    ns <- session$ns
    
    if(barcodes_in_adt() == TRUE){
      actionButton(ns("rename_auto_adt"), "Remove barcodes on adt names")
    }
    
  })
  
  
  # Remove barcodes on adt names when clicking
  observeEvent(input$rename_auto_adt, {
    
    if(barcodes_in_adt() == TRUE){
      
      rownames(rval_mod$adt_files)[which(rownames(rval_mod$adt_files) != "unmapped")] <- gsub("(.*)([[:punct:]]+[CGAT]{4,})(.*)", paste0("\\1","\\3"), rownames(rval_mod$adt_files)[which(rownames(rval_mod$adt_files) != "unmapped")], perl = T)
      
    }
    
  })
  
  
  
  
  
  #### Create and set Seurat object from files loaded ####
  
  observeEvent(input$load, {
    
    
    if(input$seu_name == '' || !grepl("[^A-Za-z0-9_]", input$seu_name) == FALSE){
      showModal(modalDialog(
        "Please enter a project name containing only alphanumeric or _ characters.",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    
    if(input$seu_name %in% names(rval$seurat_list)){
      showModal(modalDialog(
        "This name already exist.",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    
    if(is.na(input$min_cells) || input$min_cells<0){
      showModal(modalDialog(
        "Number of cells must be greater than 0.",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    
    if(is.na(input$min_features) || input$min_features<0){
      showModal(modalDialog(
        "Number of features must be greater than 0.",
        size = "s",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    
    # Check if all input are validated
    validate(
      need(input$seu_name != '', ''),
      need(!input$seu_name %in% names(rval$seurat_list), ''),
      need(!is.na(input$min_cells), ''),
      need(input$min_cells>=0, ''),
      need(!is.na(input$min_features), ''),
      need(input$min_features>=0, ''),
      need(!grepl("[^A-Za-z0-9_]", input$seu_name) == TRUE, '')
    )
    
    
    # Create modal progress during project creation
    show_modal_spinner(text = paste0("Creating ", input$seu_name_rds, "... Please wait..."), spin = 'circle')
    
    # Check if file well imported
    if(!is.null(rval_mod$data_files)){
      
      # If file is a list it means multi assays, so check if
      if(is.list(rval_mod$data_files)){
        
        # Create Seurat object with Gene expression data and filtered by input parameters
        seurat_obj <- CreateSeuratObject(counts = rval_mod$data_files[['Gene Expression']], 
                                         project = input$seu_name, 
                                         min.cells = input$min_cells, 
                                         min.features = input$min_features, 
                                         assay = "RNA")
        
        # For other assays than gene expression
        for(assay in setdiff(names(rval_mod$data_files), "Gene Expression")){
          
          # keep the common cells between all the assays
          joint_bcs <- intersect(Cells(seurat_obj), colnames(rval_mod$data_files[[assay]]))
          
          # modify name of assay
          name_assay <- toupper(word(assay, 1))
          
          # Add new assay to seurat object
          seurat_obj[[name_assay]] = CreateAssayObject(counts = as.matrix(rval_mod$data_files[[assay]][,joint_bcs]))
        }
        
      }else{
        
        # If no multiple assay create a Seurat object with gene expression
        seurat_obj <- CreateSeuratObject(counts = rval_mod$data_files, 
                                         project = input$seu_name,
                                         min.cells = input$min_cells,
                                         min.features = input$min_features, 
                                         assay = "RNA")
        
      }
      
      # Check if cells conserved on seurat object (with filtered parameters) is sufficient
      if(is.null(Cells(seurat_obj))){
        
        remove_modal_spinner()
        
        showModal(modalDialog(
          "0 cell conserved. Check your parameters.",
          size = "s",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
      
      
      validate(
        need(!is.null(Cells(seurat_obj)), '') 
      )
      
      
      seurat_obj <- RenameCells(
        object = seurat_obj,
        new.names = substr(Cells(seurat_obj),1,16)
      )
      
      # Get cells of the object
      joint_bcs <- Cells(seurat_obj)
      
      
      if(!is.null(rval_mod$hto_files)){
      
        colnames(rval_mod$hto_files) <- substr(colnames(rval_mod$hto_files),1,16)
          
        # If multiple assay, set cells to common cells between assay
        joint_bcs <- intersect(joint_bcs,colnames(rval_mod$hto_files))
        
      }
      
      
      if(!is.null(rval_mod$adt_files)){
        
        colnames(rval_mod$adt_files) <- substr(colnames(rval_mod$adt_files),1,16)
        
        # If multiple assay, set cells to common cells between assay
        joint_bcs <- intersect(joint_bcs,colnames(rval_mod$adt_files))
        
      }
      
      
      # Check if common cells are sufficient (arbitrary 50)
      if(length(joint_bcs) < 50){
        
        remove_modal_spinner()
        
        showModal(modalDialog(
          "Insufficient cell conserved between RNA and other Features. Check your files or your parameters.",
          size = "s",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
      
      
      validate(
        need(length(joint_bcs) >= 50, '') 
      )      
      
      # Subset the seurat object to keep only common cells
      seurat_obj <- subset(seurat_obj, cells = joint_bcs)
      
      
      # Check if some hto files are imported
      if(!is.null(rval_mod$hto_files)){
        
        # Subset hto matrix imported to keep common cells
        hto_matrix <- as.matrix(rval_mod$hto_files[, joint_bcs])
        hto_matrix <- hto_matrix[setdiff(rownames(hto_matrix),"unmapped"),]
        
        # Create HTO assay with the hto matrix
        seurat_obj[['HTO']] = CreateAssayObject(counts = hto_matrix)
      }
      
      
      # Check if some adt files are imported
      if(!is.null(rval_mod$adt_files)){
        
        # Subset adt matrix imported to keep common cells
        adt_matrix <- as.matrix(rval_mod$adt_files[, joint_bcs])
        adt_matrix <- adt_matrix[setdiff(rownames(adt_matrix),"unmapped"),]
        
        # Create ADT assay with the adt matrix
        seurat_obj[['ADT']] = CreateAssayObject(counts = adt_matrix)
        
        # Normalize ADT data
        seurat_obj <- NormalizeData(seurat_obj, normalization.method = "CLR", margin = 2, assay = "ADT")
        
      }
      
      
      # Check species
      if(input$species == "mouse"){
        
        # Add info to medatada
        seurat_obj@meta.data['species'] = "mouse"
        
        # Calculate mitochondrial percentage based on mouse gene names
        seurat_obj@meta.data['percent.mito'] <- PercentageFeatureSet(seurat_obj, pattern = "^mt-")
        
        # Calculate ribosomal percentage based on mouse gene names
        seurat_obj@meta.data['percent.ribo'] <- PercentageFeatureSet(seurat_obj, pattern = "^Rp[ls]")
        
        # Create listes with s and g2m genes available in seurat package and converted to mouse genes
        rval$genes_list[["imported"]][["s.genes"]] <- cc.genes_mouse$s.genes
        rval$genes_list[["imported"]][["g2m.genes"]] <- cc.genes_mouse$g2m.genes
        
      }else if(input$species == "human"){
        
        # Add info to medatada
        seurat_obj@meta.data['species'] = "human"
        
        # Calculate mitochondrial percentage based on human gene names
        seurat_obj@meta.data['percent.mito'] <- PercentageFeatureSet(seurat_obj, pattern = "^MT-")
        
        # Calculate ribosomal percentage based on human gene names
        seurat_obj@meta.data['percent.ribo'] <- PercentageFeatureSet(seurat_obj, pattern = "^RP[LS]")
        
        # Create listes with s and g2m genes available in seurat package
        rval$genes_list[["imported"]][["s.genes"]] <- cc.genes.updated.2019$s.genes
        rval$genes_list[["imported"]][["g2m.genes"]] <- cc.genes.updated.2019$g2m.genes
        
      }else{
        
        # Add info to medatada
        seurat_obj@meta.data['species'] = "other"
        
        # Create listes with s and g2m genes available in seurat package
        rval$genes_list[["imported"]][["s.genes"]] <- cc.genes.updated.2019$s.genes
        rval$genes_list[["imported"]][["g2m.genes"]] <- cc.genes.updated.2019$g2m.genes
        
      }
      
      
      # When objet is created check if other is loaded then save it
      if(!is.null(rval$seurat)){
        withProgress(message = paste0('Saving ', rval$seurat_selected), value = 0.75, {
          saveRDS(rval$seurat, paste0(rval$output,"/",rval$seurat_selected,".rds"))
        })
      }
      
      # Set current project with new seurat object created
      rval$seurat <- seurat_obj
      
      rval$seurat@meta.data$Project = input$seu_name_rds
      
      
      if(!is.null(input$norm_method)){
        # Normalize RNA assay of created seurat
        if(input$norm_method == "SCT"){
          rval$seurat <- SCTransform(rval$seurat,
                                     assay = "RNA",
                                     new.assay.name = "SCT",
                                     verbose = TRUE,
                                     return.only.var.genes = FALSE)
          rval$norm_assay <- "SCT"
          rval$genes_list[[input$seu_name]][[paste(input$seu_name, "Variable_Genes_SCT", length(VariableFeatures(rval$seurat, assay = "SCT")), sep = "_")]] = VariableFeatures(rval$seurat, assay = "SCT")
        }
        rval$seurat <- NormalizeData(rval$seurat,
                                      normalization.method = "LogNormalize", 
                                      assay = "RNA",
                                      scale.factor = 10000, 
                                      margin = 1, 
                                      verbose = TRUE)
      }else{
        rval$seurat <- NormalizeData(rval$seurat,
                                     normalization.method = "LogNormalize", 
                                     assay = "RNA",
                                     scale.factor = 10000, 
                                     margin = 1, 
                                     verbose = TRUE)
      }


      # Add name of created project to list of project names
      rval$seurat_list[[input$seu_name]] <- input$seu_name
      
      # Set selected object
      rval$seurat_selected <- input$seu_name
      
      # Create new list with all genes in the new object
      rval$genes_list[[rval$seurat_selected]][[paste0("All_genes_",rval$seurat_selected)]] = rownames(rval$seurat)
      
      rval$output <- paste0(getwd(),"/Output/",input$seu_name,"_",format(Sys.time(), "%F_%H:%M:%S"))
      # Create output dir when app is launch
      dir.create(rval$output, showWarnings = FALSE)
      
      # Add type of import to parameters
      rval$parameters[[rval$seurat_selected]][["type_import"]] = input$type_import
      
      rval$parameters[[rval$seurat_selected]][["min_cells"]] = input$min_cells
      rval$parameters[[rval$seurat_selected]][["min_features"]] = input$min_features
      
      rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "import")
      
      # Add potential imported hto metadata
      rval$hto_meta <- rval_mod$hto_meta
      
      # Add potential imported adt metadata
      rval$adt_meta <- rval_mod$adt_meta
      
      # Add potential imported project metadata
      rval$project_meta <- rval_mod$proj_meta

      if(!is.null(rval_mod$Cells_meta)){
        
        rownames(rval_mod$Cells_meta) <- str_replace_all(rownames(rval_mod$Cells_meta), pattern = "_", replacement = "-")
        
        for(col in colnames(rval_mod$Cells_meta)){
          
          if(col %in% colnames(rval$seurat@meta.data)){
            colname <- paste0(col,"_imported")
          }else{
            colname <- col
          }
          
          rval$seurat@meta.data[,colname] = "NA"

          rval$seurat@meta.data[,colname] = rval_mod$Cells_meta[rownames(rval$seurat@meta.data),col]

          if(is.character(rval$seurat@meta.data[,colname])){
            rval$seurat@meta.data[,colname] <- as.factor(rval$seurat@meta.data[,colname])
          }
        }
        
      }

      remove_modal_spinner()
      
    }
    
  })
  
  
  # To create list of cells by imported files
  cells_by_samp <- reactive({
    
    # Create list
    list_cells <- list()
    
    # Check if files are importated
    if(!is.null(rval_mod$data_files)){
      
      # Add cells from "RNA" to the list
      if(is.list(rval_mod$data_files)){
        list_cells[["RNA"]] <- substr(colnames(rval_mod$data_files[['Gene Expression']]),1,16)
      }else{
        list_cells[["RNA"]] <- substr(colnames(rval_mod$data_files),1,16)
      }
      
    }
    
    
    # If HTO add cells from this file to list
    if(!is.null(rval_mod$hto_files)){
      list_cells[["HTO"]] <- substr(colnames(rval_mod$hto_files),1,16)
    }
    
    # If ADT add cells from this file to list
    if(!is.null(rval_mod$adt_files)){
      list_cells[["ADT"]] <- substr(colnames(rval_mod$adt_files),1,16)
    }
    
    return(list_cells)
    
  })
  
  
  # Plot venn diagram with list of cells
  output$venn_cells <- renderPlot(
    venn(cells_by_samp())
  )
  
  
  # Check if cellranger files imported
  output$crfilesUploaded <- reactive({
    return(!is.null(rval_mod$data_files))
  })
  
  outputOptions(output, 'crfilesUploaded', suspendWhenHidden=FALSE)
  
  
  # Check if hto files imported
  output$htofilesUploaded <- reactive({
    return(!is.null(rval_mod$hto_files))
  })
  
  outputOptions(output, 'htofilesUploaded', suspendWhenHidden=FALSE)
  
  
  # Check if adt files imported
  output$adtfilesUploaded <- reactive({
    return(!is.null(rval_mod$adt_files))
  })
  
  outputOptions(output, 'adtfilesUploaded', suspendWhenHidden=FALSE)
  
  
  # Check if rds files imported
  output$rdsfilesUploaded <- reactive({
    return(!is.null(rval_mod$rds))
  })
  
  outputOptions(output, 'rdsfilesUploaded', suspendWhenHidden=FALSE)
  
  
  # Add title to the tab
  output$import_title <- renderText({
    "Import data to create a new project"
  })
  
  return(rval)
}
