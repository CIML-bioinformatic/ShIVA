Gene_listUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(width = 3, 
             
            box(title = p("Import Gene List", actionButton(ns("HELP_LIST_IMPORT"), "", icon = icon("question-circle"), class = "btn-xs")),
                height = NULL, solidHeader = TRUE, status = "primary",
                collapsible = F, width = NULL,
                fileInput(ns("file1"), "Choose Input File to Upload",
                          accept = c(".csv")
                ),
                checkboxInput(ns("header"), "First Row Defines Headers", TRUE),
                # Input: Select separator ----
                radioButtons(ns("sep"), "Choose Columns Separator",
                             choices = c(Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t"),
                             selected = ","),
                
                # Input: Select quotes ----
                radioButtons(ns("quote"), "Choose Quote Character",
                             choices = c(None = "",
                                         "Double Quote (\"gene\")" = '"',
                                         "Single Quote ('gene')" = "'"),
                             selected = '"'),
                uiOutput(ns("view_import")),
                textOutput(ns("nb_imported_genes")),
                br(),
                # uiOutput(ns("check_convert")),
                uiOutput(ns("import_new_list"))
            )

             
      ),
      
      column(width = 6,

             box(title = p("Edit Lists", actionButton(ns("HELP_LIST_EDIT"), "", icon = icon("question-circle"), class = "btn-xs")),
                 height = NULL, solidHeader = TRUE,
                 status = "primary", width = NULL,
                 uiOutput(ns("select_lists")),
                 uiOutput(ns("radio_merge")),
                 uiOutput(ns("select_genes_pick")),
                 textOutput(ns("nb_select_genes")),
                 br(),
                 uiOutput(ns("create_new_list"))
             )
               

      ),
      
      column(width = 3,
             
             box(title = p("Compute Signature Scores from List", actionButton(ns("HELP_LIST_SIGNATURE"), "", icon = icon("question-circle"), class = "btn-xs")),
                 height = NULL, solidHeader = TRUE,
                 status = "primary", width = NULL,
                 radioButtons(ns("type_score"), "", 
                              c("Signature Score" = "module", "Cell Cycle Score" = "cycle"),
                              inline = T),
                 uiOutput(ns("select_lists_score")),
                 uiOutput(ns("input_name_score")),
                 uiOutput(ns("select_cycle_s")),
                 uiOutput(ns("select_cycle_g")),
                 uiOutput(ns("calculate_score"))

             )
      )
      
    )
  )
}


Gene_list <- function(input, output, session, rval) {
  
  
  # Create variable for mpdule
  params <- reactiveValues(import_genes = NULL,
                           genes = NULL)
  
  
  
  ######### Help buttons ############
  
  
  observeEvent(input[["HELP_LIST_IMPORT"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_LIST_IMPORT"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_LIST_IMPORT"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_LIST_EDIT"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_LIST_EDIT"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_LIST_EDIT"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  observeEvent(input[["HELP_LIST_SIGNATURE"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_LIST_SIGNATURE"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_LIST_SIGNATURE"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  output$view_import <- renderUI({
    
    ns <- session$ns
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(file = input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    params$import_genes <- as.character(df[,1])
    
    if(!is.null(params$import_genes)){
      pickerInput(
        inputId = ns("imported_genes"),
        label = "Imported genes",
        choices = params$import_genes,
        selected = params$import_genes,
        options = list('actions-box' = TRUE, 'live-search'=TRUE, size = 10),
        multiple = T
      )
    }
    
  })
  
  
  
  output$check_convert <- renderUI({
    
    ns <- session$ns
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    if(!is.null(input$imported_genes)){
      radioButtons(ns("convert"), "Convert",
                   choices = c("None" = "none",
                               "To mouse" = "mouse",
                               "To human" = "human"),
                   selected = "none")
    }
    
  })
  
  
  
  output$nb_imported_genes <- renderText(
    
    if(!is.null(input$imported_genes)){
      
      paste0(length(input$imported_genes), " genes in your selection")
      
    }
    
  )
  
  
  
  
  
  output$import_new_list <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(params$import_genes) & !is.null(input$file1) & !is.null(input$imported_genes)){
      
        actionButton(ns("import"), label = "Import list")
        
    }
    
  })
  
  
  nameImportModal <- function(failed = FALSE, ngenes = 0, ns = NULL) {
    
    modalDialog(
      paste0("You will create a new list containing ", ngenes, " genes."),
      size = "m",
      easyClose = FALSE,
      HTML("<br><br>"),
      textInput(ns("import_name_list"), "Enter new list name", value = gsub(pattern = "\\.csv$", "", input$file1$name)),
      
      if (failed)
        div(tags$b("List name already exist or is invalid (only alphanumeric and _ characters)", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("import_list_ok"), "OK")
      )
    )
  }
  
  
  
  observeEvent(input$import_list_ok, {
    
    ns <- session$ns
    
    exist = F

    if(length(rval$genes_list) > 0){
      
      for(l in 1:length(rval$genes_list)){
        
        if(input$import_name_list %in% names(rval$genes_list[[l]])) {
          
          exist = T
          
        }
        
      }
    }

    if(exist == T){
      
      ngenes <- length(input$imported_genes)
      
      showModal(nameImportModal(failed = TRUE, ngenes = ngenes, ns = ns))
      
    }else{
      
            # Check that data object exists and is data frame.
      if (input$import_name_list != '' && !grepl("[^A-Za-z0-9_]", input$import_name_list) == TRUE){
        
        removeModal()
        
        genes <- input$imported_genes

        # if(!is.null(input$convert) & input$convert != "none"){
        # 
        #   show_modal_spinner(text = 'Converting genes... Please wait...', spin = 'circle')
        # 
        #   if(input$convert == "mouse"){
        # 
        #     genes <- convertHumanGeneList(genes)
        # 
        #   }else if(input$convert == "human"){
        # 
        #     genes <- convertMouseGeneList(genes)
        # 
        #   }
        # 
        #   remove_modal_spinner()
        # 
        # }

        
        rval$genes_list[["imported"]][[input$import_name_list]] = genes
        
        showModal(
          modalDialog(
            paste0("List ", input$import_name_list, " imported with ", length(genes), " genes."),
            size = "m",
            easyClose = TRUE,
            footer = tagList(
              modalButton("OK")
            )
          )
        )
        
        
      } else {
        
        ngenes <- length(input$imported_genes)
        
        showModal(imported_genes(failed = TRUE, ngenes = ngenes, ns = ns))
        
      }
      
    }
    
  })
  
  
  
  
  
  
  
  
  observeEvent(input$import, {
    
    ns <- session$ns
    
    if(!is.null(params$import_genes) & !is.null(input$imported_genes)){
      
      ngenes <- length(input$imported_genes)
      
      if(ngenes > 0){
        showModal(nameImportModal(ngenes = ngenes, ns = ns))
      }else {
        showModal(
          modalDialog(
            "No sufficient genes",
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
  
  
  # Get all the lists for the current project
  getLists <- reactive({
    
    if(!is.null(rval$seurat)){
      
      if(!(paste0("All_genes_",rval$seurat_selected) %in% names(rval$genes_list[[rval$seurat_selected]]))){
        
        rval$genes_list[[rval$seurat_selected]][[paste0("All_genes_",rval$seurat_selected)]] = rownames(rval$seurat)
        
      }

    }
    

    if(length(rval$genes_list) > 0){
      
      lists <- NULL
      
      for(i in 1:length(rval$genes_list)){
        
        lists <- c(lists, names(rval$genes_list[[i]]))
        
      }
      
      return(lists)
      
    }
    
  })
  
  
  # Create selection input to select available lists
  output$select_lists<- renderUI({
    
    ns <- session$ns
    
    selectizeInput(ns("selectList"), "Choose One or Several Lists", getLists(), selected = NULL, multiple = T,
                   options = NULL)
    
  }) 
  
  
  
  
  
  # Get the number of selected list
  nbLists <- reactive({
    
    if(!is.null(input$selectList)){
        
      return(length(input$selectList))
      
    }
    
  })
  
  
  # Create radio button if multiple lists are selected
  output$radio_merge<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(nbLists())){
      
      if(nbLists() > 2){
        
        radioButtons(ns("merge"), "Type of fusion", 
                     c("Union" = "union", "Intersection" = "intersect"),
                     inline = T)
        
      }else if(nbLists() == 2){
        
        radioButtons(ns("merge"), "Type of fusion", 
                     c("Union" = "union", "Intersection" = "intersect", "Complement" = "complement"),
                     inline = T)
      }
      
    }
    
  }) 
  
  
  

  # Update values when click on button
  observeEvent(c(input$selectList,input$merge), {

    validate(
      need(!is.null(nbLists()), '')
    )
      
    if(nbLists() >= 2){
      
      
      validate(
        need(!is.null(input$merge), '')
      )
      
      genes <- NULL
      
      
      for(l in 1:length(rval$genes_list)){
        
        if(input$selectList[1] %in% names(rval$genes_list[[l]])){
      
          genes <- rval$genes_list[[l]][[input$selectList[1]]]
          
        }
        
      }
      
      
      for(i in 2:nbLists()){
        
        for(l in 1:length(rval$genes_list)){
          
          if(input$selectList[i] %in% names(rval$genes_list[[l]])){
            
            if(input$merge %in% "union"){
              
              genes <- union(genes, rval$genes_list[[l]][[input$selectList[i]]])
              
            }else if(input$merge %in% "intersect"){

              genes <- intersect(genes, rval$genes_list[[l]][[input$selectList[i]]])
              
            }else if(input$merge %in% "complement"){
              
              genes <- setdiff(genes, rval$genes_list[[l]][[input$selectList[i]]])
              
            }
            
          }
          
        }
        
        
      }
      
    }else{
      
      for(i in 1:length(rval$genes_list)){
        
        if(input$selectList %in% names(rval$genes_list[[i]])){
          
          genes <- rval$genes_list[[i]][[input$selectList]]

        }
        
      }

    }
      
    params$genes <- sort(genes)

  })
  
  
  
  
  
  output$select_genes_pick <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(params$genes) && !is.null(nbLists())){
      
      pickerInput(
        inputId = ns("select_genes"),
        label = "List of Genes",
        choices = params$genes,
        selected = params$genes,
        options = list('actions-box' = TRUE, 'live-search'=TRUE, size = 10),
        multiple = T
      )
      
    }
  
  })
  
  
  
  output$nb_select_genes <- renderText(
    
    if(!is.null(input$select_genes) & !is.null(nbLists())){

      paste0(length(input$select_genes), " genes in your selection")
      
    }

  )
  

  
  
  output$create_new_list <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(params$genes) & !is.null(input$select_genes) & !is.null(nbLists())){

      if((length(intersect(params$genes, input$select_genes)) != length(params$genes)) | nbLists() >= 2){
        
        actionButton(ns("new_list"), label = "Save selection as new list")
        
      }
    }
    
  })
  
  
  
  nameListModal <- function(failed = FALSE, ngenes = 0, ns = NULL) {
    
    modalDialog(
      paste0("You will create a new list containing ", ngenes, " genes."),
      size = "m",
      easyClose = FALSE,
      HTML("<br><br>"),
      textInput(ns("name_list"), "Enter new list name", value = ""),
      
      if (failed)
        div(tags$b("List name already exist or is invalid (only alphanumeric and _ characters)", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("name_list_ok"), "OK")
      )
    )
  }
  
  
  
  
  observeEvent(input$new_list, {
      
      ns <- session$ns
      
      if(!is.null(params$genes) & !is.null(input$select_genes) & !is.null(nbLists())){
        
        ngenes <- length(input$select_genes)
        
        if(ngenes > 0){
          showModal(nameListModal(ngenes = ngenes, ns = ns))
        }else {
          showModal(
            modalDialog(
              "No sufficient genes",
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
  
  
  
  
  
  observeEvent(input$name_list_ok, {
    
    ns <- session$ns
    
    exist = F
    
    for(l in 1:length(rval$genes_list)){
      
      if(input$name_list %in% names(rval$genes_list[[l]])) {
        
        exist = T
        
      }
      
    }
    
    
    
    if(exist == T){
      
      ngenes <- length(input$select_genes)
      
      showModal(nameListModal(failed = TRUE, ngenes = ngenes, ns = ns))
      
    }else{
      
      # Check that data object exists and is data frame.
      if (input$name_list != '' && !grepl("[^A-Za-z0-9_]", input$name_list) == TRUE){
        
        removeModal()
        
        rval$genes_list[["imported"]][[input$name_list]] = input$select_genes
        
        showModal(
          modalDialog(
            paste0("List ", input$name_list, " created"),
            size = "m",
            easyClose = TRUE,
            footer = tagList(
              modalButton("OK")
            )
          )
        )
        
        
      } else {
        
        ngenes <- length(input$select_genes)
        
        showModal(nameListModal(failed = TRUE, ngenes = ngenes, ns = ns))
        
      }
      
    }
    
  })
  
  
  
  
  
  # Create selection input to select available lists
  output$select_lists_score <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_score)){
      
      if(input$type_score == "module"){
        
        selectInput(ns("selectListScore"), "Choose a Gene List", getLists(), selected = tail(getLists(),1), multiple = F, selectize = F)
        
      }
    }
    
  }) 
  
  
  
  
  # Create selection input to select available lists
  output$input_name_score <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_score)){
      
      if(input$type_score == "module"){
        
        textInput(ns("name_score"), "Enter a name for the score", value = "")
        
      }
    }
    
  }) 
  
  
  
  
  output$select_cycle_s <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_score)){
      
      if(input$type_score == "cycle"){
        
        if("s.genes" %in% getLists()){
          selection <- "s.genes"
        }else{
          selection <- NULL
        }
        
        selectInput(ns("selectListS"), "Choose Gene List for S Phase", getLists(), selected = selection, multiple = F, selectize = F)
        
      }
    }
    
  }) 
  
  
  
  output$select_cycle_g <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_score)){
      
      if(input$type_score == "cycle"){
        
        if("g2m.genes" %in% getLists()){
          selection <- "g2m.genes"
        }else{
          selection <- NULL
        }
        
        selectInput(ns("selectListG"), "Choose Gene List for G2M Phase", getLists(), selected = selection, multiple = F, selectize = F)
        
      }
    }
    
  }) 
  
  
  
  
  output$calculate_score <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_score)){

      if(input$type_score == "module" & !is.null(input$selectListScore)){
       
        actionButton(ns("new_score"), label = "Compute Score")
        
      }else if(input$type_score == "cycle" & !is.null(input$selectListS) & !is.null(input$selectListG)){
    
        actionButton(ns("new_cycle"), label = "Compute Score")
    
      }
      
    }
    
  })
  
  
  

  
  
  observeEvent(input$new_score, {
    
    ns <- session$ns
    
    # Check that data object exists and is data frame.
    if (input$name_score != '' && !grepl("[^A-Za-z0-9_]", input$name_score) == TRUE){
      
      for(l in 1:length(rval$genes_list)){
        
        if(input$selectListScore %in% names(rval$genes_list[[l]])){
          
          genes <- rval$genes_list[[l]][[input$selectListScore]]
          
        }
        
      }
      
      
      
      if(length(intersect(genes, rownames(rval$seurat))) > 0){
        
        genes <- list(genes)
        
        show_modal_spinner(text = 'Calculating score... Please wait...', spin = 'circle')
        
        rval$seurat <- AddModuleScore(rval$seurat, features = genes, assay =  Assays(rval$seurat)[1], name = input$name_score)
        
        rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "score")
        
        remove_modal_spinner()
        
        
      }else{
        
        showModal(
          modalDialog(
            div(tags$b("Features in your list are not present in the object", style = "color: red;")),
            size = "m",
            easyClose = TRUE,
            footer = tagList(
              modalButton("OK")
            )
          )
        )
        
      }
      
      
    } else {
      
      showModal(
        modalDialog(
          div(tags$b("Name is invalid (only alphanumeric and _ characters)", style = "color: red;")),
          size = "m",
          easyClose = TRUE,
          footer = tagList(
            modalButton("OK")
          )
        )
      )
      
    }
    
  })
  
  
  
  
  
  
  
  observeEvent(input$new_cycle, {
    
    ns <- session$ns
    
    # Check that data object exists and is data frame.
    if (input$selectListS != input$selectListG){
      
      for(l in 1:length(rval$genes_list)){
        
        if(input$selectListS %in% names(rval$genes_list[[l]])){
          
          s.genes <- rval$genes_list[[l]][[input$selectListS]]
          
        }
        
        if(input$selectListG %in% names(rval$genes_list[[l]])){
          
          g2m.genes <- rval$genes_list[[l]][[input$selectListG]]
          
        }
        
      }
      
      
      
      if(length(intersect(s.genes, rownames(rval$seurat))) > 0 & length(intersect(g2m.genes, rownames(rval$seurat))) > 0){
        
        show_modal_spinner(text = 'Calculating score... Please wait...', spin = 'circle')
        
        rval$seurat <- CellCycleScoring(rval$seurat, s.features = s.genes, g2m.features = g2m.genes)
        
        rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "cycle")
        
        remove_modal_spinner()
        
        
      }else{
        
        showModal(
          modalDialog(
            div(tags$b("Features in your list are not present in the object", style = "color: red;")),
            size = "m",
            easyClose = TRUE,
            footer = tagList(
              modalButton("OK")
            )
          )
        )
        
      }
      
      
    } else {
      
      showModal(
        modalDialog(
          div(tags$b("Please select two different lists", style = "color: red;")),
          size = "m",
          easyClose = TRUE,
          footer = tagList(
            modalButton("OK")
          )
        )
      )
      
    }
    
  })
  
  
  
  
  return(rval)
  
  
}