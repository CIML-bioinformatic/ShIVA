
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

server <- function(session, input, output, modules = NULL) {
  
  # Delete output dir when app is closed
  # onStop(function() {
  #   gc()
  #   unlink(paste0(getwd(),"/Output"), recursive = T)
  # })
  
  
  
  ### Definition of the main reactivevalues object #############################################
  
  rval <- reactiveValues(number_s = 0, # number of project created
                         seurat = NULL, # current project (seurat object)
                         seurat_list = list(), # names of all project
                         seurat_selected = NULL, # name of selected project
                         parameters = list(), # parameters for each projected
                         genes_list = list(), # list of genes created
                         cell_list = list(), # list of cells created
                         project_meta = NULL,
                         hto_meta = NULL,
                         adt_meta = NULL,
                         norm_assay = "RNA",
                         tab_elements = list(), 
                         menu_elements = list(),
                         modules = NULL,
                         output = NULL
  )
  
  
  # observe({
  #   if(is.null(rval$output)){
  #     rval$output <- paste0(getwd(),"/Output/",format(Sys.time(), "%Y%a%b%d_%H%M%S"))
  #     # Create output dir when app is launch
  #     dir.create(rval$output, showWarnings = FALSE)
  #   }
  #  })
  
  
  
  ### Build UI based on selected modules #######################################################
  
  observe({
    default_modules <- NULL
    # default_modules <- c("Import", "Demultiplexing","Quality_Control","Variable_Genes","PCA","TSNE_UMAP",
    #                      "Clustering","Differential_Expression","Exploration","Subsetting","Gene_list","Export")
    default_modules <- c("Data Import" = "Import", "HTO Demultiplexing" = "Demultiplexing" , "Quality Control" = "Quality_Control" , 
                         "Gene Selection" = "Variable_Genes" , "PCA and Regression" = "PCA" , "t-SNE/UMAP Embedding" = "TSNE_UMAP",
                         "Clustering and Markers" = "Clustering" ,"Differential Expression" = "Differential_Expression" ,
                         "Interactive Exploration" = "Exploration" , "Gene Lists"="Gene_list" , "Subproject Design" = "Subsetting",
                         "Data Export"="Export")
    if(is.null(modules)){
      rval$modules <- default_modules
    }else{
      rval$modules <- modules
    }
    
    
  })
  
  # observe({
  #   rval$modules <- union(rval$modules, "Modules")
  # })
  # 
  
  # select first module loaded
  observe({
    validate(need(rval$modules, "No tab elements available"))
    tab_selected <- rval$modules[1]
    tab_selected <- paste(tab_selected, "tab", sep="_")
    updateTabItems(inputId = "sidebar_tabs", selected = tab_selected, session = session)
  })
  
  
  output$body <- renderUI({
    
    if(all(rval$modules %in% names(rval$tab_elements))){
      tagList(
        h3(textOutput("seurat_name")),
        fluidPage(
          fluidRow(
            valueBoxOutput("progressBox", width = 3),
            valueBoxOutput("progressBox2", width = 3),
            valueBoxOutput("progressBox3", width = 3),
            valueBoxOutput("progressBox4", width = 3)
          ),
          do.call(tabItems, unname(rval$tab_elements[rval$modules]))
        )
      )
    }else{
      tagList(list())
    }
    
  }) 
  
  
  
  output$menu <- renderMenu({
    
    if(all(rval$modules %in% names(rval$menu_elements))){
      sidebarMenu(id = "menu", 
                  
                  selectInput("select_seu", "Select project",
                              choices = NULL,
                              selected = NULL),
                  tagList(rval$menu_elements[rval$modules]),
                  br(),
                  br(),
                  # uiOutput("list_seu"),
                  div(style = 'padding-left: 1.2em;',uiOutput("buttonDL")),
                  br(),
                  div(style = 'padding-left: 1.2em;',uiOutput("but_repo"))
      )
    }else{
      NULL
    }
  })
  
  
  # Create button to remove barcode if hto contain them
  output$buttonDL <- renderUI({
    
    
    if(!is.null(rval$seurat_selected)){
      downloadButton("downloadData", "Download ShIVA Project", class = "btn-danger")
    }
    
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(rval$seurat_selected, ".rds", sep = "")
    },
    content = function(file) {
      
      # Create modal progress during rds creation and saving
      show_modal_spinner(text = 'Creating rds file... Please wait...', spin = 'circle')
      
      all_seu <- setdiff(names(rval$seurat_list),rval$seurat_selected)
      
      for(seu in all_seu){
        rval$seurat_list[[seu]] <- readRDS(paste0(rval$output,"/",seu,".rds"))
      }
      
      saveRDS(reactiveValuesToList(rval), file)
      
      # Close modal progress when rds is saved
      remove_modal_spinner()
      
    }
  )
  
  
  observeEvent(rval$modules, {
    
    modules_to_update <- setdiff(names(rval$modules), names(rval$menu_elements))
    
    for( mod_name in modules_to_update ){
      
      mod_name_ui <- paste(rval$modules[mod_name], "UI", sep="")
      
      module_server_function <-function(...){do.call(rval$modules[mod_name], list(...) )}
      module_id <- paste(rval$modules[mod_name], "module", sep="_")
      module_tab_name <- paste(rval$modules[mod_name], "tab", sep="_")

      rval <- callModule(module_server_function,
                         id = module_id,
                         rval = rval)
      
      rval$tab_elements[[rval$modules[mod_name]]] <- tabItem(tabName = module_tab_name,
                                               do.call(mod_name_ui,
                                                       list(id = module_id)))
      
      rval$menu_elements[[rval$modules[mod_name]]] <- menuItem(mod_name,
                                                 selected = TRUE,
                                                 tabName = module_tab_name, 
                                                 startExpanded = FALSE,
                                                 icon = icon("check-circle"))
    }
    
    
  })
  
  
  # If demultiplexing test if HTO
  observeEvent(input$menu, {
    req(input$menu == "Demultiplexing_tab")
    if(is.null(rval$seurat) || !("HTO" %in% Assays(rval$seurat))){
      showModal(
        modalDialog(title = "No HTO on your project",
                    "Demultiplexing is based on HTO. Please import HTO data or go to next tab.",
                    easyClose = FALSE,
                    footer = tagList(actionButton("modal_no_hto_ok", "OK")))
      )
    }
  })
  
  
  observeEvent(input$menu, {
    req(input$menu == "Quality_Control_tab")
    if(is.null(rval$seurat)){
      showModal(
        modalDialog(title = "No data.",
                    "Please import data.",
                    easyClose = FALSE,
                    footer = tagList(actionButton("modal_no_hto_ok", "OK")))
      )
    }
  })
  
  observeEvent(input$menu, {
    req(input$menu == "Variable_Genes_tab")
    if(is.null(rval$seurat)){
      showModal(
        modalDialog(title = "No data.",
                    "Please import data.",
                    easyClose = FALSE,
                    footer = tagList(actionButton("modal_no_hto_ok", "OK")))
      )
    }
  })
  
  
  observeEvent(input$menu, {
    req(input$menu == "PCA_tab")
    if(is.null(rval$seurat) || is.null(rval$genes_list[[rval$seurat_selected]])){
      showModal(
        modalDialog(title = "No Variable genes.",
                    "Please identify variable genes",
                    easyClose = FALSE,
                    footer = tagList(actionButton("modal_no_vg", "OK")))
      )
    }else{
      
      if(length(rval$genes_list[[rval$seurat_selected]]) < 2){
        showModal(
          modalDialog(title = "No Variable genes.",
                      "Please identify variable genes",
                      easyClose = FALSE,
                      footer = tagList(actionButton("modal_no_vg", "OK")))
        )
      }
    }
    
  })
  
  
  
  observeEvent(input$menu, {
    req(input$menu == "TSNE_UMAP_tab")
    
    if(!is.null(rval$seurat)){
      reduc <- Reductions(rval$seurat)
      reduc <- reduc[grepl("^pca", reduc, ignore.case = T)]
      
      if(length(reduc) < 1){
        reduc <- NULL
      }
    }
    
    
    if(is.null(rval$seurat) || is.null(rval$genes_list[[rval$seurat_selected]]) || is.null(reduc)){
      showModal(
        modalDialog(title = "No PCA.",
                    "Please calculate PCA before tSNE/UMAP",
                    easyClose = FALSE,
                    footer = tagList(actionButton("modal_no_reduc", "OK")))
      )
    }
  })
  
  
  
  observeEvent(input$menu, {
    req(input$menu == "Clustering_tab")
    
    if(!is.null(rval$seurat)){
      reduc <- Reductions(rval$seurat)
      reduc <- reduc[grepl("^pca", reduc, ignore.case = T)]
      
      if(length(reduc) < 1){
        reduc <- NULL
      }
    }
    
    if(is.null(rval$seurat) || is.null(rval$genes_list[[rval$seurat_selected]]) || is.null(reduc)){
      showModal(
        modalDialog(title = "No Reduction.",
                    "Please, make a dimensionnal reduction",
                    easyClose = FALSE,
                    footer = tagList(actionButton("modal_no_reduc", "OK")))
      )
    }
  })
  
  
  
  observeEvent(input$menu, {
    req(input$menu == "Exploration_tab")
    
    if(!is.null(rval$seurat)){
      
      reduc <- Reductions(rval$seurat)
      
      if(length(reduc) < 1){
        reduc <- NULL
      }
      
    }
    
    
    if(is.null(rval$seurat) || is.null(reduc)){
      showModal(
        modalDialog(title = "No Reduction.",
                    "Please, make a dimensionnal reduction",
                    easyClose = FALSE,
                    footer = tagList(actionButton("modal_no_reduc", "OK")))
      )
    }
  })
  
  
  
  
  observeEvent(input$menu, {
    req(input$menu == "Explor_tab")
    
    if(!is.null(rval$seurat)){
      
      reduc <- Reductions(rval$seurat)
      
      if(length(reduc) < 1){
        reduc <- NULL
      }
      
    }
    
    
    if(is.null(rval$seurat) || is.null(reduc)){
      showModal(
        modalDialog(title = "No Reduction.",
                    "Please, make a dimensionnal reduction",
                    easyClose = FALSE,
                    footer = tagList(actionButton("modal_no_reduc", "OK")))
      )
    }
  })
  
  
  
  
  
  observeEvent(input$menu, {
    req(input$menu == "Export_tab")
    if(is.null(rval$seurat)){
      showModal(
        modalDialog(title = "No data.",
                    "Please import data.",
                    easyClose = FALSE,
                    footer = tagList(actionButton("modal_no_hto_ok", "OK")))
      )
    }
  })
  
  
  
  
  # Return to import tab if no HTO
  observeEvent( input$modal_no_hto_ok, {
    validate(need(rval$modules, "No tab elements available"))
    tab_selected <- rval$modules[1]
    tab_selected <- paste(tab_selected, "tab", sep="_")
    updateTabItems(inputId = "sidebar_tabs", selected = tab_selected, session = session)
    removeModal()
  })
  
  
  
  # Return to variable genes tab
  observeEvent( input$modal_no_vg, {
    validate(need(rval$modules[4], "No tab elements available"))
    tab_selected <- rval$modules[4]
    tab_selected <- paste(tab_selected, "tab", sep="_")
    updateTabItems(inputId = "sidebar_tabs", selected = tab_selected, session = session)
    removeModal()
  })
  
  
  
  # Return to PCA tab
  observeEvent( input$modal_no_reduc, {
    validate(need(rval$modules[5], "No tab elements available"))
    tab_selected <- rval$modules[5]
    tab_selected <- paste(tab_selected, "tab", sep="_")
    updateTabItems(inputId = "sidebar_tabs", selected = tab_selected, session = session)
    removeModal()
  })
  
  
  
  
  ### Update selected Seurat object ################################################################
  
  observeEvent( names(rval$seurat_list), {
    updateSelectInput(session, "select_seu", 
                      choices = names(rval$seurat_list), 
                      selected = names(rval$seurat_list)[length(names(rval$seurat_list))])
  })
  
  
  observeEvent(input$select_seu, {

    if(!is.null(input$select_seu) & input$select_seu != ""){
      
      if(length(names(rval$seurat_list)) > rval$number_s && length(names(rval$seurat_list)) > 1){
        
        rval$number_s <- rval$number_s + 1
        
      }else if(length(names(rval$seurat_list)) == rval$number_s && length(names(rval$seurat_list)) > 1){
        
        # Create modal progress during current project saving
        show_modal_spinner(text = paste0("Saving ", rval$seurat_selected,"...  Please wait..."), spin = 'circle')
        
        saveRDS(rval$seurat, paste0(rval$output,"/",rval$seurat_selected,".rds"))
        
        # Close modal progress when rds is saved
        remove_modal_spinner()
        
        rval$seurat_selected <- input$select_seu
        
        # Create modal progress during current project loading
        show_modal_spinner(text = paste0("Loading ", rval$seurat_selected,"...  Please wait..."), spin = 'circle')
        
        rval$seurat <- readRDS(paste0(rval$output,"/",rval$seurat_selected,".rds"))
        
        # Close modal progress when rds is loaded
        remove_modal_spinner()
        
      }else if(length(names(rval$seurat_list)) > 0){
        rval$number_s <- rval$number_s + 1
      }
      
    }
    
  })
  
  
  
  output$but_repo <- renderUI({

    if(!is.null(rval$seurat)){
      downloadButton('repo', 'Generate report', class = "btn-success")
    }else{
      NULL
    }
    
  })
  
  
  # output$list_seu <- renderUI({
  #   if(!is.null(rval$seurat)){
  #     selectInput("select_seu", "Select project", 
  #                 choices = NULL, 
  #                 selected = NULL)
  #   }else{
  #     NULL
  #   }
  # })
  
  output$repo <- downloadHandler(
    
    
    
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      
      show_modal_spinner(text = 'Generating report... Please wait...', spin = 'circle')
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      
      
      params <- list(dt=rval)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
      remove_modal_spinner()
    }
    
   
  )
  
  
  
  
  ### Main Value boxes #########################################################################
  
  output$progressBox <- renderValueBox({
    Ncells <- 0
    
    if(!is.null(rval$seurat)){
      Ncells <- ncol(rval$seurat)
    }
    valueBox(
      Ncells, "cells",icon = icon("list"),
      color = "purple"
    )
  })
  
  output$progressBox2 <- renderValueBox({
    Ngenes <- 0
    
    if(!is.null(rval$seurat)){
      Ngenes <- nrow(rval$seurat[['RNA']])
    }
    valueBox(
      Ngenes, "genes",icon = icon("list"),
      color = "yellow"
    )
  })
  
  
  output$progressBox3 <- renderValueBox({
    
    Nhto <- 0
    
    if(!is.null(rval$seurat) && ("HTO" %in% Assays(rval$seurat))){
      Nhto <- nrow(rval$seurat[['HTO']])
    }
    
    valueBox(
      Nhto, "HTO",icon = icon("list"),
      color = "green"
    )
  })
  
  
  output$progressBox4 <- renderValueBox({
    Nadt <- 0
    
    if(!is.null(rval$seurat) && ("ADT" %in% Assays(rval$seurat))){
      Nadt <- nrow(rval$seurat[['ADT']])
    }
    
    valueBox(
      Nadt, "ADT",icon = icon("list"),
      color = "red"
    )
  })
  
  output$seurat_name <- renderText({
    if(!is.null(rval$seurat_selected)){
      if(nchar(rval$seurat_selected)>0){
        paste("Current project : ", rval$seurat_selected)
      }
    }else{
      NULL
    }
  })
  
  
  
}

