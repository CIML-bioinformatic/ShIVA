ExportUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      conditionalPanel(condition = "output.check_seu", ns = ns,
                       
        box(title = p("Export Data to File", actionButton(ns("HELP_EXPORT_DATA"), "", icon = icon("question-circle"), class = "btn-xs")),
            width = 6, height = NULL,  
            solidHeader = TRUE, status = "primary", 
            radioButtons(ns("type_export"), label = "Choose the Type of Data to Export",
                         c("Seurat Object RDS File" = "seurat", "List of Genes" = "genes", "List of Cells" = "cells"), inline = T),
            uiOutput(ns("select_export")),
            uiOutput(ns("button_export"))
            #Asif
            #Asif test the button to downl the report
            # , downloadButton(ns("repo"), label = "Create report")
            
        )
        
      )
      
    )
  )
  
}


Export <- function(input, output, session, rval) {
  
  
  
  
  ######### Help buttons ############
  
  
  observeEvent(input[["HELP_EXPORT_DATA"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_EXPORT_DATA"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_EXPORT_DATA"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  
  getSeurat <- reactive({
    
    if(length(rval$seurat_list) > 0){
      
      seu <- rval$seurat_list
      
      return(seu)
      
    }
    
  })
  
  
  
  
  getGenesList <- reactive({
    
    if(length(rval$genes_list) > 0){
      
      lists <- NULL
      
      for(i in 1:length(rval$genes_list)){
        
        lists <- c(lists,names(rval$genes_list[[i]]))
        
      }
      
      return(lists)
      
    }
    
  })
  
  
  
  getCellsList <- reactive({
    
    if(length(rval$cell_list) > 0){
      
      lists <- NULL
      
      for(i in 1:length(rval$cell_list)){
        
        lists <- c(lists,names(rval$cell_list[[i]]))
        
      }
      
      return(lists)

    }
    
  })
  
  
  
  
  output$select_export<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      if(!is.null(input$type_export)){
        
        if(input$type_export == "seurat"){
          
          selectInput(ns("select"), "Select", getSeurat(), selected = tail(getSeurat(),1), 
                      multiple = F, selectize = T)
          
        }else if(input$type_export == "genes"){
          
          selectInput(ns("select"), "Select", getGenesList(), selected = tail(getGenesList(),1), 
                      multiple = F, selectize = T)
          
        }else if(input$type_export == "cells"){
        
        selectInput(ns("select"), "Select", getCellsList(), selected = tail(getCellsList(),1), 
                    multiple = F, selectize = T)
        
        }
      }
      
    }
  }) 
  
  
  
  
  output$button_export <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) & !is.null(input$type_export) & !is.null(input$select)){
      
      if(input$select != ""){
        
        downloadButton(ns("export"), label = "Export Data to File")
        
      }
    }
    
  })
  

  
  
  output$export <- downloadHandler(
    
    filename = function() {
      
      if(input$type_export == "seurat"){
        paste(input$select, ".rds", sep = "")
      }else{
        paste(input$select, ".csv", sep = "")
      }
      
    },
    
    content = function(file) {
      
      if(input$type_export == "seurat"){
        
        # Create modal progress during rds creation and saving
        show_modal_spinner(text = 'Creating rds file... Please wait...', spin = 'circle')
        
        if(input$select == rval$seurat_selected){
          
          
          saveRDS(rval$seurat, file)
          
        }else{
          
          for(seu in rval$seurat_list){
            
            if(seu == input$select){
              
               saveRDS(readRDS(paste0(rval$output,"/",seu,".rds")), file)
              
            }
            
          }
          
        }
        
        # Close modal progress when rds is saved
        remove_modal_spinner()
        
      }else if(input$type_export == "genes"){
        
        # Create modal progress during csv creation and saving
        show_modal_spinner(text = 'Creating csv file... Please wait...', spin = 'circle')

        for(i in 1:length(rval$genes_list)){
          
          if(input$select %in% names(rval$genes_list[[i]])){
            
            genes <- rval$genes_list[[i]][[input$select]]
            
          }
          
        }

        write(genes, file = file)
        
        # Close modal progress when csv is saved
        remove_modal_spinner()
        
      }else if(input$type_export == "cells"){
        
        # Create modal progress during csv creation and saving
        show_modal_spinner(text = 'Creating csv file... Please wait...', spin = 'circle')

        for(i in 1:length(rval$cell_list)){

          if(input$select %in% names(rval$cell_list[[i]])){

            cells <- rval$cell_list[[i]][[input$select]]
            
          }
          
        }
        
        write(cells, file = file)
        
        # Close modal progress when csv is saved
        remove_modal_spinner()
        
      }
    }
  )
  
  
  
  output$check_seu <- reactive({
    return(!is.null(rval$seurat))
  })
  
  
  
  
  
  #Asif #Asif
   
  
  
  # output$repo <- downloadHandler(
  #   # For PDF output, change this to "report.pdf"
  #   filename = "report.html",
  #   content = function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
  #     
  #     
  #     
  #     params <- list(dt=rval)
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  # 
  
  
  outputOptions(output, 'check_seu', suspendWhenHidden=FALSE)
  
  
  return(rval)
  
}