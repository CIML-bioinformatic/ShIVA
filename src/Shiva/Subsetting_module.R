

SubsettingUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      conditionalPanel(condition = "output.check_seu", ns = ns,
                       
                       box(title = p("Parameters to Define Subset", actionButton(ns("HELP_SUBSET_PARAMS"), "", icon = icon("question-circle"), class = "btn-xs")),
                           width = 6, height = NULL,  
                           solidHeader = TRUE, status = "primary", 
                           
                           uiOutput(ns("select_type")),
                           # uiOutput(ns("select_lists_cells")),
                           uiOutput(ns("select_type_meta")),
                           uiOutput(ns("select_meta_num")),
                           uiOutput(ns("range_meta_num1")),
                           uiOutput(ns("range_meta_num2")),
                           uiOutput(ns("range_meta_num3")),
                           hr(),
                           uiOutput(ns("select_meta_fac")),
                           uiOutput(ns("range_meta_fac1")),
                           uiOutput(ns("range_meta_fac2")),
                           uiOutput(ns("range_meta_fac3")),
                           hr(),
                           uiOutput(ns("select_genes")),
                           uiOutput(ns("range_gene1")),
                           uiOutput(ns("range_gene2")),
                           uiOutput(ns("range_gene3")),
                           uiOutput(ns("button_subset"))
                       )
                       
      )
      
    )
  )
  
}




Subsetting <- function(input, output, session, rval) {
  
  
  params <- reactiveValues(cells_to_conserve = NULL,
                           features = list())
  
  
  
  
  ######### Help buttons ############
  
  
  observeEvent(input[["HELP_SUBSET_PARAMS"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_SUBSET_PARAMS"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_SUBSET_PARAMS"),"value"]),
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
      
      return(colnames(select_if(rval$seurat@meta.data, is.factor)))
      
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
  
  
  
  
  getGenes <- reactive({
    
    if(!is.null(rval$seurat)){
      
      genes <- rownames(rval$seurat)
      
      return(genes)
      
    }
    
  })
  
  
  
  
  
  output$select_type<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      # if(!is.null(getCellsList())){
      #   
      # 
      #   checkboxGroupInput(ns("type_subset"), label = "Subset by",
      #                      c("List of cells" = "cells", "Metadata" = "metadata", "Genes expression" = "genes"), 
      #                      inline = T, selected = "cells")
      #   
      # }else{
      #   
      #   checkboxGroupInput(ns("type_subset"), label = "Subset by",
      #                      c("Metadata" = "metadata", "Genes expression" = "genes"), 
      #                      inline = T, selected = "metadata")
      #   
      # }
      
      checkboxGroupInput(ns("type_subset"), label = "Choose the Parameter Type to Create Subset",
                         c("Metadata" = "metadata", "Genes expression" = "genes"), 
                         inline = T, selected = NULL)
      
    }
  }) 
  
  
  
  # # Create selection input to select available lists
  # output$select_lists_cells<- renderUI({
  #   
  #   ns <- session$ns
  #   
  #   if(!is.null(input$type_subset)){
  #     
  #     if("cells" %in% input$type_subset){
  #       
  #       selectizeInput(ns("selectList"), "Select list of cells to conserve", getCellsList(), selected = NULL, multiple = T,
  #                      options = NULL)
  #       
  #     }
  #     
  #   }
  #   
  # }) 
  
  
  
  output$select_type_meta<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_subset)){
      
      if("metadata" %in% input$type_subset){
        
        
        checkboxGroupInput(ns("type_meta"), label = "Choose the Type of Metadata to Use",
                           c("Numeric" = "numeric", "Factorial" = "factorial"), 
                           inline = T, selected = NULL)
        
      }
    }
    
  }) 
  
  
  
  
  
  
  # Create selection input to select available lists
  output$select_meta_num<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_subset)){
      
      if("metadata" %in% input$type_subset){
        
        if(!is.null(input$type_meta)){
          
          if("numeric" %in% input$type_meta){
            
            selectizeInput(ns("selectMetanum"), "Choose the Numeric Metadata to Use", numericCol(), 
                           selected = NULL, multiple = T, options = list(maxItems = 3, placeholder = 'select 3 metadata max'))
            
          }
          
        }
        
      }
    }
    
  }) 
  
  
  
  # Create selection input to select available lists
  output$select_meta_fac<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_subset)){
      
      if("metadata" %in% input$type_subset){
        
        if(!is.null(input$type_meta)){
          
          if("factorial" %in% input$type_meta){
            
            selectizeInput(ns("selectMetafac"), "Choose the Factorial Metadata to Use", factorCol(), 
                           selected = NULL, multiple = T, options = list(maxItems = 3, placeholder = 'select 3 metadata max'))
            
          }
          
        }
        
      }
      
    }
    
  }) 
  
  
  
  
  
  # Create selection input to select available lists
  output$range_meta_num1<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_subset)){
      
      if("metadata" %in% input$type_subset){
        
        if(!is.null(input$type_meta)){
          
          if("numeric" %in% input$type_meta){
            
            if(!is.null(input$selectMetanum)){
              
              sliderInput(ns("sliderNum1"), label = paste0("Set the Range of Values to Keep for ", input$selectMetanum[1]), 
                          min = round(min(rval$seurat[[input$selectMetanum[1]]]),2), max = round(max(rval$seurat[[input$selectMetanum[1]]]),2), 
                          value = c(round(min(rval$seurat[[input$selectMetanum[1]]]),2), round(max(rval$seurat[[input$selectMetanum[1]]]),2)))
              
            }
          }
          
        }
        
      }
    }
    
  }) 
  
  
  
  
  
  # Create selection input to select available lists
  output$range_meta_num2<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_subset)){
      
      if("metadata" %in% input$type_subset){
        
        if(!is.null(input$type_meta)){
          
          if("numeric" %in% input$type_meta){
            
            if(!is.null(input$selectMetanum) & length(input$selectMetanum) >= 2){
              
              sliderInput(ns("sliderNum2"), label = paste0("Set the Range of Values to Keep for ", input$selectMetanum[2]), 
                          min = round(min(rval$seurat[[input$selectMetanum[2]]]),2), max = round(max(rval$seurat[[input$selectMetanum[2]]]),2), 
                          value = c(round(min(rval$seurat[[input$selectMetanum[2]]]),2), round(max(rval$seurat[[input$selectMetanum[2]]]),2)))
              
            }
          }
          
        }
        
      }
    }
    
  }) 
  
  
  
  
  
  # Create selection input to select available lists
  output$range_meta_num3<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_subset)){
      
      if("metadata" %in% input$type_subset){
        
        if(!is.null(input$type_meta)){
          
          if("numeric" %in% input$type_meta){
            
            if(!is.null(input$selectMetanum) & length(input$selectMetanum) >= 3){
              
              sliderInput(ns("sliderNum3"), label = paste0("Set the Range of Values to Keep for ", input$selectMetanum[3]), 
                          min = round(min(rval$seurat[[input$selectMetanum[3]]]),2), max = round(max(rval$seurat[[input$selectMetanum[3]]]),2), 
                          value = c(round(min(rval$seurat[[input$selectMetanum[3]]]),2), round(max(rval$seurat[[input$selectMetanum[3]]]),2)))
              
            }
          }
          
        }
        
      }
    }
    
  }) 
  
  
  
  
  output$range_meta_fac1<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_subset)){
      
      if("metadata" %in% input$type_subset){
        
        if(!is.null(input$type_meta)){
          
          if("factorial" %in% input$type_meta){
            
            if(!is.null(input$selectMetafac)){
              
              selectizeInput(ns("selectLevelsFac1"), paste0("Keep Labels for ", input$selectMetafac[1]),  
                             choices = levels(droplevels(rval$seurat@meta.data[,input$selectMetafac[1]])), 
                             selected = levels(droplevels(rval$seurat@meta.data[,input$selectMetafac[1]])), 
                             multiple = T, options = NULL)
              
            }
          }
          
        }
        
      }
    }
    
  }) 
  
  
  
  
  output$range_meta_fac2<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_subset)){
      
      if("metadata" %in% input$type_subset){
        
        if(!is.null(input$type_meta)){
          
          if("factorial" %in% input$type_meta){
            
            if(!is.null(input$selectMetafac) & length(input$selectMetafac) >= 2){
              
              selectizeInput(ns("selectLevelsFac2"), paste0("Keep Labels for ", input$selectMetafac[2]),  
                             choices = levels(droplevels(rval$seurat@meta.data[,input$selectMetafac[2]])), 
                             selected = levels(droplevels(rval$seurat@meta.data[,input$selectMetafac[2]])), 
                             multiple = T, options = NULL)
              
            }
          }
          
        }
        
      }
    }
    
  }) 
  
  
  
  
  output$range_meta_fac3<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_subset)){
      
      if("metadata" %in% input$type_subset){
        
        if(!is.null(input$type_meta)){
          
          if("factorial" %in% input$type_meta){
            
            if(!is.null(input$selectMetafac) & length(input$selectMetafac) >= 3){
              
              selectizeInput(ns("selectLevelsFac2"), paste0("Keep Labels for ", input$selectMetafac[3]),  
                             choices = levels(droplevels(rval$seurat@meta.data[,input$selectMetafac[3]])), 
                             selected = levels(droplevels(rval$seurat@meta.data[,input$selectMetafac[3]])), 
                             multiple = T, options = NULL)
              
            }
          }
          
        }
        
      }
    }
    
  }) 
  
  
  
  
  
  
  output$select_genes<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_subset)){
      
      if("genes" %in% input$type_subset){
        
        selectizeInput(ns("selectGene"), "Choose Genes to Use (3 max)", sort(getGenes()), 
                       selected = NULL, multiple = T, 
                       options = list(maxItems = 3, placeholder = 'select 3 genes max'))
        
      }
      
    }
    
  }) 
  
  
  
  
  
  
  output$range_gene1<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_subset)){
      
      if("genes" %in% input$type_subset){
        
        if(!is.null(input$selectGene)){

          sliderInput(ns("sliderGene1"), label = paste0("Set the Range of Values to Keep for ", input$selectGene[1]), 
                      min = round(min(FetchData(rval$seurat, vars = input$selectGene[1])),2), max = round(max(FetchData(rval$seurat, vars = input$selectGene[1])),2), 
                      value = c(round(min(FetchData(rval$seurat, vars = input$selectGene[1])),2), round(max(FetchData(rval$seurat, vars = input$selectGene[1])),2)))
          
        }
        
      }
    }
    
  }) 
  
  
  
  output$range_gene2<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_subset)){
      
      if("genes" %in% input$type_subset){
        
        if(!is.null(input$selectGene) & length(input$selectGene) >= 2){
          
          sliderInput(ns("sliderGene2"), label = paste0("Set the Range of Values to Keep for ", input$selectGene[2]), 
                      min = round(min(FetchData(rval$seurat, vars = input$selectGene[2])),2), max = round(max(FetchData(rval$seurat, vars = input$selectGene[2])),2), 
                      value = c(round(min(FetchData(rval$seurat, vars = input$selectGene[2])),2), round(max(FetchData(rval$seurat, vars = input$selectGene[2])),2)))
          
        }
        
      }
    }
    
  }) 
  
  
  
  
  
  output$range_gene3<- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$type_subset)){
      
      if("genes" %in% input$type_subset){
        
        if(!is.null(input$selectGene) & length(input$selectGene) >= 3){
          
          sliderInput(ns("sliderGene3"), label = paste0("Set the Range of Values to Keep for ", input$selectGene[3]), 
                      min = round(min(FetchData(rval$seurat, vars = input$selectGene[3])),2), max = round(max(FetchData(rval$seurat, vars = input$selectGene[3])),2), 
                      value = c(round(min(FetchData(rval$seurat, vars = input$selectGene[3])),2), round(max(FetchData(rval$seurat, vars = input$selectGene[3])),2)))
          
        }
        
      }
    }
    
  }) 
  
  
  
  
  
  output$button_subset <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$selectMetanum) || !is.null(input$selectMetafac) || !is.null(input$selectGene)){
      
      actionButton(ns("subset"), label = "Subset")
      
    }
    
  })
  
  
  
  
  observeEvent(input$subset, {
    
    
    ns <- session$ns
    
    
    if(!is.null(rval$seurat) & !is.null(input$type_subset)){
      
      
      cells <- colnames(rval$seurat)
      
      
      
      if("metadata" %in% input$type_subset){
        
        if(!is.null(input$type_meta)){
          
          
          
          
          if("numeric" %in% input$type_meta){
            
            
            if(!is.null(input$selectMetanum)){
              
              for(i in 1:length(input$selectMetanum)){
                
                if(i == 1){
                  
                  expr <- FetchData(object = rval$seurat, vars = input$selectMetanum[1])
                  
                  expr <- subset(expr, expr[,input$selectMetanum[1]] >= min(input$sliderNum1) & expr[,input$selectMetanum[1]] <= max(input$sliderNum1))
                  
                  cells <- intersect(cells,rownames(expr))
                  
                  params$features[[input$selectMetanum[1]]] = c(min(input$sliderNum1),max(input$sliderNum1))
                  
                }
                
                if(i == 2){
                  
                  expr <- FetchData(object = rval$seurat, vars = input$selectMetanum[2])
                  
                  expr <- subset(expr, expr[,input$selectMetanum[2]] >= min(input$sliderNum2) & expr[,input$selectMetanum[2]] <= max(input$sliderNum2))
                  
                  cells <- intersect(cells,rownames(expr))
                  
                  params$features[[input$selectMetanum[2]]] = c(min(input$sliderNum2),max(input$sliderNum2))
                  
                }
                
                if(i == 3){
                  
                  expr <- FetchData(object = rval$seurat, vars = input$selectMetanum[3])
                  
                  expr <- subset(expr, expr[,input$selectMetanum[3]] >= min(input$sliderNum3) & expr[,input$selectMetanum[3]] <= max(input$sliderNum3))
                  
                  cells <- intersect(cells,rownames(expr))   
                  
                  params$features[[input$selectMetanum[3]]] = c(min(input$sliderNum3),max(input$sliderNum3))
                }
                
              }
              
            }
            
          }
          
          
          
          
          
          if("factorial" %in% input$type_meta){
            
            if(!is.null(input$selectMetafac)){
              
              for(i in 1:length(input$selectMetafac)){
                
                if(i == 1){
                  
                  Idents(rval$seurat) <- input$selectMetafac[1]
                  
                  cells <- intersect(cells, WhichCells(rval$seurat, idents = input$selectLevelsFac1))
                  
                  params$features[[input$selectMetafac[1]]] = input$selectLevelsFac1
                  
                }
                
                if(i == 2){
                  
                  Idents(rval$seurat) <- input$selectMetafac[2]
                  
                  cells <- intersect(cells, WhichCells(rval$seurat, idents = input$selectLevelsFac2))
                  
                  params$features[[input$selectMetafac[2]]] = input$selectLevelsFac2
                  
                }
                
                if(i == 3){
                  
                  Idents(rval$seurat) <- input$selectMetafac[3]
                  
                  cells <- intersect(cells, WhichCells(rval$seurat, idents = input$selectLevelsFac3))
                  
                  params$features[[input$selectMetafac[3]]] = input$selectLevelsFac3
                  
                }
                
              }
              
            }
            
          }
        }
        
        
        
        
      }
      
      
      
      
      if("genes" %in% input$type_subset){
        
        if(!is.null(input$selectGene)){
          
          for(i in 1:length(input$selectGene)){
            
            if(i == 1){
              
              expr <- FetchData(object = rval$seurat, vars = input$selectGene[1])
              
              expr <- subset(expr, expr[,input$selectGene[1]] >= min(input$sliderGene1) & expr[,input$selectGene[1]] <= max(input$sliderGene1))
              
              cells <- intersect(cells,rownames(expr))
              
              params$features[[input$selectGene[1]]] = c(min(input$sliderGene1),max(input$sliderGene1))
              
            }
            
            if(i == 2){
              
              expr <- FetchData(object = rval$seurat, vars = input$selectGene[2])
              
              expr <- subset(expr, expr[,input$selectGene[2]] >= min(input$sliderGene2) & expr[,input$selectGene[2]] <= max(input$sliderGene2))
              
              cells <- intersect(cells,rownames(expr))
              
              params$features[[input$selectGene[2]]] = c(min(input$sliderGene2),max(input$sliderGene2))
              
            }
            
            if(i == 3){
              
              expr <- FetchData(object = rval$seurat, vars = input$selectGene[3])
              
              expr <- subset(expr, expr[,input$selectGene[3]] >= min(input$sliderGene3) & expr[,input$selectGene[3]] <= max(input$sliderGene3))
              
              cells <- intersect(cells,rownames(expr))        
              
              params$features[[input$selectGene[3]]] = c(min(input$sliderGene3),max(input$sliderGene3))
            }
            
          }
          
        }
        
      }
      
      
      
      if(!is.null(cells) & length(cells) > 50){
        
        if(length(cells) == length(colnames(rval$seurat))){
          
          showModal(modalDialog(
            "No cells removed. Check your parameters.",
            size = "m",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          
          
        }else{
          
          cells <- unique(cells)
          
          params$cells_to_conserve = cells
          
          showModal(subModal(ncells = length(cells), ns = ns))
          
          # seu_temp <- subset(rval$seurat, cells = cells)
          
        }
        
        
      }else{
        
        showModal(modalDialog(
          "Insufficient cells conserved. Check your parameters.",
          size = "m",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        
      }
      
    }
    
  })
  
  
  
  
  
  
  
  subModal <- function(failed = FALSE, ncells = 0, ns = NULL) {
    
    modalDialog(
      paste0("You will create a new project containing ", ncells, " cells."),
      size = "m",
      easyClose = FALSE,
      HTML("<br><br>"),
      textInput(ns("project_sub"), "Enter new project name", value = paste0(rval$seurat_selected,"_afterSubset")),
      
      if (failed)
        div(tags$b("Project name already exist or is invalid (only alphanumeric and _ characters)", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("sub_ok"), "OK")
      )
    )
  }
  
  
  
  
  
  
  
  
  observeEvent(input$sub_ok, {
    
    ns <- session$ns
    
    # Check that data object exists and is data frame.
    if (!(input$project_sub %in% names(rval$seurat_list)) && input$project_sub != '' && !grepl("[^A-Za-z0-9_]", input$project_sub) == TRUE){
      
      removeModal()
      
      # Create modal progress during rds Saving
      show_modal_spinner(text = paste0('Saving ', rval$seurat_selected,'... Please wait...'), spin = 'circle')
      
      saveRDS(rval$seurat, paste0(rval$output,"/",rval$seurat_selected,".rds"))
      saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))
      
      # Close modal progress when rds is saved
      remove_modal_spinner()
      
      
      
      
      # Create modal progress during object creation
      show_modal_spinner(text = paste0('Creating ', input$project_sub,'... Please wait...'), spin = 'circle')
      
      # rval$parameters[[input$project_sub]] <- c(rval$parameters[[input$project_sub]], "parent" = rval$seurat_selected)
      rval$parameters[[input$project_sub]][["parent"]] <- rval$seurat_selected
      rval$parameters[[input$project_sub]][["after_step"]] <- "subset"
      
      rval$parameters[[input$project_sub]][["subset_params"]] = params$features

      rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "subset")
      
      cells_to_conserve <- params$cells_to_conserve
      
      rval$parameters[[input$project_sub]]["rm_cells"] = length(Cells(rval$seurat)) - length(params$cells_to_conserve)
      
      rval$seurat <- subset(rval$seurat, cells = cells_to_conserve)

      rval$seurat_list[[input$project_sub]] <- input$project_sub
      
      rval$seurat_selected <- input$project_sub
      
      rval$genes_list[[rval$seurat_selected]][[paste0("All_genes_",rval$seurat_selected)]] = rownames(rval$seurat)
      
      # rval$parameters[[rval$seurat_selected]]["is.qc"] = TRUE
      
      # Close modal progress when object is created
      remove_modal_spinner()
      
      
      
      # Create modal progress during object normalization
      show_modal_spinner(text = paste0('Normalization ', rval$seurat_selected,'... Please wait...'), spin = 'circle')
      
      
      if(rval$norm_assay == "SCT"){
        rval$seurat <- SCTransform(rval$seurat,
                                   assay = "RNA",
                                   new.assay.name = "SCT",
                                   verbose = TRUE,
                                   return.only.var.genes = FALSE)
      }else{
        rval$seurat <- NormalizeData(rval$seurat, 
                                     normalization.method = "LogNormalize", 
                                     assay = "RNA", 
                                     scale.factor = 10000,
                                     margin = 1, 
                                     verbose = TRUE)
      }
      
      rval$parameters[[rval$seurat_selected]]["is.normalized"] = TRUE
      
      saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))
      
      # Close modal progress when object normalization is done
      remove_modal_spinner()
      
      
      
    } else {
      
      ncells <- length(params$cells_to_conserve)
      showModal(subModal(failed = TRUE, ns = ns, ncells = ncells))
      
    }
    
  })
  
  
  
  
  
  
  
  
  
  output$check_seu <- reactive({
    return(!is.null(rval$seurat))
  })
  
  outputOptions(output, 'check_seu', suspendWhenHidden=FALSE)
  
  return(rval)
}



