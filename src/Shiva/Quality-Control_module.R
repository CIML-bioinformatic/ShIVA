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
#' @rdname Quality_ControlUI
#' 


Quality_ControlUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    
    box(title = p("Filter Low Quality Cells", actionButton(ns("HELP_QC_PARAMS"), "", icon = icon("question-circle"), class = "btn-xs")),
        width = 12, height = NULL,  
        solidHeader = TRUE, status = "primary", 
        column(3, uiOutput(ns("slider_nb_features"))),
        column(3, uiOutput(ns("slider_nb_UMI"))),
        column(3, uiOutput(ns("slider_percent_Mito"))),
        column(3, uiOutput(ns("slider_percent_Ribo"))),
        
        column(3, actionButton(ns("updateF"), "Update Thresholds with Slider Values")),
        # column(3, actionButton(ns("updateU"), "Update threshold with slider values")),
        # column(3, actionButton(ns("updatePM"), "Update threshold with slider values")),
        # column(3, actionButton(ns("updatePR"), "Update threshold with slider values")),
        
        column(12, br()),
        
        column(1, uiOutput(ns("text_nb_featuresMin"))),
        column(1, br()),
        column(1, uiOutput(ns("text_nb_featuresMax"))),
        
        column(1, uiOutput(ns("text_nb_UMIMin"))),
        column(1, br()),
        column(1, uiOutput(ns("text_nb_UMIMax"))),
        
        column(1, uiOutput(ns("text_percent_MitoMin"))),
        column(1, br()),
        column(1, uiOutput(ns("text_percent_MitoMax"))),
        
        column(1, uiOutput(ns("text_percent_RiboMin"))),
        column(1, br()),
        column(1, uiOutput(ns("text_percent_RiboMax"))),
        
        column(3, actionButton(ns("updateF_num"), "Update Thresholds with Input Values")),
        # column(3, actionButton(ns("updateU_num"), "Update threshold with input values")),
        # column(3, actionButton(ns("updatePM_num"), "Update threshold with input values")),
        # column(3, actionButton(ns("updatePR_num"), "Update threshold with input values")),
        column(12, br()),
        column(2, uiOutput(ns("button_QC")))
    ),
    
    box(title = p("Visualization", actionButton(ns("HELP_QC_PLOTS"), "", icon = icon("question-circle"), class = "btn-xs")),
        width = 12,
        solidHeader = TRUE, status = "primary", 
        tabsetPanel(
          tabPanel("Violin plot",  # Panel to display the violin plot
                   uiOutput(ns("select_factor")),
                   column(6,plotOutput(ns("Vln_nFeatures")) %>% withSpinner(color="#0dc5c1")),
                   column(6,plotOutput(ns("Vln_nCount")) %>% withSpinner(color="#0dc5c1")),
                   column(6,plotOutput(ns("Vln_percent_Mito")) %>% withSpinner(color="#0dc5c1")),
                   column(6,plotOutput(ns("Vln_percent_Ribo")) %>% withSpinner(color="#0dc5c1")),
		   downPlotUI(id = ns("Plotdown"))
          ),
          
          tabPanel("Scatter plot", # Panel to display the scatter plot
                   plotOutput(ns("scaterplot")) %>% withSpinner(color="#0dc5c1"),
                   column(6,uiOutput(ns("select_paramX"))),
                   column(6,uiOutput(ns("select_paramY"))),
                   downPlotUI(id = ns("PlotdownScater"))
                   
          )
        )
    ) 
  )
  
}


Quality_Control <- function(input, output, session, rval) {
  
  # rval_mod() <- reactiveValues(rval$seurat)
  # rval_mod() <- seu
  
  rval_mod <- reactive({
    return(rval$seurat)
    # return(seu)
  })
  
  
  params <- reactiveValues(min1 = NULL,
                           max1 = NULL,
                           min2 = NULL,
                           max2 = NULL,
                           min3 = NULL,
                           max3 = NULL,
                           min4 = NULL,
                           max4 = NULL)
  
  
  
  
  
  
  ######### Help buttons ############
  
  
  observeEvent(input[["HELP_QC_PARAMS"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_QC_PARAMS"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_QC_PARAMS"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_QC_PLOTS"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_QC_PLOTS"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_QC_PLOTS"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  ### slider range of number of features (genes) ###
  
  output$slider_nb_features <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval_mod())){
      
      nfeat_col <- paste0("nFeature_","RNA")
      
      if(nfeat_col %in% colnames(rval_mod()@meta.data)){
        
        sliderInput(ns("qc_nb_feature"), "Set Number of Genes per Cell",  
                    min = floor(min(rval_mod()@meta.data[nfeat_col])/100)*100, 
                    max = ceiling(max(rval_mod()@meta.data[nfeat_col])/100)*100,
                    step = 1,
                    value = c(floor(min(rval_mod()@meta.data[nfeat_col])/100)*100, 
                              ceiling(max(rval_mod()@meta.data[nfeat_col])/100)*100))
        
      }else{
        NULL
      }
    }  
  })
  
  
  
  output$text_nb_featuresMin <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval_mod())){
      
      nfeat_col <- paste0("nFeature_","RNA")
      
      if(nfeat_col %in% colnames(rval_mod()@meta.data)){
        
        
        textInput(ns("qc_nb_featMin"), "Min", value=floor(min(rval_mod()@meta.data[nfeat_col])/100)*100)
        
        
      }else{
        NULL
      }
    }
    
  })
  
  
  
  output$text_nb_featuresMax <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval_mod())){
      
      nfeat_col <- paste0("nFeature_","RNA")
      
      if(nfeat_col %in% colnames(rval_mod()@meta.data)){
        
        
        textInput(ns("qc_nb_featMax"), "Max", value=ceiling(max(rval_mod()@meta.data[nfeat_col])/100)*100)
        
      }else{
        NULL
      }
    }
    
  })
  
  
  
  
  ### Data frame of number Features (Cells to conserved)
  
  dfNbFeatures <- reactive({
    
    ns <- session$ns
    
    if(!is.null(rval_mod()) && !is.null(input$color_factor)){
      
      nfeat_col <- paste0("nFeature_","RNA")
      
      if(nfeat_col %in% colnames(rval_mod()@meta.data)){
        
        temp_df <- data.frame(nFeature_RNA = rval_mod()@meta.data[,nfeat_col], 
                              Cells_to_conserved = TRUE, 
                              Ident = rval_mod()@meta.data[,input$color_factor],
                              row.names = rownames(rval_mod()@meta.data))    
        
        to_remove = which(temp_df$nFeature_RNA < params$min1 | temp_df$nFeature_RNA >params$max1) 
        
        temp_df[to_remove,"Cells_to_conserved"] = FALSE
        
        temp_df <- temp_df[which(temp_df$Cells_to_conserved %in% TRUE),]
        
        return(temp_df)
        
      }else{
        
        return(NULL)
        
      }
      
    }else{
      
      return(NULL)
      
    }
    
  })
  
  
  ### Visualization of number of features per cell (violin plot)
  
  qc_plot1 <- reactive({
    ns <- session$ns
   

    if(is.null(dfNbFeatures())){
      NULL
    }else{
      ggplot(df_all(), aes(x=Ident, y=nFeature_RNA)) + geom_violin(aes(fill=Ident)) + 
      geom_jitter(height = 0, size = 0.15, aes(color = Cells_to_conserved)) + 
      geom_hline(yintercept =  params$min1, color = "red", linetype = 2) + 
      geom_hline(yintercept = params$max1, color = "red", linetype = 2) + 
      scale_color_manual(values = c("FALSE" = "grey", "TRUE" = "black"), name="Cells_to_conserved")
      
    }

  })

  output$Vln_nFeatures <- renderPlot({
    qc_plot1()
  })

  
  
  
  ### slider range of number of UMI ###
  
  output$slider_nb_UMI <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval_mod())){
      
      nCount_col <- paste0("nCount_","RNA")
      
      if(nCount_col %in% colnames(rval_mod()@meta.data)){
        
        sliderInput(ns("qc_nb_UMI"), "Set Number of UMI per Cell",  
                    min = floor(min(rval_mod()@meta.data[nCount_col])/100)*100, 
                    max = ceiling(max(rval_mod()@meta.data[nCount_col])/100)*100,
                    step = 1,
                    value = c(floor(min(rval_mod()@meta.data[nCount_col])/100)*100, 
                              ceiling(max(rval_mod()@meta.data[nCount_col])/100)*100))
        
        
      }else{
        NULL
      }
    }
    
  })
  
  
  output$text_nb_UMIMin <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval_mod())){
      
      nCount_col <- paste0("nCount_","RNA")
      
      if(nCount_col %in% colnames(rval_mod()@meta.data)){
        
        
        textInput(ns("qc_nb_UMIMin"), "Min",value=floor(min(rval_mod()@meta.data[nCount_col])/100)*100)
        
        
      }else{
        NULL
      }
    }
    
  })
  
  
  output$text_nb_UMIMax <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval_mod())){
      
      nCount_col <- paste0("nCount_","RNA")
      
      if(nCount_col %in% colnames(rval_mod()@meta.data)){
        
        textInput(ns("qc_nb_UMIMax"), "Max", value=ceiling(max(rval_mod()@meta.data[nCount_col])/100)*100)
        
      }else{
        NULL
      }
    }
    
  })
  
  
  ### Data frame of number UMI (Cells to conserved)
  
  dfNbUMI <- reactive({
    
    ns <- session$ns
    
    if(!is.null(rval_mod()) && !is.null(input$color_factor)){
      
      nCount_col <- paste0("nCount_","RNA")
      
      if(nCount_col %in% colnames(rval_mod()@meta.data)){
        
        
        temp_df2 <- data.frame(nCount_RNA = rval_mod()@meta.data[,nCount_col], 
                               Cells_to_conserved = TRUE, 
                               Ident = rval_mod()@meta.data[,input$color_factor],
                               row.names = rownames(rval_mod()@meta.data))    
        
        to_remove2 = which(temp_df2$nCount_RNA < params$min2 | temp_df2$nCount_RNA > params$max2) 
        
        temp_df2[to_remove2,"Cells_to_conserved"] = FALSE
        
        temp_df2 <- temp_df2[which(temp_df2$Cells_to_conserved %in% TRUE),]
        
        return(temp_df2)
        
      }else{
        NULL
      }
    }
  })
  
  
  ### Visualization of number of UMI per cell (violin plot)
  
  qc_plot2 <- reactive({
    ns <- session$ns

    if(is.null(dfNbUMI())){
      NULL
    }else{
      ggplot(df_all(), aes(x=Ident, y=nCount_RNA)) + geom_violin(aes(fill=Ident)) + 
        geom_jitter(height = 0, size = 0.15, aes(color = Cells_to_conserved)) + 
        geom_hline(yintercept =  params$min2, color = "red", linetype = 2) + 
        geom_hline(yintercept = params$max2, color = "red", linetype = 2) + 
        scale_color_manual(values = c("FALSE" = "grey", "TRUE" = "black"), 
                           name="Cells_to_conserved")
      
      
      
     
      
    }
   
    
  })

  output$Vln_nCount <- renderPlot(
    qc_plot2()
    )
  
  
  isMito <- reactive({
    if("percent.mito" %in% colnames(rval_mod()@meta.data)){
      TRUE
    }else{
      FALSE
    }
  })
  
  
  ### slider range of percentage of mitochondrial ###
  
  output$slider_percent_Mito <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval_mod())){
      
      if(isMito() == TRUE){
        
        sliderInput(ns("qc_percent_Mito"), "Set Percentage of Mito Genes per Cell",  
                    min = 0, 
                    max = ceiling(max(rval_mod()$percent.mito)),
                    step = 0.1,
                    value = c(0, ceiling(max(rval_mod()$percent.mito))))
        
      }else{
        NULL
      }
    } 
  })
  
  
  output$text_percent_MitoMin <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval_mod())){
      
      if(isMito() == TRUE){
        
        
        textInput(ns("qc_percent_MitoMin"), "Min", 0)
        
        
      }else{
        NULL
      }
    }
    
  })
  
  
  
  output$text_percent_MitoMax <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval_mod())){
      
      if(isMito() == TRUE){
        
        textInput(ns("qc_percent_MitoMax"), "Max", value = ceiling(max(rval_mod()$percent.mito)))
        
      }else{
        NULL
      }
    }
    
  })
  
  
  
  ### Data frame of percent mitochondrial (Cells to conserved)
  
  dfPercentMito <- reactive({
    
    ns <- session$ns
    
    if(!is.null(rval_mod()) && !is.null(input$color_factor)){
      
      if("percent.mito" %in% colnames(rval_mod()@meta.data)){
        
        temp_df3 <- data.frame(percent.mito = rval_mod()@meta.data$percent.mito, 
                               Cells_to_conserved = TRUE, 
                               Ident = rval_mod()@meta.data[,input$color_factor],
                               row.names = rownames(rval_mod()@meta.data))    
        
        to_remove3 = which(temp_df3$percent.mito < params$min3 | temp_df3$percent.mito > params$max3) 
        
        temp_df3[to_remove3,"Cells_to_conserved"] = FALSE
        
        temp_df3 <- temp_df3[which(temp_df3$Cells_to_conserved %in% TRUE),]
        
        return(temp_df3)
        
      }else{
        NULL
      }
    } 
  })
  
  
  ### Visualization of percentage mitochondrial genes per cell (violin plot)
  
qc_plot3 <- reactive({
    ns <- session$ns

    if(is.null(dfPercentMito())){
      NULL
    }else{
      ggplot(df_all(), aes(x=Ident, y=percent.mito)) + geom_violin(aes(fill=Ident)) + 
        geom_jitter(height = 0, size = 0.15, aes(color = Cells_to_conserved)) + 
        geom_hline(yintercept =  params$min3, color = "red", linetype = 2) + 
        geom_hline(yintercept = params$max3, color = "red", linetype = 2) + 
        scale_color_manual(values = c("FALSE" = "grey", "TRUE" = "black"), 
                           name="Cells_to_conserved")
      
      
      
      
    }
    
    
    

    })

  output$Vln_percent_Mito <- renderPlot({
    qc_plot3()
  
    
  }
    
    )
  
  
  
  isRibo <- reactive({
    if("percent.ribo" %in% colnames(rval_mod()@meta.data)){
      TRUE
    }else{
      FALSE
    }
  })
  
  
  ### slider range of percentage of ribosomal ###
  
  output$slider_percent_Ribo <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval_mod())){
      
      if(isRibo() == TRUE){
        
        sliderInput(ns("qc_percent_Ribo"), "Set Percentage of Ribo Genes per Cell",  
                    min = 0, 
                    max = ceiling(max(rval_mod()$percent.ribo)),
                    step = 0.1,
                    value = c(0, ceiling(max(rval_mod()$percent.ribo))))
        
      }else{
        NULL
      }
    } 
  })
  
  
  
  output$text_percent_RiboMin <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval_mod())){
      
      if(isRibo() == TRUE){
        
        
        textInput(ns("qc_percent_RiboMin"), "Min", 0)
        
        
      }else{
        NULL
      }
    }
    
  })
  
  
  
  output$text_percent_RiboMax <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval_mod())){
      
      if(isRibo() == TRUE){
        
        textInput(ns("qc_percent_RiboMax"), "Max", value = ceiling(max(rval_mod()$percent.ribo)))
        
      }else{
        NULL
      }
    }
    
  })
  
  
  
  
  ### Data frame of percent ribosomal (Cells to conserved)
  
  dfPercentRibo <- reactive({
    
    ns <- session$ns
    
    
    if(!is.null(rval_mod()) && !is.null(input$color_factor)){
      
      if("percent.ribo" %in% colnames(rval_mod()@meta.data)){
        
        temp_df4 <- data.frame(percent.ribo = rval_mod()@meta.data$percent.ribo, 
                               Cells_to_conserved = TRUE, 
                               Ident = rval_mod()@meta.data[,input$color_factor],
                               row.names = rownames(rval_mod()@meta.data))    
        
        to_remove4 = which(temp_df4$percent.ribo < params$min4 | temp_df4$percent.ribo > params$max4) 
        
        temp_df4[to_remove4,"Cells_to_conserved"] = FALSE
        
        temp_df4 <- temp_df4[which(temp_df4$Cells_to_conserved %in% TRUE),]
        
        return(temp_df4)
      }else{
        NULL
      }
    } 
  })
  
  
  ### Visualization of percentage ribosomal genes per cell (violin plot)
  
qc_plot4 <- reactive({
    ns <- session$ns
     if(is.null(dfPercentRibo())){
      NULL
    }else{
      p4 <- ggplot(df_all(), aes(x=Ident, y=percent.ribo)) + geom_violin(aes(fill=Ident)) + 
        geom_jitter(height = 0, size = 0.15, aes(color = Cells_to_conserved)) + 
        geom_hline(yintercept =  params$min4, color = "red", linetype = 2) + 
        geom_hline(yintercept = params$max4, color = "red", linetype = 2) + 
        scale_color_manual(values = c("FALSE" = "grey", "TRUE" = "black"), 
                           name="Cells_to_conserved")
      
      return(p4)
      
    }
    
    
    })

  output$Vln_percent_Ribo <- renderPlot(
    qc_plot4()
    )


#combine all plots and make them reactive
all_plots <- reactive({
  ns <- session$ns
#pp <-qc_plot1()+ qc_plot2()+ qc_plot3()+ qc_plot4() 
plot_merge <- CombinePlots(list(qc_plot1(), qc_plot2(), qc_plot3(), qc_plot4()))
plot_merge
  })
 
callModule(downPlotServer, id = "Plotdown", data = all_plots, out_file = "QC_plot")
  
  
  factorCol <- reactive({
    
    if(!is.null(rval_mod())){
      
      return(colnames(select_if(rval_mod()@meta.data, is.factor)))
      
    }
    
  })
  
  ### select factor with a selectInput for choose legend
  
  output$select_factor <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval_mod())){
      
      choicesFactor <- factorCol()
      # test_choix <- select_if(rval_mod()@meta.data, is.factor)
      
      selectInput(ns("color_factor"), "Select Metadata for Visualization",  width = "40%",
                  choicesFactor, selected = choicesFactor[1])
      
    }else{
      NULL
    }
    
  })
  
  
  
  df_all <- reactive({
    
    ns <- session$ns
    
    if(!is.null(rval_mod()) && !is.null(input$color_factor)){
      
      
      nCount_col <- paste0("nCount_","RNA")
      
      nfeat_col <- paste0("nFeature_","RNA")
      
      temp_df5 <- data.frame(row.names = rownames(rval_mod()@meta.data))
      
      temp_df5["Cells_to_conserved"] = TRUE
      
      temp_df5["Ident"] = rval_mod()@meta.data[,input$color_factor]
      
      if(nCount_col %in% colnames(rval_mod()@meta.data)){
        
        temp_df5["nCount_RNA"] <- rval_mod()@meta.data[,nCount_col]
        
        to_remove_ncount = which(temp_df5$nCount_RNA < params$min2 | temp_df5$nCount_RNA > params$max2)
        
      }else{
        to_remove_ncount = NULL
      }
      
      if(nfeat_col %in% colnames(rval_mod()@meta.data)){
        
        temp_df5["nFeature_RNA"] <- rval_mod()@meta.data[,nfeat_col]
        
        to_remove_nfeat = which(temp_df5$nFeature_RNA < params$min1 | temp_df5$nFeature_RNA > params$max1)
        
      }else{
        to_remove_nfeat = NULL
      }
      
      if("percent.ribo" %in% colnames(rval_mod()@meta.data)){
        temp_df5$percent.ribo <- rval_mod()@meta.data[,"percent.ribo"]
        to_remove5_rib = which(temp_df5$percent.ribo < params$min4 | temp_df5$percent.ribo > params$max4)
      }else{
        to_remove5_rib = NULL
      }
      
      if("percent.mito" %in% colnames(rval_mod()@meta.data)){
        temp_df5$percent.mito <- rval_mod()@meta.data[,"percent.mito"]
        to_remove5_mito = which(temp_df5$percent.mito < params$min3 | temp_df5$percent.mito > params$max3)
      }else{
        to_remove5_mito = NULL
      }
      
      to_remove5 <- unique(c(to_remove_ncount,to_remove_nfeat,to_remove5_mito,to_remove5_rib))
      
      
      temp_df5[to_remove5,"Cells_to_conserved"] = FALSE
      
      return(temp_df5)
      
    }else{
      NULL
    }
    
    
  })
  
  
  
  ### Configuration of X-axis
  
  paramX <- reactive({
    
    if(length(input$paramX) == 0){
      axisX = NULL
    }else{
      if (input$paramX == "Nb_Gene"){
        
        axisX <- "nFeature_RNA"
        
        
      } else if (input$paramX == "Nb_UMI"){
        
        axisX <- "nCount_RNA"
        
        
      } else if (input$paramX == "Percent_mito"){
        
        axisX <- "percent.mito"
        
        
      } else if (input$paramX == "Percent_ribo"){
        
        axisX <- "percent.ribo"
        
        
      } else{
        axisX <- NULL
      }
      
    }
    
    return(axisX)
  })
  
  
  
  ### Configuration of Y-axis  
  
  paramY <- reactive({
    
    if(length(input$paramY) == 0){
      axisY = NULL
    }else{
      if (input$paramY == "Nb_Gene"){
        
        axisY <- "nFeature_RNA"
        
      } else if (input$paramY == "Nb_UMI"){
        
        axisY <- "nCount_RNA"
        
      } else if (input$paramY == "Percent_mito"){
        
        axisY <- "percent.mito"
        
      } else if(input$paramY == "Percent_ribo"){
        
        axisY <- "percent.ribo"
        
      }else{
        axisY <- NULL
        
      }
    }
    
    return(axisY)
  })
  
  
  
  ### Representation of scater plot
  
 sca_plot <- reactive({
      if(is.null(df_all()) | is.null(paramX()) | is.null(paramY())){
      NULL
    }else{
      
      plot <- ggplot(df_all(), aes(x = df_all()[[paramX()]] , y = df_all()[[paramY()]]))
      
      plot <- plot + geom_point(aes(color = df_all()[["Cells_to_conserved"]]), size = 0.15)
      
      plot <- plot + xlab(input$paramX) + ylab(input$paramY)
      
      plot <- plot + scale_color_manual(values = c("FALSE" = "#D9717D", "TRUE" = "#4DB6D0"), name="Cells_to_conserved")
      
      plot
      
    }


    })
  
  output$scaterplot <- renderPlot({
    sca_plot()

    })

  callModule(downPlotServer, id = "PlotdownScater", data = sca_plot, out_file = "ScaterPLot")
  
  
  ### select parameter in X axis (select input)
  
  output$select_paramX <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval_mod())){
      
      choicesX <- c("Nb_Gene",
                    "Nb_UMI")
      
      if("percent.mito" %in% colnames(rval_mod()@meta.data)){
        choicesX <- c(choicesX,
                      "Percent_mito")
      }
      
      if("percent.ribo" %in% colnames(rval_mod()@meta.data)){
        choicesX <- c(choicesX,
                      "Percent_ribo")
      }
      
      
      selectInput(ns("paramX"), "X Axis",  
                  choicesX, selected = choicesX[1])
      
    }else{
      NULL
    }
    
  })
  
  
  
  ### select parameter in Y axis (select input) 
  
  output$select_paramY <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval_mod())){
      
      choicesY <- c("Nb_Gene",
                    "Nb_UMI")
      
      if("percent.mito" %in% colnames(rval_mod()@meta.data)){
        choicesY <- c(choicesY,
                      "Percent_mito")
      }
      
      if("percent.ribo" %in% colnames(rval_mod()@meta.data)){
        choicesY <- c(choicesY,
                      "Percent_ribo")
      }
      
      selectInput(ns("paramY"), "Y Axis",  
                  choicesY, selected = choicesY[2])
      
    }else{
      NULL
    }
    
  })
  
  
  
  
  
  
  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  qcModal <- function(failed = FALSE, ncells = 0, ns = NULL) {
    
    modalDialog(
      paste0("You will create a new project containing ", ncells, " cells."),
      size = "m",
      easyClose = FALSE,
      HTML("<br><br>"),
      textInput(ns("project_qc"), "Enter new project name", value = paste0(rval$seurat_selected,"_afterQC")),
      
      if (failed)
        div(tags$b("Project name already exist or is invalid (only alphanumeric and _ characters)", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("filter_qc_ok"), "OK")
      )
    )
  }
  
  
  output$button_QC <- renderUI({
    
    ns <- session$ns
    
    # Check if some cells are selected
    if(!is.null(rval_mod())){
      
      ncells <- nrow(df_all()[which(df_all()$Cells_to_conserved == FALSE),])
      
      if(!is.null(ncells) && ncells >= 0){
        
        actionButton(ns("go_QC"), label = paste0("Filter Out ", ncells, " Low Quality Cells"))
        
      }
      
    }
    
    
  })
  
  
  
  observeEvent(input$go_QC, {
    
    ns <- session$ns
    
    if(!is.null(rval_mod())){
      
      ncells <- nrow(df_all()[which(df_all()$Cells_to_conserved == TRUE),])
      
      if(ncells >= 50){
        showModal(qcModal(ncells = ncells, ns = ns))
      }else {
        showModal(
          modalDialog(
            "No sufficient cells to conserve. Try to modify your parameters.",
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
  observeEvent(input$filter_qc_ok, {
    
    ns <- session$ns
    
    # Check that data object exists and is data frame.
    if (!(input$project_qc %in% names(rval$seurat_list)) && input$project_qc != '' && !grepl("[^A-Za-z0-9_]", input$project_qc) == TRUE){
      
      removeModal()
      
      
      # Create modal progress during rds Saving
      show_modal_spinner(text = paste0('Saving ', rval$seurat_selected,'... Please wait...'), spin = 'circle')
      
      saveRDS(rval$seurat, paste0(rval$output,"/",rval$seurat_selected,".rds"))
      
      # Close modal progress when rds is saved
      remove_modal_spinner()
      
      
      
      
      # Create modal progress during object creation
      show_modal_spinner(text = paste0('Creating ', input$project_qc,'... Please wait...'), spin = 'circle')
      
      # rval$parameters[[input$project_qc]] <- c(rval$parameters[[input$project_qc]], "parent" = rval$seurat_selected)
      rval$parameters[[input$project_qc]][["parent"]] <- rval$seurat_selected
      rval$parameters[[input$project_qc]][["after_step"]] <- "qc"
      
      if(!is.null(params$min1)){
        rval$parameters[[input$project_qc]]["min_feat"] = params$min1
      }else{
        rval$parameters[[input$project_qc]]["min_feat"] = floor(min(rval_mod()@meta.data["nFeature_RNA"])/100)*100
      }
      if(!is.null(params$max1)){
        rval$parameters[[input$project_qc]]["max_feat"] = params$max1
      }else{
        rval$parameters[[input$project_qc]]["max_feat"] = ceiling(max(rval_mod()@meta.data["nFeature_RNA"])/100)*100
      }
      if(!is.null(params$min2)){
        rval$parameters[[input$project_qc]]["min_UMI"] = params$min2
      }else{
        rval$parameters[[input$project_qc]]["min_UMI"] = floor(min(rval_mod()@meta.data["nCount_RNA"])/100)*100
      }
      if(!is.null(params$max2)){
        rval$parameters[[input$project_qc]]["max_UMI"] = params$max2
      }else{
        rval$parameters[[input$project_qc]]["max_UMI"] = ceiling(max(rval_mod()@meta.data["nCount_RNA"])/100)*100
      }
      if(!is.null(params$min3)){
        rval$parameters[[input$project_qc]]["min_mito"] = params$min3
      }else{
        rval$parameters[[input$project_qc]]["min_mito"] = 0
      }
      if(!is.null(params$max3)){
        rval$parameters[[input$project_qc]]["max_mito"] = params$max3
      }else{
        rval$parameters[[input$project_qc]]["max_mito"] = ceiling(max(rval_mod()$percent.mito))
      }
      if(!is.null(params$min4)){
        rval$parameters[[input$project_qc]]["min_ribo"] = params$min4
      }else{
        rval$parameters[[input$project_qc]]["min_ribo"] = 0
      }
      if(!is.null(params$max4)){
        rval$parameters[[input$project_qc]]["max_ribo"] = params$max4
      }else{
        rval$parameters[[input$project_qc]]["max_ribo"] = ceiling(max(rval_mod()$percent.ribo))
      }

      rval$parameters[[input$project_qc]]["rm_cells"] = nrow(df_all()[which(df_all()$Cells_to_conserved == FALSE),])
      
      rval$parameters[[input$project_qc]][["after_step"]] <- "qc"
      
      rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "qc")
      
      cells_to_conserve <- rownames(df_all()[which(df_all()$Cells_to_conserved == TRUE),])
      
      # rval$seurat <- subset(seu, cells = cells_to_conserve)
      
      rval$seurat <- subset(rval$seurat, cells = cells_to_conserve)
      
      rval$seurat_list[[input$project_qc]] <- input$project_qc
      
      rval$seurat_selected <- input$project_qc
      
      rval$genes_list[[rval$seurat_selected]][[paste0("All_genes_",rval$seurat_selected)]] = rownames(rval$seurat)
      
      rval$parameters[[rval$seurat_selected]]["is.qc"] = TRUE
      
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
      ncells <- nrow(df_all()[which(df_all()$Cells_to_conserved == TRUE),])
      showModal(qcModal(failed = TRUE, ns = ns, ncells = ncells))
    }
    
  })
  
  
  
  
  #minmax Feature
  
  ## observe numeric input
  # observeEvent(input$qc_nb_featMin,{
  #   params$min1 <- as.numeric(input$qc_nb_featMin)
  # }, ignoreInit = TRUE)
  # 
  # observeEvent(input$qc_nb_featMax,{
  #   params$max1 <- as.numeric(input$qc_nb_featMax)
  # }, ignoreInit = TRUE)
  
  observeEvent(input$updateF_num,{
    params$min1 <- as.numeric(input$qc_nb_featMin)
    params$min2 <- as.numeric(input$qc_nb_UMIMin)
    params$min3 <- as.numeric(input$qc_percent_MitoMin)
    params$min4 <- as.numeric(input$qc_percent_RiboMin)
  }, ignoreInit = TRUE)
  
  observeEvent(input$updateF_num,{
    params$max1 <- as.numeric(input$qc_nb_featMax)
    params$max2 <- as.numeric(input$qc_nb_UMIMax)
    params$max3 <- as.numeric(input$qc_percent_MitoMax)
    params$max4 <- as.numeric(input$qc_percent_RiboMax)
  }, ignoreInit = TRUE)
  
  ## observe button
  observeEvent(input$updateF,{
    params$min1 <- as.numeric(input$qc_nb_feature[1])
    params$min2 <- as.numeric(input$qc_nb_UMI[1])
    params$min3 <- as.numeric(input$qc_percent_Mito[1])
    params$min4 <- as.numeric(input$qc_percent_Ribo[1])
  })
  
  observeEvent(input$updateF,{
    params$max1 <- as.numeric(input$qc_nb_feature[2])
    params$max2 <- as.numeric(input$qc_nb_UMI[2])
    params$max3 <- as.numeric(input$qc_percent_Mito[2])
    params$max4 <- as.numeric(input$qc_percent_Ribo[2])
  })
  
  
  # observeEvent(input$qc_nb_featMin,{
  #    rval$min1 <- input$qc_nb_featMin[1]
  # }, ignoreInit = TRUE)
  # 
  # 
  # observeEvent(input$qc_nb_featMax,{
  #   rval$max1 <- input$qc_nb_featMax[1]
  # }, ignoreInit = TRUE)
  
  
  ## observe reactive
  observeEvent({params$min1; params$max1},{
    updateNumericInput(
      session, "qc_nb_feature", value = c(params$min1, params$max1))
    updateTextInput(session, "qc_nb_featMin", value = params$min1)
    updateTextInput(session, "qc_nb_featMax", value = params$max1)
  }, ignoreInit = TRUE)
  
  
  
  
  
  #minmax UMI
  
  # ## observe slider
  # observeEvent(input$qc_nb_UMI,{
  #   rval$min2 <- input$qc_nb_UMI[1]
  #   rval$max2 <- input$qc_nb_UMI[2]
  # }, ignoreInit = TRUE)
  # 
  # ## observe button
  # observeEvent(input$updateU,{rval$min2 <- as.numeric(input$qc_nb_UMIMin)})
  # observeEvent(input$updateU,{rval$max2 <- as.numeric(input$qc_nb_UMIMax)})
  # 
  # 
  # 
  # ## observe reactive
  # observeEvent({rval$min2; rval$max2},{
  #   updateSliderInput(
  #     session, "qc_nb_UMI", value = c(rval$min2, rval$max2))
  #   updateTextInput(session, "qc_nb_UMIMin", value = rval$min2)
  #   updateTextInput(session, "qc_nb_UMIMax", value = rval$max2)
  # })
  
  
  
  # observeEvent(input$updateU_num,{
  #   params$min2 <- as.numeric(input$qc_nb_UMIMin)
  # }, ignoreInit = TRUE)
  # 
  # observeEvent(input$updateU_num,{
  #   params$max2 <- as.numeric(input$qc_nb_UMIMax)
  # }, ignoreInit = TRUE)
  
  ## observe button
  # observeEvent(input$updateU,{params$min2 <- as.numeric(input$qc_nb_UMI[1])})
  # observeEvent(input$updateU,{params$max2 <- as.numeric(input$qc_nb_UMI[2])})
  
  
  
  ## observe reactive
  observeEvent({params$min2; params$max2},{
    updateNumericInput(
      session, "qc_nb_UMI", value = c(params$min2, params$max2))
    updateTextInput(session, "qc_nb_UMIMin", value = params$min2)
    updateTextInput(session, "qc_nb_UMIMax", value = params$max2)
  }, ignoreInit = TRUE)
  
  
  
  
  
  #minmax percent mito
  
  
  # observeEvent(input$updatePM_num,{
  #   params$min3 <- as.numeric(input$qc_percent_MitoMin)
  # }, ignoreInit = TRUE)
  # 
  # observeEvent(input$updatePM_num,{
  #   params$max3 <- as.numeric(input$qc_percent_MitoMax)
  # }, ignoreInit = TRUE)
  
  ## observe button
  # observeEvent(input$updatePM,{params$min3 <- as.numeric(input$qc_percent_Mito[1])})
  # observeEvent(input$updatePM,{params$max3 <- as.numeric(input$qc_percent_Mito[2])})
  
  
  
  ## observe reactive
  observeEvent({params$min3; params$max3},{
    updateNumericInput(
      session, "qc_percent_Mito", value = c(params$min3, params$max3))
    updateTextInput(session, "qc_percent_MitoMin", value = params$min3)
    updateTextInput(session, "qc_percent_MitoMax", value = params$max3)
  }, ignoreInit = TRUE)
  
  
  
  
  
  
  #minmax percent ribo
  
  
  # observeEvent(input$updatePR_num,{
  #   params$min4 <- as.numeric(input$qc_percent_RiboMin)
  # }, ignoreInit = TRUE)
  # 
  # observeEvent(input$updatePR_num,{
  #   params$max4 <- as.numeric(input$qc_percent_RiboMax)
  # }, ignoreInit = TRUE)
  
  ## observe button
  # observeEvent(input$updatePR,{params$min4 <- as.numeric(input$qc_percent_Ribo[1])})
  # observeEvent(input$updatePR,{params$max4 <- as.numeric(input$qc_percent_Ribo[2])})
  
  
  
  ## observe reactive
  observeEvent({params$min4; params$max4},{
    updateNumericInput(
      session, "qc_percent_Ribo", value = c(params$min4, params$max4))
    updateTextInput(session, "qc_percent_RiboMin", value = params$min4)
    updateTextInput(session, "qc_percent_RiboMax", value = params$max4)
  }, ignoreInit = TRUE)
  
  
  
  
  
  
  return(rval)
}
