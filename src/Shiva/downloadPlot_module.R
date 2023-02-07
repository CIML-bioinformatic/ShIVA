
downPlotUI <- function(id) {
  ns <- NS(id)

  tagList(
    
    radioButtons(ns("fileFormat"), " ",  
                 choices = c("jpeg", "png", "pdf"), 
                 selected = "jpeg", inline = TRUE ),
    
    downloadButton(ns("Plotdown"), "Export", class = "btn btn-primary")
      # choices for file download format
  
  

    )
  
  


}

downPlotServer <- function(input, output, session, data, out_file) {
  

   output$Plotdown <- downloadHandler(
         filename = function(){paste(out_file,input$fileFormat,sep='.')},
         content = function(file){

          ggsave(file, plot=data() + theme_classic(), width = 7, height = 7, units = "in")
        }
    )
}



# downPlotServer <- function(input, output, session, data) {

#   #ns <- session$ns

#     output$Plotdown <- downloadHandler(
#       #ns <- session$ns
#     # file name
#     filename = function(){
#       "plot222.png"
#       #paste("input$plot3",'.png',sep='')
#       },

#     #filename <- "plot222.png", #paste("plot", input$fileFormat, sep="."),
#     # content
#     content = function(file){
#       shiny::withProgress(
#         message = paste0("Downloading ", filename, " .... "),
#         value = 0,
#         {
#           shiny::incProgress(1/10)
#           Sys.sleep(1)
#           shiny::incProgress(6/10)

#           ggsave(file,plot=data())

#           #tempFile <- paste("temp", filename, sep="_")
#           # create plot
#           #export(p = data(), file = paste("temp", filename, sep="_"))
            
#           # hand over the file
#           #file.copy(paste("temp", filename, sep="_"),file)
#         }
#       )
#     }
#     )

    
# }


 
#first add to moduke file by 
#source('downloadPlot_module.R', local = TRUE) #need to test it
#ui of module
#downPlotUI(id = ns("downloadPlot")),
#call the module test it
#callModule(downPlotServer, id = "downloadPlot", data = PLOT)