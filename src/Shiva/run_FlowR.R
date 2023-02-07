#' Run flowR GUI
#' @param modules names of shiny modules (server functions) to include in the GUI
#' @param shiny.maxRequestSize file size limit (in MB)
#' @import shiny
#' @export
#' 
#' 
#' 
run_flowR2 <- function(modules = NULL, shiny.maxRequestSize = 1000){
  
  options(shiny.maxRequestSize = shiny.maxRequestSize * 1024^10)
  
  app <- shinyApp(ui = flowR_ui2(), 
                  server = function(input, output, session){
                    
                    flowR_server2(input = input,
                                 output = output,
                                 session = session,
                                 modules = modules)
                    
                  }
  )
  
  runApp(app, launch.browser = T)
  
}
