


downUI <- function(id) {
  ns <- NS(id)
  
  downloadButton(ns("data_download"), label = "Download Table", class = "btn-primary")
  

}

downServer <- function(input, output, session, data, out_file, comm=NULL) {
  
  if(is.null(comm)){
    comm <- "Results were obtained from ShiVA (URL)"
  }
  
  output$data_download <- downloadHandler(
    filename = function() {
      paste(out_file, Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      #####################
    
      con <- file(file, open="wt")#t
      #writeLines(paste("#", comm), con)
      writeLines(paste("#", comm), con)
      
      ###########
      # print(dim(data()))
      
      write.csv(data(), con) # add parentheses to data arg if reactive
      close(con)
    }
  )
}