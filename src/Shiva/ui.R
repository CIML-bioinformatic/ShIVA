#' flowR app main ui function
#' @import shiny
#' @import shinydashboard
#' @export


ui <- function() {
  
  body <- {
    dashboardBody(
      uiOutput("body")
    )}
  
  sidebar <- {
    dashboardSidebar(
      sidebarMenu(id = "sidebar_tabs", menuItemOutput("menu"))
    )}
  
  ui <- dashboardPage(
    dashboardHeader(title = "ShIVA"),
    sidebar,
    body
  )
  
  return(ui)
}