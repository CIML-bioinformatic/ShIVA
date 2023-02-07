#install PhantumJS
##https://stackoverflow.com/questions/48702133/phantomjs-not-found-on-shiny-server
#https://gist.github.com/telbiyski/ec56a92d7114b8631c906c18064ce620#file-install-phantomjs-2-1-1-ubuntu

ExplorationUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(

    fluidPage(

      fluidRow(#fluid row 1
        h2 ("Explore and visualize the data analysis results in an interactive and comparative way"),
        hr(),
        
        #big box
        box(solidHeader = TRUE,title = "Visualization 1", collapsible = TRUE, collapsed = FALSE,
               height = NULL, width = NULL, status = "primary",
          tabsetPanel(

            tabPanel(p("Two-Dimensional Plot", actionButton(ns("HELP_EXPLO_2D"), "", icon = icon("question-circle"), class = "btn-xs")),
                     br(),
              box(solidHeader = TRUE,title = "Plot Parameters", collapsible = TRUE, collapsed = FALSE,
               height = NULL, status = "primary",
               
               tabBox(width = NULL, title = "",
                tabPanel("Axis",
                  #making X axis
                  uiOutput(ns("radiobox_chooseDataX")),
                  uiOutput(ns("radiobox_chooseFeature")),
                  tags$div(uiOutput(ns("select_all_rd_x")), style = "display:inline-block; width: 250px"),
                  tags$div(HTML("<br>"), style = "display:inline-block"),
                  tags$div(HTML("<br>"), style = "display:inline-block"),
                  tags$div(uiOutput(ns("select_X")), style = "display:inline-block; width: 250px"),
                  br(),
                  br(),

                  #making Y axis
                  uiOutput(ns("radiobox_chooseDataY")),
                  uiOutput(ns("radiobox_chooseFeatureY")),
                  tags$div(uiOutput(ns("select_all_rd_y")),  style="display:inline-block; width: 250px"),
                  tags$div(HTML("<br>"),  style="display:inline-block"),
                  tags$div(HTML("<br>"),  style="display:inline-block"),
                  tags$div(uiOutput(ns("select_Y")),  style="display:inline-block; width: 250px"),
		   #asif
		   uiOutput(ns("dataPoint_size1")),
		   useShinyalert()
                               
                ), # tab panel 1
                      
                tabPanel("Color",
                  uiOutput(ns("checkbox_color")),
                  uiOutput(ns("radio_color1")),
                  uiOutput(ns("radiobox_chooseColor")),
                  uiOutput(ns("select_color"))
                ),
                      
                tabPanel("Layout",
                  uiOutput(ns("checkbox_facet_row")),
                  uiOutput(ns("select_frow")),
                  uiOutput(ns("checkbox_facet_col")),
                  uiOutput(ns("select_fcol"))
                ),

                tabPanel("Theme",
                  uiOutput(ns("radio_theme1"))
                ),

                tabPanel("Selection",
                  # div(style = 'overflow-x: scroll', tableOutput(ns("brush1"))%>% withSpinner(color="#0dc5c1")),
                  tableOutput(ns("brush1"))%>% withSpinner(color="#0dc5c1"),
                  br(),
                  div(style="display:inline-block",uiOutput(ns("button_cells1"))),
                  div(style="display:inline-block",uiOutput(ns("button_cells_unsel")))
                  
                )
                      
              ),#tabBox

              actionButton(ns("rf"),label = "Plot", icon = icon("refresh"))#color = "#0dc5c1",
              ),#box 2 of col 1
              
              conditionalPanel(condition = "input.rf", ns = ns,
                               

              box(title = "Plot", height = NULL,
                solidHeader = TRUE, status = "primary", collapsible = T,
                br(),
              #Defining output
              plotlyOutput(ns("Plot1")) %>% withSpinner(color="#0dc5c1"),
              br(),
              br(),
              uiOutput(ns("exportFormat1")),
              br(),
              uiOutput(ns("exportButton1")),

              
              
              #adding the parameter box
              br()
              )#box 1
              )#conditionalPanel rf

            ), # 1st tabpanel
            tabPanel(p("One-Dimensional Plot", actionButton(ns("HELP_EXPLO_1D"), "", icon = icon("question-circle"), class = "btn-xs")),
                     br(),

              box(solidHeader = TRUE,title = "Plot Parameters", collapsible = TRUE, collapsed = FALSE,
               height = NULL, status = "primary",
               tabBox(width = NULL, title = "",
                      
                      tabPanel("Plot type",
                               
                               uiOutput(ns("radiobox_chooseType3")),
                               uiOutput(ns("radiobox_chooseData3")),
                               uiOutput(ns("radiobox_chooseFeature3")),
                               uiOutput(ns("select_value_3")),
                                           
                                           uiOutput(ns("dataPoint_size3")),
                                           useShinyalert()
                      ),
                      
                      tabPanel("Color",
                               
                               uiOutput(ns("color_3"))
                      ),
                      
                      
                      tabPanel("Theme",
                               
                               uiOutput(ns("radio_theme3"))
                      ),
                      
                      
                      tabPanel("Selection",
                               # div(style = 'overflow-x: scroll', dataTableOutput(ns("brush3"))%>% withSpinner(color="#0dc5c1")),
                               tableOutput(ns("brush3"))%>% withSpinner(color="#0dc5c1"),
                               br(),
                               div(style="display:inline-block",uiOutput(ns("button_cells3"))),
                               div(style="display:inline-block",uiOutput(ns("button_cells_unsel3")))
                      )
                      
                ),#tabBox
               
               
                actionButton(ns("rf2"),label = "Plot", icon = icon("refresh"))#color = "#0dc5c1",
           
              ),
              
              conditionalPanel(condition = "input.rf2", ns = ns, 
           
              box(title = "Plot", height = NULL, solidHeader = TRUE, status = "primary", collapsible = T,
                  br(),
                #Defining output
                plotlyOutput(ns("Plot2")) %>% withSpinner(color="#0dc5c1"),
                br(),
               
                br(),
                uiOutput(ns("exportFormat2")),
                br(),
                uiOutput(ns("exportButton2")),

               
                #adding the parameter box
                br()
              )
              ) # CondPanel input.rf2
            ),
            
            tabPanel(p("Table (Quantitative representation)", actionButton(ns("HELP_EXPLO_TABLE"), "", icon = icon("question-circle"), class = "btn-xs")),
                     br(),
              box(solidHeader = TRUE,title = "Choose parameters for quantitative representation", collapsible = TRUE, collapsed = FALSE,
                height = NULL, status = "primary",

                tabBox(width = NULL, title = "",
                  tabPanel("Table content",
                    uiOutput(ns("radiobox_chooseTypeTab")),
                    # uiOutput(ns("radiobox_chooseData3")),
                    uiOutput(ns("select_value_Tab")),
                    uiOutput(ns("checkbox_Tab")),
                    uiOutput(ns("select_meta_Tab_genes")),
                    uiOutput(ns("select_value_Tab_Split"))
                  )
                ),#tabBox

              actionButton(ns("rf3"),label = "Plot", icon = icon("refresh"))#color = "#0dc5c1",
            
              ),
              
              conditionalPanel(condition = "input.rf3", ns = ns, 
    
              box(title = "Quantitative representation", height = NULL, solidHeader = TRUE, status = "primary", collapsible = T,
                  br(),
                #Defining output
                div(style = 'overflow-x: scroll',dataTableOutput(ns("Plot_DT")) %>% withSpinner(color="#0dc5c1")),
                #adding the parameter box
                br(),
                uiOutput(ns("tab1download")),
                
                br()
              )
              ) # CondPanel input.rf3
            )
          )
        ),#big box of fluid row 1
        br(),
        actionButton(ns("addVis2"),label = "Add Visualization", class = "btn-primary"), 
        br(),
        br()
        
      ), # fluid row1

  # Starting Visualization 2 (Two-dimensional visualization) : vis2 #############################
      fluidRow(
        
        conditionalPanel(condition = "input.addVis2", ns = ns, 

        #big box
        box(solidHeader = TRUE,title = "Visualization 2", collapsible = TRUE, collapsed = FALSE,
          height = NULL, width = NULL, status = "primary",
          tabsetPanel(
            tabPanel("Two-Dimensional visualization",
                     br(),
              box(solidHeader = TRUE,title = "Plot Parameters", collapsible = TRUE, collapsed = FALSE,height = NULL, status = "primary",
                tabBox(width = NULL, title = "",
                  tabPanel("Axis",
                    #making X axis
                    uiOutput(ns("vis2radiobox_chooseDataX")),
                    uiOutput(ns("vis2radiobox_chooseFeatureX")),
                    tags$div(uiOutput(ns("vis2select_all_rd_x")), style = "display:inline-block; width: 250px"),
                    tags$div(HTML("<br>"), style = "display:inline-block"),
                    tags$div(HTML("<br>"), style = "display:inline-block"),
                    tags$div(uiOutput(ns("vis2select_X")), style = "display:inline-block; width: 250px"),
                    br(),
                    br(),

                    #making Y axis
                    uiOutput(ns("vis2radiobox_chooseDataY")),
                    uiOutput(ns("vis2radiobox_chooseFeatureY")),
                    tags$div(uiOutput(ns("vis2select_all_rd_y")),  style="display:inline-block; width: 250px"),
                    tags$div(HTML("<br>"),  style="display:inline-block"),
                    tags$div(HTML("<br>"),  style="display:inline-block"),
                    tags$div(uiOutput(ns("vis2select_Y")),  style="display:inline-block; width: 250px"),
			uiOutput(ns("vis2dataPoint_size")),
                        useShinyalert()
                  ), # tab panel 1

                  tabPanel("Color",
                    uiOutput(ns("vis2checkbox_color")),
                    uiOutput(ns("vis2radio_color1")),
                    uiOutput(ns("vis2radiobox_chooseColor")),
                    uiOutput(ns("vis2select_color"))
                  ),

                  tabPanel("Layout",
                    uiOutput(ns("vis2checkbox_facet_row")),
                    uiOutput(ns("vis2select_frow")),
                    uiOutput(ns("vis2checkbox_facet_col")),
                    uiOutput(ns("vis2select_fcol"))
                  ),
                  tabPanel("Theme",
                    uiOutput(ns("vis2radio_theme1"))
                  ),

                  tabPanel("Selection",
                    # div(style = 'overflow-x: scroll', dataTableOutput(ns("vis2brush1"))%>% 
                    #   withSpinner(color="#0dc5c1")),
                    tableOutput(ns("vis2brush1"))%>% withSpinner(color="#0dc5c1"),
                    br(),
                    div(style="display:inline-block",uiOutput(ns("vis2button_cells1"))),
                    div(style="display:inline-block",uiOutput(ns("vis2button_cells_unsel")))
                  )
                          
                ),#tabBox
                   
                actionButton(ns("vis2rf"),label = "Plot", icon = icon("refresh"))#color = "#0dc5c1",
              ),#box  
              
              conditionalPanel(condition = "input.vis2rf", ns = ns, 
               
              box(title = "Plot", height = NULL, solidHeader = TRUE, status = "primary", collapsible = T,
                  br(),
                #Defining output
                plotlyOutput(ns("vis2Plot")) %>% withSpinner(color="#0dc5c1"),
                br(),
                uiOutput(ns("vis2exportFormat")),
                br(),
                uiOutput(ns("vis2exportButton")),
                #adding the parameter box
                br()
              )
              ) # CondPanel input.vis2rf

            ),#1st tabPanel
            
            tabPanel("One-Dimensional visualization",
                     br(),
              box(solidHeader = TRUE,title = "Plot Parameters", collapsible = TRUE, collapsed = FALSE,
                height = NULL, status = "primary",

                tabBox(width = NULL, title = "",
                  tabPanel("Plot type",
                    uiOutput(ns("vio2radiobox_chooseType3")),
                    uiOutput(ns("vio2radiobox_chooseData3")),
                    uiOutput(ns("vio2radiobox_chooseFeature3")),
                    uiOutput(ns("vio2select_value_3")),
                    uiOutput(ns("vio2dataPoint_size")),
		    useShinyalert()
                  ),

                  tabPanel("Color",
                    uiOutput(ns("vio2color_3"))
                  ),

                  tabPanel("Theme",
                    uiOutput(ns("vio2radio_theme3"))
                  ),

                  tabPanel("Selection",
                    # div(style = 'overflow-x: scroll', dataTableOutput(ns("vio2brush3"))%>% withSpinner(color="#0dc5c1")),
                    tableOutput(ns("vio2brush3"))%>% withSpinner(color="#0dc5c1"),
                    br(),
                    div(style="display:inline-block",uiOutput(ns("vio2button_cells3"))),
                    div(style="display:inline-block",uiOutput(ns("vio2button_cells_unsel3")))
                  )
                ),#tabBox
                
                actionButton(ns("vio2rf2"),label = "Plot", icon = icon("refresh"))#color = "#0dc5c1",
              ),
              
              conditionalPanel(condition = "input.vio2rf2", ns = ns, 

              box(title = "Plot", height = NULL, solidHeader = TRUE, status = "primary", 
                collapsible = T,
                br(),
                #Defining output
                plotlyOutput(ns("vio2Plot2")) %>% withSpinner(color="#0dc5c1"),
                br(),
                uiOutput(ns("vio2exportFormat")),
                br(),
                uiOutput(ns("vio2exportButton")),

                #adding the parameter box
                br()
              )
              ) # CondPanel input.vio2rf2

            ),
            
            tabPanel("Table (Quantitative representation)",
                     br(),
              box(solidHeader = TRUE,title = "Choose parameters for quantitative representation", collapsible = TRUE, collapsed = FALSE,
                height = NULL, status = "primary",
                tabBox(width = NULL, title = "",
                  tabPanel("Table content",
                    uiOutput(ns("tab2radiobox_chooseTypeTab")),
                    # uiOutput(ns("radiobox_chooseData3")),
                    uiOutput(ns("tab2select_value_Tab")),
                    uiOutput(ns("tab2checkbox_Tab")),
                    uiOutput(ns("tab2select_meta_Tab_genes")),
                    uiOutput(ns("tab2select_value_Tab_Split"))
                  )

                ),#tabBox
                actionButton(ns("tab2rf3"),label = "Plot", icon = icon("refresh"))#color = "#0dc5c1",
              ),
              
              conditionalPanel(condition = "input.tab2rf3", ns = ns, 

              box(title = "Quantitative representation", height = NULL, solidHeader = TRUE, status = "primary", collapsible = T,
                  br(),
              #Defining output
              div(style = 'overflow-x: scroll',dataTableOutput(ns("tab2Plot_DT")) %>% withSpinner(color="#0dc5c1")),
              br(),
              uiOutput(ns("tab2download")),

              #adding the parameter box
              br()
             
              )
              ) # CondPanel input.tab2rf3
            )
          ),#tabsetPanel

        ),#big box

        br(),
        actionButton(ns("addVis3"),label = "Add Visualization", class = "btn-primary"), 
        br(),
        br()
        ) # CondPanel input.addVis2
        
        ),# fluid row2

  #vis3 2D
      fluidRow(#fluidRow3
        conditionalPanel(condition = "input.addVis3", ns = ns, 
        #big box
        box(solidHeader = TRUE,title = "Visualization 3", collapsible = TRUE, collapsed = FALSE,
          height = NULL, width = NULL, status = "primary",

          tabsetPanel(
            tabPanel("Two-Dimensional visualization",
                     br(),
              box(solidHeader = TRUE,title = "Plot Parameters", collapsible = TRUE, collapsed = FALSE, height = NULL, 
                status = "primary",
                tabBox(width = NULL, title = "",
                  tabPanel("Axis",
                    #making X axis
                    uiOutput(ns("vis3radiobox_chooseDataX")),
                    uiOutput(ns("vis3radiobox_chooseFeatureX")),
                    tags$div(uiOutput(ns("vis3select_all_rd_x")), style = "display:inline-block; width: 250px"),
                    tags$div(HTML("<br>"), style = "display:inline-block"),
                    tags$div(HTML("<br>"), style = "display:inline-block"),
                    tags$div(uiOutput(ns("vis3select_X")), style = "display:inline-block; width: 250px"),
                    br(),
                    br(),
                    #making Y axis
                    uiOutput(ns("vis3radiobox_chooseDataY")),
                    uiOutput(ns("vis3radiobox_chooseFeatureY")),
                    tags$div(uiOutput(ns("vis3select_all_rd_y")),  style="display:inline-block; width: 250px"),
                    tags$div(HTML("<br>"),  style="display:inline-block"),
                    tags$div(HTML("<br>"),  style="display:inline-block"),
                    tags$div(uiOutput(ns("vis3select_Y")),  style="display:inline-block; width: 250px"),
                    #asif
                    uiOutput(ns("vis3dataPoint_size")),
                    useShinyalert()
                  ), # tab panel 1

                  tabPanel("Color",
                    uiOutput(ns("vis3checkbox_color")),
                    uiOutput(ns("vis3radio_color1")),
                    uiOutput(ns("vis3radiobox_chooseColor")),
                    uiOutput(ns("vis3select_color"))
                  ),

                  tabPanel("Layout",
                    uiOutput(ns("vis3checkbox_facet_row")),
                    uiOutput(ns("vis3select_frow")),
                    uiOutput(ns("vis3checkbox_facet_col")),
                    uiOutput(ns("vis3select_fcol"))
                  ),

                  tabPanel("Theme",
                    uiOutput(ns("vis3radio_theme1"))
                  ),

                  tabPanel("Selection",
                    # div(style = 'overflow-x: scroll', dataTableOutput(ns("vis3brush1"))%>% withSpinner(color="#0dc5c1")),
                    tableOutput(ns("vis3brush1"))%>% withSpinner(color="#0dc5c1"),
                    br(),
                    div(style="display:inline-block",uiOutput(ns("vis3button_cells1"))),
                    div(style="display:inline-block",uiOutput(ns("vis3button_cells_unsel")))
                  )
                ),#tabBox

                actionButton(ns("vis3rf"),label = "Plot", icon = icon("refresh"))#color = "#0dc5c1",
              ),#box 2 of col 1 
              
              conditionalPanel(condition = "input.vis3rf", ns = ns, 
                 
              box(title = "Plot", height = NULL, solidHeader = TRUE, status = "primary", collapsible = T,
                  br(),
                #Defining output
                plotlyOutput(ns("vis3Plot")) %>% withSpinner(color="#0dc5c1"),
                br(),
                uiOutput(ns("vis3exportFormat")),
                br(),
                uiOutput(ns("vis3exportButton")),

                #adding the parameter box
                br(),
              )
              ) # CondPanel input.vis3rf

            ),#1st tabPanel
            
            tabPanel("One-Dimensional visualization",
                     br(),
              box(solidHeader = TRUE,title = "Plot Parameters", collapsible = TRUE, collapsed = FALSE,
                height = NULL, status = "primary",

                tabBox(width = NULL, title = "",
                  tabPanel("Plot type",
                    uiOutput(ns("vio3radiobox_chooseType3")),
                    uiOutput(ns("vio3radiobox_chooseData3")),
                    uiOutput(ns("vio3radiobox_chooseFeature3")),
                    uiOutput(ns("vio3select_value_3")),
                    uiOutput(ns("vio3dataPoint_size")),
                    useShinyalert()
                  ),

                  tabPanel("Color",
                    uiOutput(ns("vio3color_3"))
                  ),

                  tabPanel("Theme",
                    uiOutput(ns("vio3radio_theme3"))
                  ),

                  tabPanel("Selection",
                    # div(style = 'overflow-x: scroll', dataTableOutput(ns("vio3brush3"))%>% 
                    #   withSpinner(color="#0dc5c1")),
                    tableOutput(ns("vio3brush3"))%>% withSpinner(color="#0dc5c1"),
                    br(),
                    div(style="display:inline-block",uiOutput(ns("vio3button_cells3"))),
                    div(style="display:inline-block",uiOutput(ns("vio3button_cells_unsel3")))
                  )
                ),#tabBox
                     
                     
                actionButton(ns("vio3rf2"),label = "Plot", icon = icon("refresh"))#color = "#0dc5c1",
              ),
              
              conditionalPanel(condition = "input.vio3rf2", ns = ns, 

              box(title = "Plot", height = NULL, solidHeader = TRUE, 
                status = "primary", collapsible = T,
                br(),

                #Defining output
                plotlyOutput(ns("vio3Plot2")) %>% withSpinner(color="#0dc5c1"),
                br(),
                uiOutput(ns("vio3exportFormat")),
                br(),
                uiOutput(ns("vio3exportButton")),

                #adding the parameter box
                br()
              )
              ) # CondPanel input.vio3rf2
            ),

            tabPanel("Table (Quantitative representation)",
                     br(),
              box(solidHeader = TRUE,title = "Choose parameters for quantitative representation", collapsible = TRUE, collapsed = FALSE,
                height = NULL, status = "primary",

                tabBox(width = NULL, title = "",
                  tabPanel("Table content",
                    uiOutput(ns("tab3radiobox_chooseTypeTab")),
                    # uiOutput(ns("radiobox_chooseData3")),
                    uiOutput(ns("tab3select_value_Tab")),
                    uiOutput(ns("tab3checkbox_Tab")),
                    uiOutput(ns("tab3select_meta_Tab_genes")),
                    uiOutput(ns("tab3select_value_Tab_Split"))
                  )

                ),#tabBox
                  
                  
                actionButton(ns("tab3rf3"),label = "Plot", icon = icon("refresh"))#color = "#0dc5c1",
              ),
              
              conditionalPanel(condition = "input.tab3rf3", ns = ns, 

              box(title = "Quantitative representation", height = NULL, 
                solidHeader = TRUE, status = "primary", collapsible = T,
                br(),
                #Defining output
                div(style = 'overflow-x: scroll',dataTableOutput(ns("tab3Plot_DT")) %>% withSpinner(color="#0dc5c1")),
                br(),
                uiOutput(ns("tab3download")),
                #adding the parameter box
                br(),
              )
              ) # CondPanel input.tab3rf3
            )
          )#tabsetPanel
        )#big box
        ) # CondPanel input.addVis3
        )# fluid row3  
    )#fluidPage
    
  
      
  
  ) # taglist
} # main function





Exploration <- function(input, output, session, rval) {


  
  ######### Help buttons ############
  
  
  observeEvent(input[["HELP_EXPLO_2D"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_EXPLO_2D"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_EXPLO_2D"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  observeEvent(input[["HELP_EXPLO_1D"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_EXPLO_1D"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_EXPLO_1D"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  observeEvent(input[["HELP_EXPLO_TABLE"]], {
    
    showModal(modalDialog(
      title = HTML(help_infos[which(help_infos$key == "HELP_EXPLO_TABLE"),"title"]),
      HTML(help_infos[which(help_infos$key == "HELP_EXPLO_TABLE"),"value"]),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
  })
  
  
  
  
  

  # Ractive function to recuperate dimensions if x axis is reduction
  
  nbPCACol <- reactive({
    
    if(!is.null(rval$seurat)){
      
      
      if(!is.null(input$radio_data1) && !is.null(input$all_rd) && input$radio_data1 == 1){ # Check if reduction is selected on the radio button
        
        # Return dimensions of the selected reduction in x
        
        return(colnames(Embeddings(rval$seurat, reduction = input$all_rd)))
      }
      
    }
    
  })
  
  # Same for y axis
  
  nbPCACol_y <- reactive({
    
    if(!is.null(rval$seurat)){
      
      if(!is.null(input$radio_data2) && !is.null(input$all_rd_y) && input$radio_data2 == 1){
        
        return(colnames(Embeddings(rval$seurat, reduction = input$all_rd_y)))
        
      }
      
    }
    
  })
  


#for visualization 2
vis2nbPCACol <- reactive({
    
    if(!is.null(rval$seurat)){
      
      
      if(!is.null(input$vis2radio_data1) && !is.null(input$vis2all_rd) && input$vis2radio_data1 == 1){ # Check if reduction is selected on the radio button
        
        # Return dimensions of the selected reduction in x
        
        return(colnames(Embeddings(rval$seurat, reduction = input$vis2all_rd)))
      }
      
    }
    
  })
  
  # Same for y axis
  
  vis2nbPCACol_y <- reactive({
    
    if(!is.null(rval$seurat)){
      
      if(!is.null(input$vis2radio_data2) && !is.null(input$vis2all_rd_y) && input$vis2radio_data2 == 1){
        
        return(colnames(Embeddings(rval$seurat, reduction = input$vis2all_rd_y)))
        
      }
      
    }
    
  })


  #vis3
vis3nbPCACol <- reactive({
    
    if(!is.null(rval$seurat)){
      
      
      if(!is.null(input$vis3radio_data1) && !is.null(input$vis3all_rd) && input$vis3radio_data1 == 1){ # Check if reduction is selected on the radio button
        
        # Return dimensions of the selected reduction in x
        
        return(colnames(Embeddings(rval$seurat, reduction = input$vis3all_rd)))
      }
      
    }
    
  })
  
   # Same for y axis
  
  vis3nbPCACol_y <- reactive({
    
    if(!is.null(rval$seurat)){
      
      if(!is.null(input$vis3radio_data2) && !is.null(input$vis3all_rd_y) && input$vis3radio_data2 == 1){
        
        return(colnames(Embeddings(rval$seurat, reduction = input$vis3all_rd_y)))
        
      }
      
    }
    
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
  
  
  
  # Reactive function to recuperate all features of the object
  
  feature <- reactive({
    
    if(!is.null(rval$seurat)){
      
      features <- NULL
      
      for(assay in Assays(rval$seurat)){ # For all the assays in the object
        
        # Add features of the assay to the other feature
        
        features <- c(features, rownames(rval$seurat[[assay]]))
        
      }
      
      # Return all the features of the object
      
      return(features)
      
    }
    
  })
  
  
  
  # Reactive function to recuperate HVG of the object
  
  feature_hvg <- reactive({
    
    if(!is.null(rval$seurat)){

      features <- VariableFeatures(rval$seurat)
      
      # Return all the features of the object
      
      return(features)
      
    }
    
  })
  

  # Cell selection visu 1
  
  
  # Create variable to conserve selected cells
  selection <- reactiveValues(selected_cells_1 = NULL)
  
  
  # observe if there is a deselect on a plot and in this case remove selected cells
  observe({
    event_data("plotly_doubleclick", priority = "event", source = "plot_vis_1")
    event_data("plotly_deselect", priority = "event", source = "plot_vis_1")
    selection$selected_cells_1 <- NULL
  })
  
  
  
  observe({
    event_data("plotly_doubleclick", priority = "event", source = "plot_vis_4")
    event_data("plotly_deselect", priority = "event", source = "plot_vis_4")
    selection$selected_cells_3 <- NULL
  })
  
  #remove sleection if we slecte anorther project
  observe({
    
    rval$seurat_selected
    selection$selected_cells_1 <- NULL
    selection$selected_cells_3 <- NULL
    
  })
  
  
  
  # observe if cells are selected in plot 1 and update cells to conserve for all other plots 
  # using key column of plotly envent data
  observe({
    
    if(!is.null(event_data("plotly_selected", source = "plot_vis_1"))){
      
      if(is.data.frame((event_data("plotly_selected", source = "plot_vis_1")))){
        selection$selected_cells_1 <- event_data("plotly_selected", source = "plot_vis_1")[,"key"]
      }
      
    }
    
  })
  
  
  
  
  
  output$brush1 <- function() {
    
    if(!is.null(rval$seurat)){
      
      if (!is.null(selection$selected_cells_1)){
        
        kable(as.data.frame(rval$seurat@meta.data[selection$selected_cells_1,]), "html") %>%
          kable_styling(bootstrap_options = c("striped", "hover")) %>%
          scroll_box(width = "100%", height = "450px")
        
      }
    }
  }
  
  
  
  
  # 
  # # plot DT table containing the metadata of cells selected in plot 1
  # output$brush1 <- renderDataTable({
  #   
  #   if(!is.null(rval$seurat)){
  #     
  #     if (!is.null(selection$selected_cells_1)){
  # 
  #       return(as.data.frame(rval$seurat@meta.data[selection$selected_cells_1,]))
  #     }
  #   }
  #   
  # })
  
  
  # Create button after the table if cells are selected
  output$button_cells1 <- renderUI({
    
    ns <- session$ns
    
    # Check if some cells are selected
    if(!is.null(selection$selected_cells_1)){
      
      actionButton(ns("save_select1"), label = "Save selected cells")
      
    }
    
    
  })
  

  output$button_cells_unsel <- renderUI({
    
    ns <- session$ns
    
    # Check if some cells are selected
    if(!is.null(selection$selected_cells_1)){
      
      actionButton(ns("unselect1"), label = "Unselect cells")
      
    }
    
    
  })
  
  
  
  observeEvent(input$unselect1, {
    
    ns <- session$ns
    
    selection$selected_cells_1 <- NULL
  
  }) 
  
  
    
  
  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  cellModal1 <- function(failed = FALSE, ncells = 0, ns = NULL) {
    
    modalDialog(
      paste0("You will create a new cell list containing ", ncells, " cells."),
      size = "m",
      easyClose = FALSE,
      HTML("<br><br>"),
      textInput(ns("list_name1"), "Enter list name", value = ""),
      
      if (failed)
        div(tags$b("List name already exist or is invalid (only alphanumeric and _ characters)", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("create_list1"), "Create list")
      )
    )
  }
  
  
  # When button to save cells is clicked, open a modal
  observeEvent(input$save_select1, {
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      # Recuperate metadata for cells in the selection
      table <- rval$seurat@meta.data[selection$selected_cells_1,]
      # Take only cells in the table after potential filtering
      # table <- table[input$brush1_rows_all,]
      
      ncells <- nrow(table)

      # Open cellModal1
      showModal(cellModal1(ncells = ncells, ns = ns))
    }
    
  }) 
  
  
  # When OK button is pressed, attempt to create new cell list. If successful,
  # remove the modal and create it. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$create_list1, {
    
    ns <- session$ns
    
    # Recuperate metadata for cells in the selection
    table <- rval$seurat@meta.data[selection$selected_cells_1,]
    
    # Take only cells in the table after potential filtering
    # table <- table[input$brush1_rows_all,]
    
    ncells <- nrow(table)
    
    # Create temp variable to check if name list already exist
    name_exist = FALSE
    
    # Check if there is cell list created for the current project
    if(rval$seurat_selected %in% names(rval$cell_list)){
      
      # Check if list name already exist for the project
      if(input$list_name1 %in% names(rval$cell_list[[rval$seurat_selected]])){
        
        # If exist then temp variable become TRUE
        name_exist = TRUE
        
      }
      
    }else{
      
      # If no cell list for the current project then create it
      rval$cell_list[[rval$seurat_selected]] <- list()
      
    }
    
    
    
    # Check the name of list
    if(input$list_name1 != '' && !grepl("[^A-Za-z0-9_]", input$list_name1) == TRUE && name_exist == FALSE){
      
      # Close modal
      removeModal()
      
      
      # Create modal progress during list creation
      show_modal_spinner(text = paste0('Creation of list ', input$list_name1,'... Please wait...'), spin = 'circle')
      
      
      # Add cell name to the list
      rval$cell_list[[rval$seurat_selected]][[input$list_name1]] <- rownames(table)
      
      rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "gating")

      params_gate = list()
      params_gate[["type"]] = "scatter"
      params_gate[["x_axis"]] = input$axisPCA
      params_gate[["y_axis"]] = input$axisPCA_y
      params_gate[["color"]] = input$color
      params_gate[["facet_1"]] = input$facet_row
      params_gate[["facet_2"]] = input$facet_col
      
      rval$parameters[[rval$seurat_selected]][[input$list_name1]] = params_gate
      
      # Selection 2D fisrt vis

      # Create new metadata with list name
      rval$seurat@meta.data[input$list_name1] <- "Not_Selected"
      
      # Check witch cells are in the selection and update metadat
      rval$seurat@meta.data[rownames(table),input$list_name1] <- "Selected"
      
      # Transform the metadata column of the list in factor
      rval$seurat@meta.data[,input$list_name1] <- as.factor(rval$seurat@meta.data[,input$list_name1])
      
      saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))
      
      # Close modal progress when list is created
      remove_modal_spinner()
      
      
    }else{
      
      # If name is note validated, call the modal with failure message
      showModal(cellModal1(failed = TRUE, ncells = ncells, ns = ns))
      
    }
    
    
  })
  

  
  ### Cell selection visu 2
  
  
  
  
  # observe if cells are selected in plot 1 and update cells to conserve for all other plots 
  # using key column of plotly envent data if plot is violin or using x column to find cells in 
  # metadata if plot is an histogram
  observe({
    
    # Recuperate df from event data if selection on the plotly
    selected <- event_data("plotly_selected", source = "plot_vis_4")
    
    
    # Check if cells are selected
    if(!is.null(selected) & is.data.frame(selected)){
      
      # Check if plot is an histogram or not
      if(!is.null(input$radio_type_plot_3) & !is.null(input$value_3)){
        
        # if it is an histogram
        if(input$radio_type_plot_3 == 2){
          
          if("key" %in% colnames(selected)){

            selected <- NULL
            
          }else{
            
            # recuperate the value of color selectd on the plot
            col_levels <- levels(as.factor(as.numeric(selected[,"curveNumber"])+1))
            
            # recuperate the value of x selectd on the plot
            x_levels <- levels(as.factor(selected[,"x"]))
            
            # get rownames of metadata using the selected X and the color on the plots
            
            cells <- rownames(rval$seurat@meta.data[which(rval$seurat@meta.data[,input$value_3] 
              %in% levels(rval$seurat@meta.data[,input$value_3])[as.numeric(x_levels)] &
              rval$seurat@meta.data[,input$color_3] %in% levels(rval$seurat@meta.data[,
                input$color_3])[as.numeric(col_levels)]),])
            
            
            
            # update values of selected cells
            selection$selected_cells_3 <- cells

            
          }
          
          
        }else{
          
          
          if("key" %in% colnames(selected)){
            
            # update value by key if plot is not an histogram
            selection$selected_cells_3 <- selected[,"key"]

          }else{

            selected <- NULL
            
          }
          
        }
        
      }
    }
  })
  
  
  
  
  
  output$brush3 <- function() {
    
    if(!is.null(rval$seurat)){
      if (!is.null(selection$selected_cells_3)){
        
        kable(as.data.frame(rval$seurat@meta.data[selection$selected_cells_3,]), "html") %>%
          kable_styling(bootstrap_options = c("striped", "hover")) %>%
          scroll_box(width = "100%", height = "450px")
        
      }
    }
  }
  
  
  
  
  # 
  # # plot  table containing the metadata of cells selected in plot 3
  # output$brush3 <- renderDataTable({
  #   if(!is.null(rval$seurat)){
  #     if (!is.null(selection$selected_cells_3)){
  #       return(as.data.frame(rval$seurat@meta.data[selection$selected_cells_3,]))
  #     }
  #   }
  # })
  
  
  
  # Create button after the table if cells are selected
  output$button_cells3 <- renderUI({
    
    ns <- session$ns
    
    # Check if some cells are selected
    if(!is.null(selection$selected_cells_3)){
      
      actionButton(ns("save_select3"), label = "Save selected cells")
      
    }
    
    
  })
  

  output$button_cells_unsel3 <- renderUI({
    
    ns <- session$ns
    
    # Check if some cells are selected
    if(!is.null(selection$selected_cells_3)){
      
      actionButton(ns("unselect3"), label = "Unselect cells")
      
    }
    
    
  })
  
  
  observeEvent(input$unselect3, {
    
    ns <- session$ns
    
    selection$selected_cells_3 <- NULL
    
  }) 
  
  
  
  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  cellModal3 <- function(failed = FALSE, ncells = 0, ns = NULL) {
    
    modalDialog(
      paste0("You will create a new cell list containing ", ncells, " cells."),
      size = "m",
      easyClose = FALSE,
      HTML("<br><br>"),
      textInput(ns("list_name3"), "Enter list name", value = ""),
      
      if (failed)
        div(tags$b("List name already exist or is invalid (only alphanumeric and _ characters)", 
          style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("create_list3"), "Create list")
      )
    )
  }
  
  
  
  # When button to save cells is clicked, open a modal
  observeEvent(input$save_select3, {
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      # Recuperate metadata for cells in the selection
      table <- rval$seurat@meta.data[selection$selected_cells_3,]
      
      # Take only cells in the table after potential filtering
      # table <- table[input$brush3_rows_all,]
      
      ncells <- nrow(table)
      
      # Open cellModal3
      showModal(cellModal3(ncells = ncells, ns = ns))
    }
    
  }) 
  
  
  # When OK button is pressed, attempt to create new cell list. If successful,
  # remove the modal and create it. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$create_list3, {
    
    ns <- session$ns
    
    # Recuperate metadata for cells in the selection
    table <- rval$seurat@meta.data[selection$selected_cells_3,]
    
    # Take only cells in the table after potential filtering
    # table <- table[input$brush3_rows_all,]
    
    ncells <- nrow(table)
    
    # Create temp variable to check if name list already exist
    name_exist = FALSE
    
    # Check if there is cell list created for the current project
    if(rval$seurat_selected %in% names(rval$cell_list)){
      
      # Check if list name already exist for the project
      if(input$list_name3 %in% names(rval$cell_list[[rval$seurat_selected]])){
        
        # If exist then temp variable become TRUE
        name_exist = TRUE
        
      }
      
    }else{
      
      # If no cell list for the current project then create it
      rval$cell_list[[rval$seurat_selected]] <- list()
      
    }
    
    
    
    # Check the name of list
    if(input$list_name3 != '' && !grepl("[^A-Za-z0-9_]", input$list_name3) == TRUE && name_exist == FALSE){
      
      # Close modal
      removeModal()
      
      
      # Create modal progress during list creation
      show_modal_spinner(text = paste0('Creation of list ', input$list_name3,'... Please wait...'), spin = 'circle')
      
      # Add cell name to the list
      rval$cell_list[[rval$seurat_selected]][[input$list_name3]] <- rownames(table)
      
      rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "gating")
      
      params_gate = list()
      if(input$radio_type_plot_3 == 1){
        params_gate[["type"]] = "Violin"
      }else if(input$radio_type_plot_3 == 2){
        params_gate[["type"]] = "Histogram"
      }
      params_gate[["for"]] = input$value_3
      params_gate[["color"]] = input$color_3

      rval$parameters[[rval$seurat_selected]][[input$list_name3]] = params_gate
      
      # Create new metadata with list name
      rval$seurat@meta.data[input$list_name3] <- "Not_Selected"
      
      # Check witch cells are in the selection and update metadat
      rval$seurat@meta.data[rownames(table),input$list_name3] <- "Selected"
      
      # Transform the metadata column of the list in factor
      rval$seurat@meta.data[,input$list_name3] <- as.factor(rval$seurat@meta.data[,input$list_name3])
      
      
      saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))
      
      # Close modal progress when list is created
      remove_modal_spinner()
      
      
    }else{
      
      # If name is note validated, call the modal with failure message
      showModal(cellModal3(failed = TRUE, ncells = ncells, ns = ns))
      
    }
    
  })
  
  
  #### X axis
  
  output$radiobox_chooseDataX <- renderUI({
    ns <- session$ns
    if(!is.null(rval$seurat)){
      
      radioButtons(ns("radio_data1"), label = "Choose Type for X Axis", 
                   choices = list("Embeddings" = 1, "Metadata" = 2, "Feature" = 3),
                   selected = 1, inline = TRUE)
      
      
    } # if
    else{NULL}
  })
  
  
  
  
  
  output$radiobox_chooseFeature <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$radio_data1)){
      
      if(input$radio_data1 == 3){
        
        radioButtons(ns("radio_feature"), label = "",
                     choices = list("Enter a Gene Name" = 1, "Select Gene from HVG List" = 2, "Select Gene from Project Genes List" = 3),
                     selected = 1, inline = T)
      }
      
    }else{
      NULL
    }
  })
  
  
  
  
  
  
  # defining the radio button
  
  # fetch reductions from seurat object when reduction is selected in the radio button
  ####create an input using the ductions
  output$select_all_rd_x <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$radio_data1)){
      if(input$radio_data1 == 1){
        reduc <- Reductions(rval$seurat)
        
        if(length(reduc) > 0){
          selectInput(ns("all_rd"), label = "Select Embeddings", 
                      choices = reduc, selected = tail(reduc,1))
        }else{NULL}
        
      }else{NULL}
    }else{NULL}
  })
  
  ####Create select input to choose the axis depending on the result of radio button 1
  
  output$select_X <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$radio_data1)){
      if(input$radio_data1 == 1){
        choicesPCA <- nbPCACol()
        
        selectInput(ns("axisPCA"), "Select X Axis", 
                    choices = choicesPCA, selected = choicesPCA[1])
      }
      else if(input$radio_data1 == 2){
        choicesNumeric <- numericCol()
        selectInput(ns("axisPCA"), "Select X Axis",
                    choices = choicesNumeric, selected = choicesNumeric[1])
        
      }
      else if(input$radio_data1 == 3){
        
        if(!is.null(input$radio_feature)){
          
          if(input$radio_feature == 1){
            
            textInput(ns("axisPCA"), "Enter a Gene Name")
            
          }else if(input$radio_feature == 2){
            
            choicesfeature <- feature_hvg()
            
            # Create select input with the features
            
            selectInput(ns("axisPCA"), "Choose Feature",
                        choicesfeature, selected = choicesfeature[1])
            
            
          }else if(input$radio_feature == 3){
            
            # Recuperate features
            
            choicesfeature <- feature()
            
            # Create select input with the features
            
            selectInput(ns("axisPCA"), "Choose Feature",
                        choicesfeature, selected = choicesfeature[1])
            
          }
          
        }

        
      }else{NULL}
      
    }
    
    
  })
  

  

  
  
  #  starting with Y axis.......
  
  output$radiobox_chooseDataY <- renderUI({
    ns <- session$ns
    
    if (!is.null(rval$seurat)){
      radioButtons(ns("radio_data2"), label = "Choose Type for Y Axis",
                   choices = list("Embeddings" = 1, "Metadata" = 2, "Feature" = 3),selected = 1, inline = T) #
    }else{NULL}
    
  })
  
  
  output$radiobox_chooseFeatureY <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$radio_data2)){
      
      if(input$radio_data2 == 3){
        
        radioButtons(ns("radio_featureY"), label = "",
                     choices = list("Enter a Gene Name" = 1, "Select Gene from HVG List" = 2, "Select Gene from Project Genes List" = 3),
                     selected = 1, inline = T)
      }
      
    }else{
      NULL
    }
  })
  
  
  
  
  
  
  output$select_all_rd_y <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$radio_data2)){
      if(input$radio_data2 == 1){
        reduc <- Reductions(rval$seurat)
        if(length(reduc) > 0){
          
          selectInput(ns("all_rd_y"), label = "Select Embeddings", 
                      choices = reduc, selected = tail(reduc,1))
        }else{NULL}
      }else{NULL}
    }else{NULL}
  })
  
  ##### selcting Y
  
  output$select_Y <- renderUI({
    ns <- session$ns
    if(!is.null(input$radio_data2)){
      if(input$radio_data2 == 1){
        choicesPCA <- nbPCACol_y()
        selectInput(ns("axisPCA_y"), label = "Select Y Axis",
                    choices = choicesPCA, selected = choicesPCA[2])
      }else if(input$radio_data2 == 2){
        choicesNumeric <- numericCol()
        selectInput(ns("axisPCA_y"), "Select Y Axis",
                    choices = choicesNumeric, selected = choicesNumeric[2])
      }else if(input$radio_data2 == 3){
        
        
        if(!is.null(input$radio_featureY)){
          
          if(input$radio_featureY == 1){
            
            textInput(ns("axisPCA_y"), "Enter a Gene Name")
            
          }else if(input$radio_featureY == 2){
            
            choicesfeature <- feature_hvg()
            
            # Create select input with the features
            
            selectInput(ns("axisPCA_y"), "Choose Feature",
                        choicesfeature, selected = choicesfeature[1])
            
            
          }else if(input$radio_featureY == 3){
            
            # Recuperate features
            
            choicesfeature <- feature()
            
            # Create select input with the features
            
            selectInput(ns("axisPCA_y"), "Choose Feature",
                        choicesfeature, selected = choicesfeature[1])
            
          }
          
        }
      }else{NULL}
    }
  })
  
  #show data point input
  output$dataPoint_size1 <- renderUI({
    
    ns <- session$ns
    numericInput(ns("in_dataPoint_size1"), "Set the Size of Data Points in Plot:", value=0.15, min = 0.1, max = 5,  width = "50%")
  })  ####### visual parameters
  
  
  output$checkbox_color <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      checkboxInput(ns("checkBox_col_1"), label = "Color", value = TRUE)
    } else{NULL}
  })
  
  # If color if True, create radio buton to choose type of data to color
  
  output$radio_color1 <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$checkBox_col_1)){
      if(input$checkBox_col_1 == TRUE){
        radioButtons(ns("radio_color1"), label = "By", 
                     choices = list("Metadata" = 1, "Feature" = 2), selected = 1, inline = T)
      }else{NULL}
    }
    
    
  })
  
  
  output$radiobox_chooseColor <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$radio_color1)){
      
      if(input$radio_color1 == 2){
        
        radioButtons(ns("radio_featureColor"), label = "",
                     choices = list("Enter a Gene Name" = 1, "Select Gene from HVG List" = 2, "Select Gene from Project Genes List" = 3),
                     selected = 1, inline = T)
      }
      
    }else{
      NULL
    }
  })
  
  
  output$select_color <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$checkBox_col_1)){
      if(input$checkBox_col_1 == TRUE){
        if(!is.null(input$radio_color1)){
          if(input$radio_color1 == 1){
            
            choices <- c(factorCol(), numericCol())
            
            selectInput(ns("color"), label = "Choose color", 
                        choices = choices, selected = choices[1], width = "50%")
            
          }else if(input$radio_color1 == 2){
            
            if(!is.null(input$radio_featureColor)){
              
              if(input$radio_featureColor == 1){
                
                textInput(ns("color"), "Enter a Gene Name", width = "50%")
                
              }else if(input$radio_featureColor == 2){
                
                choicesfeature <- feature_hvg()
                
                # Create select input with the features
                
                selectInput(ns("color"), "Choose Feature",
                            choicesfeature, selected = choicesfeature[1], width = "50%")
                
                
              }else if(input$radio_featureColor == 3){
                
                # Recuperate features
                
                choicesfeature <- feature()
                
                # Create select input with the features
                
                selectInput(ns("color"), "Choose Feature",
                            choicesfeature, selected = choicesfeature[1], width = "50%")
                
              }
            }
            
          }else {NULL}
        }else{NULL}
      }else{NULL}
    }else{NULL}
    
    
  })
  
  # Radio button to select theme of plot
  
  output$radio_theme1 <- renderUI({
    ns <- session$ns
    radioButtons(ns("theme1"), label = "Theme", choices = list("Classic" = "classic",
                                                               "Gray" = "gray", "Void" = "void", "Minimal" = "minimal", "Dark" = "dark"),
                 selected = "classic", inline = T)
  })
  
  
  # Checkbox to facet by row
  
  output$checkbox_facet_row <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      # Checkbox false by default
      
      checkboxInput(ns("checkBox_frow_1"), label = "Facet 1", value = FALSE)
    }else{
      NULL
    }
    
  })
  
  
  
  # Select input if checkbox facet by row is true
  
  output$select_frow <- renderUI({
    
    ns <- session$ns
    
    
    if(!is.null(input$checkBox_frow_1)){
      
      if(input$checkBox_frow_1 == TRUE){ # Check if checkbox is true
        
        # Recuperate factor column
        
        choices <- factorCol()
        
        # Create select input
        
        selectInput(ns("facet_row"), "Choose facet",
                    choices, selected = choices[1], width = "50%")
        
      }else{
        NULL
      }
      
    }
    
    
    
  })
  
  
  # Same for facet by column
  
  output$checkbox_facet_col <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      if(!is.null(input$checkBox_frow_1)){
        
        if(input$checkBox_frow_1 == TRUE){
          
          checkboxInput(ns("checkBox_fcol_1"), label = "Facet 2", value = FALSE)
          
        }
      }
      
    }else{
      NULL
    }
    
  })
  
  
  
  output$select_fcol <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$checkBox_fcol_1)){
      
      if(input$checkBox_fcol_1 == TRUE){
        
        choices <- factorCol()
        
        selectInput(ns("facet_col"), "Choose facet",
                    choices, selected = choices[1], width = "50%")
        
      }else{
        NULL
      }
    }
    
  })
  
  ##### Create output ######  
  
  
  # Reactive to create dataframe with selected input
  
  df1 <- reactive({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      if(is.null(input$checkBox_col_1)){
        color <- colnames(rval$seurat@meta.data)[1]
      }else{
        color <-input$color
      }
      

      params <- c(input$axisPCA, input$axisPCA_y, color, input$facet_row, input$facet_col)

      # Conserve only unique parameters
      
      params <- unique(params)
      
      # Create dataframe with the value of the object
      
      df <- FetchData(rval$seurat, vars = params)
      
      if(ncol(df) < length(unique(params))){
        
        showModal(modalDialog(
          title = "Error in parameters",
          paste0(paste(setdiff(params,colnames(df)), collapse = ","), " are not present in the object"),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      
      }else{
        
        # Add column on the df with names of cells
        
        df$cells <- rownames(df)
        
        
        for(assay in Assays(rval$seurat)){ # For all the assays in the object
          
          # Remove tag of the assay in column if present
          
          assay <- tolower(assay)
          
          name <- paste0(assay,"_(.*)")
          
          colnames(df) <- gsub(name, "\\1", colnames(df))
          
        }
        
        
        # Return dataframe
        
        return(df)
      }
      
    }
    
    
  })



  # Return plotly
  
  #reactive eveny
  
  ploting1 <- eventReactive( c(input$rf, input$unselect1), {
    ns <- session$ns

    if(!is.null(rval$seurat) & !is.null(df1()) ){ # Check if object and dataframe not null
     
      if(!is.null(input$in_dataPoint_size1) && input$in_dataPoint_size1 >= 0.01 && input$in_dataPoint_size1 <= 5)
      {
        output$Plot_error <- renderText({NULL})
      
      if(!is.null(input$axisPCA) & !is.null(input$axisPCA_y)){ # Check if x and y axis input selectd
        
        if(input$axisPCA != "" & input$axisPCA_y != ""){
          
          plot <-ggplot(df1(), aes_string(x = paste0("`", input$axisPCA, "`"), 
                                          y = paste0("`", input$axisPCA_y, "`"), key = "cells"))
          
          plot <- plot + guides(alpha = FALSE)
          
          if(!is.null(input$checkBox_col_1)){
            
            if(input$checkBox_col_1 == TRUE){ # Check if need color 
              
              # Add colored point with input color
              
              
              if(!is.null(input$color)){
                
                plot <- plot + geom_point(aes_string(color = paste0("`", input$color, "`")), size = input$in_dataPoint_size1)
                
                if(is.numeric(FetchData(rval$seurat,input$color)[1,])){
                  
                  plot <- plot + scale_color_gradient(low = "grey", high = "red")
                  
                }
                
              }else{
                
                plot <- plot + geom_point(size = input$in_dataPoint_size1)
                
              }
              
            }else{
              
              # Add point without color 
              
              plot <- plot + geom_point(size = input$in_dataPoint_size1)
              
            }
            
          }else{
            
            plot <- plot + geom_point(aes_string(color = colnames(rval$seurat@meta.data)[1]), size = input$in_dataPoint_size1)
            
          }
          
          
          
          if(!is.null(input$checkBox_frow_1) & !is.null(input$checkBox_fcol_1)){
            
            if(input$checkBox_frow_1 == TRUE & input$checkBox_fcol_1 == TRUE){ #Check if both facet by row and col
              
              # Add facet by row and col input
              
              plot <- plot + facet_grid(rows = vars(get(input$facet_row)), cols = vars(get(input$facet_col)))
              
            }else if(input$checkBox_frow_1 == TRUE){ # Check if only facet by col
              
              # Add facet wrap
              
              plot <- plot + facet_wrap(~get(input$facet_row))
              
            }
            
            
          }
          
          
          if(!is.null(input$theme1)){
            
            theme <- paste0("theme_",input$theme1)
            
          }else{
            
            theme <- "theme_classic"
            
          }
          
          selection$selected_cells_1 <- NULL
          
          plot <- plot + get(theme)()
          
          
          # transform ggplot to plotly
          
          ggplotly(plot, source = "plot_vis_1")
          
        }
        
      }
      
       }# data point limit
       else{
         shinyalert("Warning!", "The Selected Value for the Data Points Size is Out of Range. Please Select a Value in a Range of 0.01 to 5", type = "warning")
         output$Plot_error <- renderText({"Replot the plot with corrected parameters"})
         NULL
       }
    }
    
  }) 
  

  #plotting the @d plot   
  output$Plot1 <- renderPlotly({
    ploting1()

  })



  #adding the export button
  observeEvent(input$rf, {
    ns <- session$ns
    output$exportButton1 <- renderUI({
      
        downloadButton(ns("down1"), "Download",class = "butt1")
      
      })
    output$exportFormat1 <- renderUI({

       
      # choices for file download format
      radioButtons(ns("fileFormat1"), "Choose file format to download:", 
        choices = c("jpeg", "png", "pdf"), 
        selected = "jpeg", inline = TRUE )


      })
  })

  # #save file in the shiny file conating directory
observeEvent(input$fileFormat1,{
    output$down1 <- downloadHandler(

          filename <- paste("plot", input$fileFormat1, sep="."),
    # content
    content = function(file){
      shiny::withProgress(
        message = paste0("Downloading ", filename, " .... "),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(6/10)

          #tempFile <- paste("temp", filename, sep="_")
          # create plot
          export(p = ploting1(), file = paste("temp", filename, sep="_"))
            
          # hand over the file
          file.copy(paste("temp", filename, sep="_"),file)
        }
      )
    }
    )
})
    



### Second 2D visualization

  # Cell selection visu 1
  # Create variable to conserve selected cells
  vis2selection <- reactiveValues(vis2selected_cells_1 = NULL)
  
  
  # observe if there is a deselect on a plot and in this case remove selected cells
  observe({
    event_data("plotly_doubleclick", priority = "event", source = "plot_vis_2")
    event_data("plotly_deselect", priority = "event", source = "plot_vis_2")
    vis2selection$vis2selected_cells_1 <- NULL
  })
  
  
  
  observe({
    event_data("plotly_doubleclick", priority = "event", source = "plot_vis_5")
    event_data("plotly_deselect", priority = "event", source = "plot_vis_5")
    vis2selection$vis2selected_cells_3 <- NULL
  })
  
  
  observe({
    
    rval$seurat_selected
    vis2selection$vis2selected_cells_1 <- NULL
    vis2selection$vis2selected_cells_3 <- NULL
    
  })
  
  
  
  # observe if cells are selected in plot 1 and update cells to conserve for all other plots 
  # using key column of plotly envent data
  observe({
    
    if(!is.null(event_data("plotly_selected", source = "plot_vis_2"))){
      
      if(is.data.frame((event_data("plotly_selected", source = "plot_vis_2")))){
        vis2selection$vis2selected_cells_1 <- event_data("plotly_selected", source = "plot_vis_2")[,"key"]
        
      }
      
    }
    
  })
  
  
  
  
  output$vis2brush1 <- function() {
    
    if(!is.null(rval$seurat)){
      
      if (!is.null(vis2selection$vis2selected_cells_1)){
        
        kable(as.data.frame(rval$seurat@meta.data[vis2selection$vis2selected_cells_1,]), "html") %>%
          kable_styling(bootstrap_options = c("striped", "hover")) %>%
          scroll_box(width = "100%", height = "450px")
        
      }
    }
  }
  
  
  
  
  # 
  # # plot DT table containing the metadata of cells selected in plot 1
  # output$vis2brush1 <- renderDataTable({
  #   if(!is.null(rval$seurat)){
  #     
  #     if (!is.null(vis2selection$vis2selected_cells_1)){
  #       return(as.data.frame(rval$seurat@meta.data[vis2selection$vis2selected_cells_1,]))
  #       
  #     }
  #   }
  #   
  # })
  
  
  # Create button after the table if cells are selected
  output$vis2button_cells1 <- renderUI({
    
    ns <- session$ns
    
    # Check if some cells are selected
    if(!is.null(vis2selection$vis2selected_cells_1)){
      
      actionButton(ns("vis2save_select1"), label = "Save selected cells")
      
    }
    
    
  })
  
  output$vis2button_cells_unsel <- renderUI({
    
    ns <- session$ns
    
    # Check if some cells are selected
    if(!is.null(vis2selection$vis2selected_cells_1)){
      
      actionButton(ns("vis2unselect1"), label = "Unselect cells")
      
    }
    
    
  })
  
  
  
  observeEvent(input$vis2unselect1, {
    
    ns <- session$ns
    
    vis2selection$vis2selected_cells_1 <- NULL
  
  }) 
  

  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  cellModal2 <- function(failed = FALSE, vis2ncells = 0, ns = NULL) {
    
    modalDialog(
      paste0("You will create a new cell list containing ", vis2ncells, " cells."),
      size = "m",
      easyClose = FALSE,
      HTML("<br><br>"),
      textInput(ns("vis2list_name1"), "Enter list name", value = ""),
      
      if (failed)
        div(tags$b("List name already exist or is invalid (only alphanumeric and _ characters)", 
          style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("vis2create_list1"), "Create list")
      )
    )
  }
  
  
  # When button to save cells is clicked, open a modal
  observeEvent(input$vis2save_select1, {

 
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      # Recuperate metadata for cells in the selection
      table <- rval$seurat@meta.data[vis2selection$vis2selected_cells_1,]

      # Take only cells in the table after potential filtering
      # table <- table[input$vis2brush1_rows_all,]
      
      vis2ncells <- nrow(table)

      # Open cellModal1
      showModal(cellModal2(vis2ncells = vis2ncells, ns = ns))
    }
    
  }) 
  
  
  # When OK button is pressed, attempt to create new cell list. If successful,
  # remove the modal and create it. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$vis2create_list1, {


    ns <- session$ns
    # Recuperate metadata for cells in the selection
    vis2table <- rval$seurat@meta.data[vis2selection$vis2selected_cells_1,]

    # Take only cells in the table after potential filtering
    # vis2table <- vis2table[input$vis2brush1_rows_all,]
    
    vis2ncells <- nrow(vis2table)
    
    # Create temp variable to check if name list already exist
    name_exist = FALSE


    # Check if there is cell list created for the current project
    if(rval$seurat_selected %in% names(rval$cell_list)){
      
      # Check if list name already exist for the project
      if(input$vis2list_name1 %in% names(rval$cell_list[[rval$seurat_selected]])){

        # If exist then temp variable become TRUE
        name_exist = TRUE

      }
      
    }else{
      
      # If no cell list for the current project then create it
      rval$cell_list[[rval$seurat_selected]] <- list()
      
    }
    
    
    
    # Check the name of list
    if(input$vis2list_name1 != '' && !grepl("[^A-Za-z0-9_]", input$vis2list_name1) == TRUE && 
      name_exist == FALSE){

      # Close modal
      removeModal()
      
      
      # Create modal progress during list creation
      show_modal_spinner(text = paste0('Creation of list ', input$vis2list_name1,'... Please wait...'), 
        spin = 'circle')
      
      # Add cell name to the list
      rval$cell_list[[rval$seurat_selected]][[input$vis2list_name1]] <- rownames(vis2table)
      
      rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "gating")
      
      params_gate = list()
      params_gate[["type"]] = "scatter"
      params_gate[["x_axis"]] = input$vis2axisPCA
      params_gate[["y_axis"]] = input$vis2axisPCA_y
      params_gate[["color"]] = input$vis2color
      params_gate[["facet_1"]] = input$vis2facet_row
      params_gate[["facet_2"]] = input$vis2facet_col
      
      rval$parameters[[rval$seurat_selected]][[input$vis2list_name1]] = params_gate
      

      # Create new metadata with list name
      rval$seurat@meta.data[input$vis2list_name1] <- "Not_Selected"

      # Check witch cells are in the selection and update metadat
      rval$seurat@meta.data[rownames(vis2table),input$vis2list_name1] <- "Selected"
      
      # Transform the metadata column of the list in factor
      rval$seurat@meta.data[,input$vis2list_name1] <- as.factor(rval$seurat@meta.data[,
        input$vis2list_name1])
      
      saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))

      # Close modal progress when list is created
      remove_modal_spinner()
      
      
    }else{
      
      # If name is note validated, call the modal with failure message
      showModal(cellModal2(failed = TRUE, ns = ns, vis2ncells = vis2ncells))
      
    }
    
    
  })
  

  
  # ### Cell selection visu 2
  

  # # observe if cells are selected in plot 1 and update cells to conserve for all other plots 
  # # using key column of plotly envent data if plot is violin or using x column to find cells in 
  # metadata if plot is an histogram
  observe({


    # Recuperate df from event data if selection on the plotly
    vis2selected <- event_data("plotly_selected", source = "plot_vis_5")


    
    # Check if cells are selected
    if(!is.null(vis2selected) & is.data.frame(vis2selected)){
      
      # Check if plot is an histogram or not
      if(!is.null(input$vio2radio_type_plot_3) & !is.null(input$vio2value_3)){
        
        # if it is an histogram
        if(input$vio2radio_type_plot_3 == 2){
          
          if("key" %in% colnames(vis2selected)){

            vis2selected <- NULL
            
          }else{
            
            # recuperate the value of color selectd on the plot
            vis2col_levels <- levels(as.factor(as.numeric(vis2selected[,"curveNumber"])+1))
            
            # recuperate the value of x selectd on the plot
            vis2x_levels <- levels(as.factor(vis2selected[,"x"]))
            
            # get rownames of metadata using the selected X and the color on the plots
            
            vis2cells <- rownames(rval$seurat@meta.data[which(rval$seurat@meta.data[,
              input$vio2value_3] %in% levels(rval$seurat@meta.data[,input$vio2value_3])
              [as.numeric(vis2x_levels)] &
              rval$seurat@meta.data[,input$vio2color_3] %in% 
              levels(rval$seurat@meta.data[,input$vio2color_3])[as.numeric(vis2col_levels)]),])
            
            
            
            # update values of selected cells
            vis2selection$vis2selected_cells_3 <- vis2cells

          }
          
          
        }else{
          
          
          if("key" %in% colnames(vis2selected)){
            
            # update value by key if plot is not an histogram

            vis2selection$vis2selected_cells_3 <- vis2selected[,"key"]

          }else{

            vis2selected <- NULL
            
          }

        }
        
      }
    }
  })
  
  
  
  output$vio2brush3 <- function() {
    
    if(!is.null(rval$seurat)){
      if (!is.null(vis2selection$vis2selected_cells_3)){
        
        kable(as.data.frame(rval$seurat@meta.data[vis2selection$vis2selected_cells_3,]), "html") %>%
          kable_styling(bootstrap_options = c("striped", "hover")) %>%
          scroll_box(width = "100%", height = "450px")
        
      }
    }
  }
  
  
  
  
  
  # # plot DT table containing the metadata of cells selected in plot 3
  # output$vio2brush3 <- renderDataTable({
  #   
  #   if(!is.null(rval$seurat)){
  #     if (!is.null(vis2selection$vis2selected_cells_3)){
  #       return(as.data.frame(rval$seurat@meta.data[vis2selection$vis2selected_cells_3,]))
  #     }
  #   }
  # })
  
  
  
  # Create button after the table if cells are selected
  output$vio2button_cells3 <- renderUI({
    
    ns <- session$ns
    
    # Check if some cells are selected
    if(!is.null(vis2selection$vis2selected_cells_3)){
      
      actionButton(ns("vio2save_select3"), label = "Save selected cells")
      
    }
    
    
  })
  

  output$vio2button_cells_unsel3 <- renderUI({
    
    ns <- session$ns
    
    # Check if some cells are selected
    if(!is.null(vis2selection$vis2selected_cells_3)){
      
      actionButton(ns("vio2unselect3"), label = "Unselect cells")
      
    }
    
    
  })
  
  
  
  observeEvent(input$vio2unselect3, {
    
    ns <- session$ns
    
    vis2selection$vis2selected_cells_3 <- NULL
    
  }) 
  
  
  
  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  vis2cellModal3 <- function(failed = FALSE, vis2ncells = 0, ns = NULL) {
    
    modalDialog(
      paste0("You will create a new cell list containing ", vis2ncells, " cells."),
      size = "m",
      easyClose = FALSE,
      HTML("<br><br>"),
      textInput(ns("vis2list_name3"), "Enter list name", value = ""),
      
      if (failed)
        div(tags$b("List name already exist or is invalid (only alphanumeric and _ characters)", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("vis2create_list3"), "Create list")
      )
    )
  }
  
  
  
  # When button to save cells is clicked, open a modal
  observeEvent(input$vio2save_select3, {
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      # Recuperate metadata for cells in the selection
      vis2table <- rval$seurat@meta.data[vis2selection$vis2selected_cells_3,]

      # Take only cells in the table after potential filtering
      # vis2table <- vis2table[input$vio2brush3_rows_all,]

      vis2ncells <- nrow(vis2table)
      
      # Open cellModal3
      showModal(vis2cellModal3(vis2ncells = vis2ncells, ns = ns))
    }
    
  }) 
  
  
  # When OK button is pressed, attempt to create new cell list. If successful,
  # remove the modal and create it. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$vis2create_list3, {
    
    ns <- session$ns
    
    # Recuperate metadata for cells in the selection
    vis2table <- rval$seurat@meta.data[vis2selection$vis2selected_cells_3,]
    
    # Take only cells in the table after potential filtering
    # vis2table <- vis2table[input$vio2brush3_rows_all,]
    
    vis2ncells <- nrow(vis2table)
    
    # Create temp variable to check if name list already exist
    name_exist = FALSE
    
    # Check if there is cell list created for the current project
    if(rval$seurat_selected %in% names(rval$cell_list)){
      
      # Check if list name already exist for the project
      if(input$vis2list_name3 %in% names(rval$cell_list[[rval$seurat_selected]])){
        
        # If exist then temp variable become TRUE
        name_exist = TRUE
        
      }
      
    }else{
      
      # If no cell list for the current project then create it
      rval$cell_list[[rval$seurat_selected]] <- list()
      
    }
    
    
    
    # Check the name of list
    if(input$vis2list_name3 != '' && !grepl("[^A-Za-z0-9_]", input$vis2list_name3) == TRUE && 
      name_exist == FALSE){
      
      # Close modal
      removeModal()
      
      
      # Create modal progress during list creation
      show_modal_spinner(text = paste0('Creation of list ', input$vis2list_name3,'... Please wait...'), spin = 'circle')
      
      # Add cell name to the list
      rval$cell_list[[rval$seurat_selected]][[input$vis2list_name3]] <- rownames(vis2table)
      
      rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "gating")
      
      params_gate = list()
      if(input$vio2radio_type_plot_3 == 1){
        params_gate[["type"]] = "Violin"
      }else if(input$vio2radio_type_plot_3 == 2){
        params_gate[["type"]] = "Histogram"
      }
      params_gate[["for"]] = input$vio2value_3
      params_gate[["color"]] = input$vio2color_3

      rval$parameters[[rval$seurat_selected]][[input$vis2list_name3]] = params_gate

      # Create new metadata with list name
      rval$seurat@meta.data[input$vis2list_name3] <- "Not_Selected"
      
      # Check witch cells are in the selection and update metadat
      rval$seurat@meta.data[rownames(vis2table),input$vis2list_name3] <- "Selected"
      
      # Transform the metadata column of the list in factor
      rval$seurat@meta.data[,input$vis2list_name3] <- as.factor(rval$seurat@meta.data[,
        input$vis2list_name3])
      
      saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))
      
      # Close modal progress when list is created
      remove_modal_spinner()
      
      
    }else{
      
      # If name is note validated, call the modal with failure message
      showModal(vis2cellModal3(failed = TRUE, ns = ns, vis2ncells = vis2ncells))
      
    }
    
  })
  
  #### X axis
  
  output$vis2radiobox_chooseDataX <- renderUI({
    ns <- session$ns
    if(!is.null(rval$seurat)){
      
      radioButtons(ns("vis2radio_data1"), label = "Choose Type for X Axis", 
                   choices = list("Embeddings" = 1, "Metadata" = 2, "Feature" = 3),
                   selected = 1, inline = TRUE)
      
      
    } # if
    else{NULL}
  })
  
  
  
  output$vis2radiobox_chooseFeatureX <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$vis2radio_data1)){
      
      if(input$vis2radio_data1 == 3){
        
        radioButtons(ns("vis2radio_featureX"), label = "",
                     choices = list("Enter a Gene Name" = 1, "Select Gene from HVG List" = 2, "Select Gene from Project Genes List" = 3),
                     selected = 1, inline = T)
      }
      
    }else{
      NULL
    }
  })
  
  
  
  
  
  # definding the radio button
  
  # fetch reductions from seurat object when reduction is slected in the radio button
  ####create an input using the ductions
  output$vis2select_all_rd_x <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$vis2radio_data1)){
      if(input$vis2radio_data1 == 1){
        vis2reduc <- Reductions(rval$seurat)
        
        if(length(vis2reduc) > 0){
          selectInput(ns("vis2all_rd"), label = "Select Embeddings", 
                      choices = vis2reduc, selected = tail(vis2reduc,1))
        }else{NULL}
        
      }else{NULL}
    }else{NULL}
  })
  
  ####Create select input to choose the axis depending on the result of radio button 1
  
  output$vis2select_X <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$vis2radio_data1)){
      if(input$vis2radio_data1 == 1){
        vis2choicesPCA <- vis2nbPCACol()
        
        selectInput(ns("vis2axisPCA"), "Select X Axis", 
                    choices = vis2choicesPCA, selected = vis2choicesPCA[1])
      }
      else if(input$vis2radio_data1 == 2){
        vis2choicesNumeric <- numericCol()
        selectInput(ns("vis2axisPCA"), "Select X Axis",
                    choices = vis2choicesNumeric, selected = vis2choicesNumeric[1])
        
      }
      else if(input$vis2radio_data1 == 3){
        
        if(!is.null(input$vis2radio_featureX)){
          
          if(input$vis2radio_featureX == 1){
            
            textInput(ns("vis2axisPCA"), "Enter a Gene Name")
            
          }else if(input$vis2radio_featureX == 2){
            
            choicesfeature <- feature_hvg()
            
            # Create select input with the features
            
            selectInput(ns("vis2axisPCA"), "Choose Feature",
                        choicesfeature, selected = choicesfeature[1])
            
            
          }else if(input$vis2radio_featureX == 3){
            
            # Recuperate features
            
            choicesfeature <- feature()
            
            # Create select input with the features
            
            selectInput(ns("vis2axisPCA"), "Choose Feature",
                        choicesfeature, selected = choicesfeature[1])
            
          }
          
        }

        
      }else{NULL}
      
    }
    
    
  })
  
  
  #  starting with Y axis.......
  
  output$vis2radiobox_chooseDataY <- renderUI({
    ns <- session$ns
    
    if (!is.null(rval$seurat)){
      radioButtons(ns("vis2radio_data2"), label = "Choose Type for Y Axis",
                   choices = list("Embeddings" = 1, "Metadata" = 2, "Feature" = 3),selected = 1, inline = T) #
    }else{NULL}
    
  })
  
  
  
  output$vis2radiobox_chooseFeatureY <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$vis2radio_data2)){
      
      if(input$vis2radio_data2 == 3){
        
        radioButtons(ns("vis2radio_featureY"), label = "",
                     choices = list("Enter a Gene Name" = 1, "Select Gene from HVG List" = 2, "Select Gene from Project Genes List" = 3),
                     selected = 1, inline = T)
      }
      
    }else{
      NULL
    }
  })
  
  
  
  
  
  output$vis2select_all_rd_y <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$vis2radio_data2)){
      if(input$vis2radio_data2 == 1){
        vis2reduc <- Reductions(rval$seurat)
        if(length(vis2reduc) > 0){
          
          selectInput(ns("vis2all_rd_y"), label = "Select Embeddings", 
                      choices = vis2reduc, selected = tail(vis2reduc,1))
        }else{NULL}
      }else{NULL}
    }else{NULL}
  })
  
  ##### selcting Y
  
  output$vis2select_Y <- renderUI({
    ns <- session$ns
    if(!is.null(input$vis2radio_data2)){
      if(input$vis2radio_data2 == 1){
        vis2choicesPCA <- vis2nbPCACol_y()
        selectInput(ns("vis2axisPCA_y"), label = "Select Y Axis",
                    choices = vis2choicesPCA, selected = vis2choicesPCA[2])
      }else if(input$vis2radio_data2 == 2){
        vis2choicesNumeric <- numericCol()
        selectInput(ns("vis2axisPCA_y"), "Select Y Axis",
                    choices = vis2choicesNumeric, selected = vis2choicesNumeric[2])
      }else if(input$vis2radio_data2 == 3){
        
        if(!is.null(input$vis2radio_featureY)){
          
          if(input$vis2radio_featureY == 1){
            
            textInput(ns("vis2axisPCA_y"), "Enter a Gene Name")
            
          }else if(input$vis2radio_featureY == 2){
            
            choicesfeature <- feature_hvg()
            
            # Create select input with the features
            
            selectInput(ns("vis2axisPCA_y"), "Choose Feature",
                        choicesfeature, selected = choicesfeature[1])
            
            
          }else if(input$vis2radio_featureY == 3){
            
            # Recuperate features
            
            choicesfeature <- feature()
            
            # Create select input with the features
            
            selectInput(ns("vis2axisPCA_y"), "Choose Feature",
                        choicesfeature, selected = choicesfeature[1])
            
          }
          
        }

      }else{NULL}
    }
  })
  
  #asif data points
  
  #show data point input
  output$vis2dataPoint_size <- renderUI({
    
    ns <- session$ns
    numericInput(ns("vis2in_dataPoint_size"), "Set the Size of Data Points in Plot:", value=0.15, min = 0.1, max = 5,  width = "50%")
  })  ####### visual parameters
  
  ####### visual parameters
  output$vis2checkbox_color <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      checkboxInput(ns("vis2checkBox_col_1"), label = "Color", value = TRUE)
    } else{NULL}
  })
  
  # If color if True, create radio buton to choose type of data to color
  
  output$vis2radio_color1 <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$vis2checkBox_col_1)){
      if(input$vis2checkBox_col_1 == TRUE){
        radioButtons(ns("vis2radio_color1"), label = "By", 
                     choices = list("Metadata" = 1, "Feature" = 2), selected = 1, inline = T)
      }else{NULL}
    }
    
    
  })
  
  
  
  output$vis2radiobox_chooseColor <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$vis2radio_color1)){
      
      if(input$vis2radio_color1 == 2){
        
        radioButtons(ns("vis2radio_featureColor"), label = "",
                     choices = list("Enter a Gene Name" = 1, "Select Gene from HVG List" = 2, "Select Gene from Project Genes List" = 3),
                     selected = 1, inline = T)
      }
      
    }else{
      NULL
    }
  })
  
  
  
  output$vis2select_color <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$vis2checkBox_col_1)){
      if(input$vis2checkBox_col_1 == TRUE){
        if(!is.null(input$vis2radio_color1)){
          if(input$vis2radio_color1 == 1){
            
            vis2choices <- c(factorCol(), numericCol())
            
            selectInput(ns("vis2color"), label = "Choose color", 
                        choices = vis2choices, selected = vis2choices[1], width = "50%")
            
          }else if(input$vis2radio_color1 == 2){
            
            if(!is.null(input$vis2radio_featureColor)){
              if(input$vis2radio_featureColor == 1){
                
                textInput(ns("vis2color"), "Enter a Gene Name", width = "50%")
                
              }else if(input$vis2radio_featureColor == 2){
                
                choicesfeature <- feature_hvg()
                
                # Create select input with the features
                
                selectInput(ns("vis2color"), "Choose Feature",
                            choicesfeature, selected = choicesfeature[1], width = "50%")
                
                
              }else if(input$vis2radio_featureColor == 3){
                
                # Recuperate features
                
                choicesfeature <- feature()
                
                # Create select input with the features
                
                selectInput(ns("vis2color"), "Choose Feature",
                            choicesfeature, selected = choicesfeature[1], width = "50%")
                
              }
              
            }
            
          }else {NULL}
        }else{NULL}
      }else{NULL}
    }else{NULL}
    
    
  })
  
  # Radio button to select theme of plot
  
  output$vis2radio_theme1 <- renderUI({
    ns <- session$ns
    radioButtons(ns("vis2theme1"), label = "Theme", choices = list("Classic" = "classic",
                                                               "Gray" = "gray", "Void" = "void", "Minimal" = "minimal", "Dark" = "dark"),
                 selected = "classic", inline = T)
  })
  
  
  # Checkbox to facet by row
  
  output$vis2checkbox_facet_row <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      # Checkbox false by default
      
      checkboxInput(ns("vis2checkBox_frow_1"), label = "Facet 1", value = FALSE)
    }else{
      NULL
    }
    
  })
  
  
  
  # Select input if checkbox facet by row is true
  
  output$vis2select_frow <- renderUI({
    
    ns <- session$ns
    
    
    if(!is.null(input$vis2checkBox_frow_1)){
      
      if(input$vis2checkBox_frow_1 == TRUE){ # Check if checkbox is true
        
        # Recuperate factor column
        
        vis2choices <- factorCol()
        
        # Create select input
        
        selectInput(ns("vis2facet_row"), "Choose facet",
                    vis2choices, selected = vis2choices[1], width = "50%")
        
      }else{
        NULL
      }
      
    }
    
    
    
  })
  
  
  # Same for facet by column
  
  output$vis2checkbox_facet_col <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      if(!is.null(input$vis2checkBox_frow_1)){
        
        if(input$vis2checkBox_frow_1 == TRUE){
          
          checkboxInput(ns("vis2checkBox_fcol_1"), label = "Facet 2", value = FALSE)
          
        }
      }
      
    }else{
      NULL
    }
    
  })
  
  
  
  output$vis2select_fcol <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$vis2checkBox_fcol_1)){
      
      if(input$vis2checkBox_fcol_1 == TRUE){
        
        vis2choices <- factorCol()
        
        selectInput(ns("vis2facet_col"), "Choose facet",
                    vis2choices, selected = vis2choices[1], width = "50%")
        
      }else{
        NULL
      }
    }
    
  })
  
  ##### Create output ######  
  
  
  # Reactive to create dataframe with selected input
  
  vis2df1 <- reactive({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      if(is.null(input$vis2checkBox_col_1)){
        vis2color <- colnames(rval$seurat@meta.data)[1]
      }else{
        vis2color <-input$vis2color
      }
      
      
      
      vis2params <- c(input$vis2axisPCA, input$vis2axisPCA_y, vis2color, input$vis2facet_row, input$vis2facet_col)

      # Conserve only unique parameters
      
      vis2params <- unique(vis2params)
      
      # Create dataframe with the value of the object
      
      vis2df <- FetchData(rval$seurat, vars = vis2params)
      
      if(ncol(vis2df) < length(unique(vis2params))){
        
        showModal(modalDialog(
          title = "Error in parameters",
          paste0(paste(setdiff(vis2params,colnames(vis2df)), collapse = ","), " are not present in the object"),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        
      }else{
        
        # Add column on the df with names of cells
        
        vis2df$cells <- rownames(vis2df)
        
        
        for(assay in Assays(rval$seurat)){ # For all the assays in the object
          
          # Remove tag of the assay in column if present
          
          vis2assay <- tolower(assay)
          
          vis2name <- paste0(vis2assay,"_(.*)")
          
          colnames(vis2df) <- gsub(vis2name, "\\1", colnames(vis2df))
          
        }
        
        
        # Return dataframe
        
        return(vis2df)
        
      }
      
    }
    
    
  })
  
  # Return plotly
  
  #reactive eveny
  
  vis2ploting <- eventReactive( c(input$vis2rf, input$vis2unselect1), {
    ns <- session$ns
    
    ##### Check for data point size
    if(!is.null(input$vis2in_dataPoint_size) && input$vis2in_dataPoint_size >= 0.01 && input$vis2in_dataPoint_size <= 5){
      output$vis2Plot_error <- renderText({NULL})
      
      
    # ######################
    if(!is.null(rval$seurat) & !is.null(vis2df1()) ){ # Check if object and dataframe not null
      
      if(!is.null(input$vis2axisPCA) & !is.null(input$vis2axisPCA_y)){ # Check if x and y axis input selectd
        
        if(input$vis2axisPCA != "" & input$vis2axisPCA_y != ""){
          
          vis2plot <-ggplot(vis2df1(), aes_string(x = paste0("`", input$vis2axisPCA, "`"), 
                                          y = paste0("`", input$vis2axisPCA_y, "`"), key = "cells"))
          
          vis2plot <- vis2plot + guides(alpha = FALSE)
          
          
          
          
          if(!is.null(input$vis2checkBox_col_1)){
            
            if(input$vis2checkBox_col_1 == TRUE){ # Check if need color 
              
              # Add colored point with input color
              
              
              if(!is.null(input$vis2color)){
                
                vis2plot <- vis2plot + geom_point(aes_string(color = paste0("`", input$vis2color, "`")), size = input$vis2in_dataPoint_size)
                
                if(is.numeric(FetchData(rval$seurat,input$vis2color)[1,])){
                  
                  vis2plot <- vis2plot + scale_color_gradient(low = "grey", high = "red")
                  
                }
                
              }else{
                
                vis2plot <- vis2plot + geom_point(size = input$vis2in_dataPoint_size)
                
              }
              
            }else{
              
              # Add point without color 
              
              vis2plot <- vis2plot + geom_point(size = input$vis2in_dataPoint_size)
              
            }
            
          }else{
            
            vis2plot <- vis2plot + geom_point(aes_string(color = colnames(rval$seurat@meta.data)[1]), size = input$vis2in_dataPoint_size)
            
          }
          
          
          
          if(!is.null(input$vis2checkBox_frow_1) & !is.null(input$vis2checkBox_fcol_1)){
            
            if(input$vis2checkBox_frow_1 == TRUE & input$vis2checkBox_fcol_1 == TRUE){ #Check if both facet by row and col
              
              # Add facet by row and col input
              
              vis2plot <- vis2plot + facet_grid(rows = vars(get(input$vis2facet_row)), cols = vars(get(input$vis2facet_col)))
              
            }else if(input$vis2checkBox_frow_1 == TRUE){ # Check if only facet by col
              
              # Add facet wrap
              
              vis2plot <- vis2plot + facet_wrap(~get(input$vis2facet_row))
              
            }
            
            
          }
          
          
          if(!is.null(input$vis2theme1)){
            
            vis2theme <- paste0("theme_",input$vis2theme1)
            
          }else{
            
            vis2theme <- "theme_classic"
            
          }
          
          
          vis2selection$vis2selected_cells_1 <- NULL
          
          vis2plot <- vis2plot + get(vis2theme)()
          
          
          # transform ggplot to plotly
          
          ggplotly(vis2plot, source = "plot_vis_2")
          
        }
        
      }
      
    }
    
    }# data point limit
    else{shinyalert("Warning!", "The Selected Value for the Data Points Size is Out of Range. Please Select a Value in a Range of 0.01 to 5", type = "warning")
      output$vis2Plot_error <- renderText({"Replot the plot with corrected parameters"})
      NULL
      
      
    }
    
    ##############################
  }) 
  
  
  output$vis2Plot <- renderPlotly({
    vis2ploting()
  } )
 
  


  #adding the export button
  observeEvent(input$vis2rf, {
    ns <- session$ns
    output$vis2exportButton <- renderUI({
      
        downloadButton(ns("vis2down1"), "Download",class = "butt1")
      
      })
    output$vis2exportFormat <- renderUI({

       
      # choices for file download format
      radioButtons(ns("vis2fileFormat1"), "Choose file format to download:", 
        choices = c("jpeg", "png", "pdf"), 
        selected = "jpeg", inline = TRUE )


      })
  })

  # #save file in the shiny file conating directory

observeEvent(input$vis2fileFormat1,{


    output$vis2down1 <- downloadHandler(
      #ns <- session$ns
    # file name

    vis2filename <- paste("plot", input$vis2fileFormat1, sep="."),
    # content
    content = function(file){
      shiny::withProgress(
        message = paste0("Downloading ", vis2filename, " .... "),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(6/10)

          #tempFile <- paste("temp", filename, sep="_")
          # create plot
          export(p = vis2ploting(), file = paste("temp", vis2filename, sep="_"))
            
          # hand over the file
          file.copy(paste("temp", vis2filename, sep="_"),file)
        }
      )
    }
    )
})


#Third 2D visualization

  
  # Create variable to conserve selected cells
  vis3selection <- reactiveValues(vis3selected_cells_1 = NULL)
  
  
  # observe if there is a deselect on a plot and in this case remove selected cells
  observe({
    event_data("plotly_doubleclick", priority = "event", source = "plot_vis_3")
    event_data("plotly_deselect", priority = "event", source = "plot_vis_3")
    vis3selection$vis3selected_cells_1 <- NULL
  })
  
  
  
  observe({
    event_data("plotly_doubleclick", priority = "event", source = "plot_vis_6")
    event_data("plotly_deselect", priority = "event", source = "plot_vis_6")
    vis3selection$vis3selected_cells_3 <- NULL
  })
  
  
  observe({
    
    rval$seurat_selected
    vis3selection$vis3selected_cells_1 <- NULL
    vis3selection$vis3selected_cells_3 <- NULL
    
  })
  
  
  
  # observe if cells are selected in plot 1 and update cells to conserve for all other plots 
  # using key column of plotly envent data
  observe({
    
    if(!is.null(event_data("plotly_selected", source = "plot_vis_3"))){
      
      if(is.data.frame((event_data("plotly_selected", source = "plot_vis_3")))){
        vis3selection$vis3selected_cells_1 <- event_data("plotly_selected", source = "plot_vis_3")[,"key"]
        
      }
      
    }
    
  })
  
  
  
  
  output$vis3brush1 <- function() {
    
    if(!is.null(rval$seurat)){
      
      if (!is.null(vis3selection$vis3selected_cells_1)){
        
        kable(as.data.frame(rval$seurat@meta.data[vis3selection$vis3selected_cells_1,]), "html") %>%
          kable_styling(bootstrap_options = c("striped", "hover")) %>%
          scroll_box(width = "100%", height = "450px")
        
      }
    }
  }
  
  
  
  # 
  # # plot DT table containing the metadata of cells selected in plot 1
  # output$vis3brush1 <- renderDataTable({
  #   
  #   
  #   if(!is.null(rval$seurat)){
  #     
  #     if (!is.null(vis3selection$vis3selected_cells_1)){
  #       return(as.data.frame(rval$seurat@meta.data[vis3selection$vis3selected_cells_1,]))
  #       
  #       # return(datatable(rval$seurat@meta.data[vis3selection$vis3selected_cells_1,], 
  #       #                  caption = "Selected cells vis3", filter = "top",
  #       #                  options = list(pageLength = 10)))
  #     }
  #   }
  #   
  # })
  
  
  # Create button after the table if cells are selected
  output$vis3button_cells1 <- renderUI({
    
    ns <- session$ns
    
    # Check if some cells are selected
    if(!is.null(vis3selection$vis3selected_cells_1)){
      
      actionButton(ns("vis3save_select1"), label = "Save selected cells")
      
    }
    
    
  })
  
  
  
  
  output$vis3button_cells_unsel <- renderUI({
    
    ns <- session$ns
    
    # Check if some cells are selected
    if(!is.null(vis3selection$vis3selected_cells_1)){
      
      actionButton(ns("vis3unselect1"), label = "Unselect cells")
      
    }
    
    
  })
  
  
  
  observeEvent(input$vis3unselect1, {
    
    ns <- session$ns
    
    vis3selection$vis3selected_cells_1 <- NULL
  
  }) 
  
  
    
  
  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  vis3cellModal2 <- function(failed = FALSE, vis3ncells = 0, ns = NULL) {
    
    modalDialog(
      paste0("You will create a new cell list containing ", vis3ncells, " cells."),
      size = "m",
      easyClose = FALSE,
      HTML("<br><br>"),
      textInput(ns("vis3list_name1"), "Enter list name", value = ""),
      
      if (failed)
        div(tags$b("List name already exist or is invalid (only alphanumeric and _ characters)", 
          style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("vis3create_list1"), "Create list")
      )
    )
  }
  
  
  # When button to save cells is clicked, open a modal
  observeEvent(input$vis3save_select1, {


    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      # Recuperate metadata for cells in the selection
      table <- rval$seurat@meta.data[vis3selection$vis3selected_cells_1,]

      # Take only cells in the table after potential filtering
      # table <- table[input$vis3brush1_rows_all,]
      
      vis3ncells <- nrow(table)

      # Open cellModal1
      showModal(vis3cellModal2(vis3ncells = vis3ncells, ns = ns))
    }
    
  }) 
  
  
  # When OK button is pressed, attempt to create new cell list. If successful,
  # remove the modal and create it. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$vis3create_list1, {


    ns <- session$ns
    # Recuperate metadata for cells in the selection
    vis3table <- rval$seurat@meta.data[vis3selection$vis3selected_cells_1,]

    # Take only cells in the table after potential filtering
    # vis3table <- vis3table[input$vis3brush1_rows_all,]
    
    vis3ncells <- nrow(vis3table)
    
    # Create temp variable to check if name list already exist
    name_exist = FALSE


    # Check if there is cell list created for the current project
    if(rval$seurat_selected %in% names(rval$cell_list)){
      
      # Check if list name already exist for the project

      if(input$vis3list_name1 %in% names(rval$cell_list[[rval$seurat_selected]])){


        # If exist then temp variable become TRUE
        name_exist = TRUE

      }
      
    }else{
      
      # If no cell list for the current project then create it
      rval$cell_list[[rval$seurat_selected]] <- list()
      
    }
    
    
    
    # Check the name of list
    if(input$vis3list_name1 != '' && !grepl("[^A-Za-z0-9_]", input$vis3list_name1) == TRUE && 
      name_exist == FALSE){

      # Close modal
      removeModal()
      
      
      # Create modal progress during list creation
      show_modal_spinner(text = paste0('Creation of list ', input$vis3list_name1,'... Please wait...'), 
        spin = 'circle')
      
      # Add cell name to the list
      rval$cell_list[[rval$seurat_selected]][[input$vis3list_name1]] <- rownames(vis3table)
      
      rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "gating")
      
      params_gate = list()
      params_gate[["type"]] = "scatter"
      params_gate[["x_axis"]] = input$vis3axisPCA
      params_gate[["y_axis"]] = input$vis3axisPCA_y
      params_gate[["color"]] = input$vis3color
      params_gate[["facet_1"]] = input$vis3facet_row
      params_gate[["facet_2"]] = input$vis3facet_col
      
      rval$parameters[[rval$seurat_selected]][[input$vis3list_name1]] = params_gate

      # Create new metadata with list name
      rval$seurat@meta.data[input$vis3list_name1] <- "Not_Selected"

      # Check witch cells are in the selection and update metadat
      rval$seurat@meta.data[rownames(vis3table),input$vis3list_name1] <- "Selected"
      
      # Transform the metadata column of the list in factor
      rval$seurat@meta.data[,input$vis3list_name1] <- as.factor(rval$seurat@meta.data[,
        input$vis3list_name1])

      saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))
      
      # Close modal progress when list is created
      remove_modal_spinner()
      
      
    }else{
      
      # If name is note validated, call the modal with failure message
      showModal(cellModal2(failed = TRUE, ns = ns, vis3ncells = vis3ncells))
      
    }
    
    
  })
  

  
  # ### Cell selection visu 2
  

  # # observe if cells are selected in plot 1 and update cells to conserve for all other plots 
  # # using key column of plotly envent data if plot is violin or using x column to find cells in 
  # metadata if plot is an histogram
  observe({


    # Recuperate df from event data if selection on the plotly
    vis3selected <- event_data("plotly_selected", source = "plot_vis_6")


    
    # Check if cells are selected
    if(!is.null(vis3selected) & is.data.frame(vis3selected)){
      
      # Check if plot is an histogram or not
      if(!is.null(input$vio3radio_type_plot_3) & !is.null(input$vio3value_3)){
        
        # if it is an histogram
        if(input$vio3radio_type_plot_3 == 2){
          
          if("key" %in% colnames(vis3selected)){

            vis3selected <- NULL
            
          }else{
            
            # recuperate the value of color selectd on the plot
            vis3col_levels <- levels(as.factor(as.numeric(vis3selected[,"curveNumber"])+1))
            
            # recuperate the value of x selectd on the plot
            vis3x_levels <- levels(as.factor(vis3selected[,"x"]))
            
            # get rownames of metadata using the selected X and the color on the plots
            
            vis3cells <- rownames(rval$seurat@meta.data[which(rval$seurat@meta.data[,
              input$vio3value_3] %in% levels(rval$seurat@meta.data[,input$vio3value_3])
              [as.numeric(vis3x_levels)] &
              rval$seurat@meta.data[,input$vio3color_3] %in% 
              levels(rval$seurat@meta.data[,input$vio3color_3])[as.numeric(vis3col_levels)]),])
            
            
            
            # update values of selected cells
            vis3selection$vis3selected_cells_3 <- vis3cells

 
            
          }
          
          
        }else{
          
          
          if("key" %in% colnames(vis3selected)){
            
            # update value by key if plot is not an histogram

            vis3selection$vis3selected_cells_3 <- vis3selected[,"key"]

          }else{

            vis3selected <- NULL
            
          }
          
        }
        
      }
    }
  })
  
  
  
  
  output$vio3brush3 <- function() {
    
    if(!is.null(rval$seurat)){
      if (!is.null(vis3selection$vis3selected_cells_3)){
        
        kable(as.data.frame(rval$seurat@meta.data[vis3selection$vis3selected_cells_3,]), "html") %>%
          kable_styling(bootstrap_options = c("striped", "hover")) %>%
          scroll_box(width = "100%", height = "450px")
        
      }
    }
  }
  
  
  
  # 
  # # plot DT table containing the metadata of cells selected in plot 3
  # output$vio3brush3 <- renderDataTable({
  #   
  #   if(!is.null(rval$seurat)){
  #     if (!is.null(vis3selection$vis3selected_cells_3)){
  #       return(as.data.frame(rval$seurat@meta.data[vis3selection$vis3selected_cells_3,]))
  #     }
  #   }
  # })
  
  
  
  # Create button after the table if cells are selected
  output$vio3button_cells3 <- renderUI({
    
    ns <- session$ns
    
    # Check if some cells are selected
    if(!is.null(vis3selection$vis3selected_cells_3)){
      
      actionButton(ns("vio3save_select3"), label = "Save selected cells")
      
    }
    
    
  })
  
  
  
  
  
  
  
  output$vio3button_cells_unsel3 <- renderUI({
    
    ns <- session$ns
    
    # Check if some cells are selected
    if(!is.null(vis3selection$vis3selected_cells_3)){
      
      actionButton(ns("vio3unselect3"), label = "Unselect cells")
      
    }
    
    
  })
  
  
  
  observeEvent(input$vio3unselect3, {
    
    ns <- session$ns
    
    vis3selection$vis3selected_cells_3 <- NULL
    
  }) 
  
  
  
  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  vis3cellModal3 <- function(failed = FALSE, vis3ncells = 0, ns = NULL) {
    
    modalDialog(
      paste0("You will create a new cell list containing ", vis3ncells, " cells."),
      size = "m",
      easyClose = FALSE,
      HTML("<br><br>"),
      textInput(ns("vis3list_name3"), "Enter list name", value = ""),
      
      if (failed)
        div(tags$b("List name already exist or is invalid (only alphanumeric and _ characters)", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("vis3create_list3"), "Create list")
      )
    )
  }
  
  
  
  # When button to save cells is clicked, open a modal
  observeEvent(input$vio3save_select3, {
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      # Recuperate metadata for cells in the selection
      vis3table <- rval$seurat@meta.data[vis3selection$vis3selected_cells_3,]
      
      # Take only cells in the table after potential filtering
      # vis3table <- vis3table[input$vio3brush3_rows_all,]
      
      vis3ncells <- nrow(vis3table)
      
      # Open cellModal3
      showModal(vis3cellModal3(vis3ncells = vis3ncells, ns = ns))
    }
    
  }) 
  
  
  # When OK button is pressed, attempt to create new cell list. If successful,
  # remove the modal and create it. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$vis3create_list3, {
    
    ns <- session$ns
    
    # Recuperate metadata for cells in the selection
    vis3table <- rval$seurat@meta.data[vis3selection$vis3selected_cells_3,]
    
    # Take only cells in the table after potential filtering
    # vis3table <- vis3table[input$vio3brush3_rows_all,]
    
    vis3ncells <- nrow(vis3table)
    
    # Create temp variable to check if name list already exist
    name_exist = FALSE
    
    # Check if there is cell list created for the current project
    if(rval$seurat_selected %in% names(rval$cell_list)){
      
      # Check if list name already exist for the project
      if(input$vis3list_name3 %in% names(rval$cell_list[[rval$seurat_selected]])){
        
        # If exist then temp variable become TRUE
        name_exist = TRUE
        
      }
      
    }else{
      
      # If no cell list for the current project then create it
      rval$cell_list[[rval$seurat_selected]] <- list()
      
    }
    
    
    
    # Check the name of list
    if(input$vis3list_name3 != '' && !grepl("[^A-Za-z0-9_]", input$vis3list_name3) == TRUE && 
      name_exist == FALSE){
      
      # Close modal
      removeModal()
      
      
      # Create modal progress during list creation
      show_modal_spinner(text = paste0('Creation of list ', input$vis3list_name3,'... Please wait...'), spin = 'circle')
      
      # Add cell name to the list
      rval$cell_list[[rval$seurat_selected]][[input$vis3list_name3]] <- rownames(vis3table)

      
      rval$parameters[[rval$seurat_selected]][["steps"]] = c(rval$parameters[[rval$seurat_selected]][["steps"]], "gating")
      
      params_gate = list()
      if(input$vio3radio_type_plot_3 == 1){
        params_gate[["type"]] = "Violin"
      }else if(input$vio3radio_type_plot_3 == 2){
        params_gate[["type"]] = "Histogram"
      }
      params_gate[["for"]] = input$vio3value_3
      params_gate[["color"]] = input$vio3color_3

      rval$parameters[[rval$seurat_selected]][[input$vis3list_name3]] = params_gate
      
      
      # Create new metadata with list name
      rval$seurat@meta.data[input$vis3list_name3] <- "Not_Selected"
      
      # Check witch cells are in the selection and update metadat
      rval$seurat@meta.data[rownames(vis3table),input$vis3list_name3] <- "Selected"
      
      # Transform the metadata column of the list in factor
      rval$seurat@meta.data[,input$vis3list_name3] <- as.factor(rval$seurat@meta.data[,
        input$vis3list_name3])
      
      saveRDS(reactiveValuesToList(rval),paste0(rval$output,"/Project_elements.rds"))
      
      # Close modal progress when list is created
      remove_modal_spinner()
      
      
    }else{
      
      # If name is note validated, call the modal with failure message
      showModal(vis3cellModal3(failed = TRUE, ns = ns, vis3ncells = vis3ncells))
      
    }
    
  })
  
   #### X axis
  
  output$vis3radiobox_chooseDataX <- renderUI({
    ns <- session$ns
    if(!is.null(rval$seurat)){
      
      radioButtons(ns("vis3radio_data1"), label = "Choose Type for X Axis", 
                   choices = list("Embeddings" = 1, "Metadata" = 2, "Feature" = 3),
                   selected = 1, inline = TRUE)
      
      
    } # if
    else{NULL}
  })
  
  
  
  output$vis3radiobox_chooseFeatureX <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$vis3radio_data1)){
      
      if(input$vis3radio_data1 == 3){
        
        radioButtons(ns("vis3radio_featureX"), label = "",
                     choices = list("Enter a Gene Name" = 1, "Select Gene from HVG List" = 2, "Select Gene from Project Genes List" = 3),
                     selected = 1, inline = T)
      }
      
    }else{
      NULL
    }
  })
  
  # definding the radio button
  
  # fetch reductions from seurat object when reduction is slected in the radio button
  ####create an input using the ductions
  output$vis3select_all_rd_x <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$vis3radio_data1)){
      if(input$vis3radio_data1 == 1){
        vis3reduc <- Reductions(rval$seurat)
        
        if(length(vis3reduc) > 0){
          selectInput(ns("vis3all_rd"), label = "Select Embeddings", 
                      choices = vis3reduc, selected = tail(vis3reduc,1))
        }else{NULL}
        
      }else{NULL}
    }else{NULL}
  })
  
  ####Create select input to choose the axis depending on the result of radio button 1
  
  output$vis3select_X <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$vis3radio_data1)){
      if(input$vis3radio_data1 == 1){
        vis3choicesPCA <- vis3nbPCACol()
        
        selectInput(ns("vis3axisPCA"), "Select X Axis", 
                    choices = vis3choicesPCA, selected = vis3choicesPCA[1])
      }
      else if(input$vis3radio_data1 == 2){
        vis3choicesNumeric <- numericCol()
        selectInput(ns("vis3axisPCA"), "Select X Axis",
                    choices = vis3choicesNumeric, selected = vis3choicesNumeric[1])
        
      }
      else if(input$vis3radio_data1 == 3){
        
        if(!is.null(input$vis3radio_featureX)){
          
          if(input$vis3radio_featureX == 1){
            
            textInput(ns("vis3axisPCA"), "Enter a Gene Name")
            
          }else if(input$vis3radio_featureX == 2){
            
            choicesfeature <- feature_hvg()
            
            # Create select input with the features
            
            selectInput(ns("vis3axisPCA"), "Choose Feature",
                        choicesfeature, selected = choicesfeature[1])
            
            
          }else if(input$vis3radio_featureX == 3){
            
            # Recuperate features
            
            choicesfeature <- feature()
            
            # Create select input with the features
            
            selectInput(ns("vis3axisPCA"), "Choose Feature",
                        choicesfeature, selected = choicesfeature[1])
            
          }
          
        }
        
        
      }else{NULL}
      
    }
    
    
  })
  
  
  #  starting with Y axis.......
  
  output$vis3radiobox_chooseDataY <- renderUI({
    ns <- session$ns
    
    if (!is.null(rval$seurat)){
      radioButtons(ns("vis3radio_data2"), label = "Choose Type for Y Axis",
                   choices = list("Embeddings" = 1, "Metadata" = 2, "Feature" = 3),selected = 1, inline = T) #
    }else{NULL}
    
  })
  
  
  
  output$vis3radiobox_chooseFeatureY <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$vis3radio_data2)){
      
      if(input$vis3radio_data2 == 3){
        
        radioButtons(ns("vis3radio_featureY"), label = "",
                     choices = list("Enter a Gene Name" = 1, "Select Gene from HVG List" = 2, "Select Gene from Project Genes List" = 3),
                     selected = 1, inline = T)
      }
      
    }else{
      NULL
    }
  })
  
  
  
  output$vis3select_all_rd_y <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$vis3radio_data2)){
      if(input$vis3radio_data2 == 1){
        vis3reduc <- Reductions(rval$seurat)
        if(length(vis3reduc) > 0){
          
          selectInput(ns("vis3all_rd_y"), label = "Select Embeddings", 
                      choices = vis3reduc, selected = tail(vis3reduc,1))
        }else{NULL}
      }else{NULL}
    }else{NULL}
  })
  
  ##### selcting Y
  
  output$vis3select_Y <- renderUI({
    ns <- session$ns
    if(!is.null(input$vis3radio_data2)){
      if(input$vis3radio_data2 == 1){
        vis3choicesPCA <- vis3nbPCACol_y()
        selectInput(ns("vis3axisPCA_y"), label = "Select Y Axis",
                    choices = vis3choicesPCA, selected = vis3choicesPCA[2])
      }else if(input$vis3radio_data2 == 2){
        vis3choicesNumeric <- numericCol()
        selectInput(ns("vis3axisPCA_y"), "Select Y Axis",
                    choices = vis3choicesNumeric, selected = vis3choicesNumeric[2])
      }else if(input$vis3radio_data2 == 3){
        
        if(!is.null(input$vis3radio_featureY)){
          
          if(input$vis3radio_featureY == 1){
            
            textInput(ns("vis3axisPCA_y"), "Enter a Gene Name")
            
          }else if(input$vis3radio_featureY == 2){
            
            choicesfeature <- feature_hvg()
            
            # Create select input with the features
            
            selectInput(ns("vis3axisPCA_y"), "Choose Feature",
                        choicesfeature, selected = choicesfeature[1])
            
            
          }else if(input$vis3radio_featureY == 3){
            
            # Recuperate features
            
            choicesfeature <- feature()
            
            # Create select input with the features
            
            selectInput(ns("vis3axisPCA_y"), "Choose Feature",
                        choicesfeature, selected = choicesfeature[1])
            
          }
          
        }

      }else{NULL}
    }
  })
  
  #data points
  output$vis3dataPoint_size <- renderUI({
    
    ns <- session$ns
    numericInput(ns("vis3in_dataPoint_size"), "Set the Size of Data Points in Plot:", value=0.15, min = 0.1, max = 10,  width = "50%")
  })
  
  ####### visual parameters
  
  
  output$vis3checkbox_color <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      checkboxInput(ns("vis3checkBox_col_1"), label = "Color", value = TRUE)
    } else{NULL}
  })
  
  # If color if True, create radio buton to choose type of data to color
  
  output$vis3radio_color1 <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$vis3checkBox_col_1)){
      if(input$vis3checkBox_col_1 == TRUE){
        radioButtons(ns("vis3radio_color1"), label = "By", 
                     choices = list("Metadata" = 1, "Feature" = 2), selected = 1, inline = T)
      }else{NULL}
    }
    
    
  })
  
  
  
  
  output$vis3radiobox_chooseColor <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$vis3radio_color1)){
      
      if(input$vis3radio_color1 == 2){
        
        radioButtons(ns("vis3radio_featureColor"), label = "",
                     choices = list("Enter a Gene Name" = 1, "Select Gene from HVG List" = 2, "Select Gene from Project Genes List" = 3),
                     selected = 1, inline = T)
      }
      
    }else{
      NULL
    }
  })
  
  
  
  output$vis3select_color <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$vis3checkBox_col_1)){
      if(input$vis3checkBox_col_1 == TRUE){
        if(!is.null(input$vis3radio_color1)){
          if(input$vis3radio_color1 == 1){
            
            vis3choices <- c(factorCol(), numericCol())
            
            selectInput(ns("vis3color"), label = "Choose color", 
                        choices = vis3choices, selected = vis3choices[1], width = "50%")
            
          }else if(input$vis3radio_color1 == 2){
            if(!is.null(input$vis3radio_featureColor)){
              if(input$vis3radio_featureColor == 1){
                
                textInput(ns("vis3color"), "Enter a Gene Name", width = "50%")
                
              }else if(input$vis3radio_featureColor == 2){
                
                choicesfeature <- feature_hvg()
                
                # Create select input with the features
                
                selectInput(ns("vis3color"), "Choose Feature",
                            choicesfeature, selected = choicesfeature[1], width = "50%")
                
                
              }else if(input$vis3radio_featureColor == 3){
                
                # Recuperate features
                
                choicesfeature <- feature()
                
                # Create select input with the features
                
                selectInput(ns("vis3color"), "Choose Feature",
                            choicesfeature, selected = choicesfeature[1], width = "50%")
                
              }
              
            }
            
          }else {NULL}
        }else{NULL}
      }else{NULL}
    }else{NULL}
    
    
  })
  
  # Radio button to select theme of plot
  
  output$vis3radio_theme1 <- renderUI({
    ns <- session$ns
    radioButtons(ns("vis3theme1"), label = "Theme", choices = list("Classic" = "classic",
                                                               "Gray" = "gray", "Void" = "void", "Minimal" = "minimal", "Dark" = "dark"),
                 selected = "classic", inline = T)
  })
  
  
  # Checkbox to facet by row
  
  output$vis3checkbox_facet_row <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      # Checkbox false by default
      
      checkboxInput(ns("vis3checkBox_frow_1"), label = "Facet 1", value = FALSE)
    }else{
      NULL
    }
    
  })
  
  
  
  # Select input if checkbox facet by row is true
  
  output$vis3select_frow <- renderUI({
    
    ns <- session$ns
    
    
    if(!is.null(input$vis3checkBox_frow_1)){
      
      if(input$vis3checkBox_frow_1 == TRUE){ # Check if checkbox is true
        
        # Recuperate factor column
        
        vis3choices <- factorCol()
        
        # Create select input
        
        selectInput(ns("vis3facet_row"), "Choose facet",
                    vis3choices, selected = vis3choices[1], width = "50%")
        
      }else{
        NULL
      }
      
    }
    
    
    
  })
  
  
  # Same for facet by column
  
  output$vis3checkbox_facet_col <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      if(!is.null(input$vis3checkBox_frow_1)){
        
        if(input$vis3checkBox_frow_1 == TRUE){
          
          checkboxInput(ns("vis3checkBox_fcol_1"), label = "Facet 2", value = FALSE)
          
        }
      }
      
    }else{
      NULL
    }
    
  })
  
  
  
  output$vis3select_fcol <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(input$vis3checkBox_fcol_1)){
      
      if(input$vis3checkBox_fcol_1 == TRUE){
        
        vis3choices <- factorCol()
        
        selectInput(ns("vis3facet_col"), "Choose facet",
                    vis3choices, selected = vis3choices[1], width = "50%")
        
      }else{
        NULL
      }
    }
    
  })
  
  ##### Create output ######  
  
  
  # Reactive to create dataframe with selected input
  
  vis3df1 <- reactive({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      if(is.null(input$vis3checkBox_col_1)){
        vis3color <- colnames(rval$seurat@meta.data)[1]
      }else{
        vis3color <-input$vis3color
      }
      
      
      
      vis3params <- c(input$vis3axisPCA, input$vis3axisPCA_y, vis3color, input$vis3facet_row, input$vis3facet_col)

      # Conserve only unique parameters
      
      vis3params <- unique(vis3params)
      
      # Create dataframe with the value of the object
      
      vis3df <- FetchData(rval$seurat, vars = vis3params)
      
      
      if(ncol(vis3df) < length(unique(vis3params))){
        
        showModal(modalDialog(
          title = "Error in parameters",
          paste0(paste(setdiff(vis3params,colnames(vis3df)), collapse = ","), " are not present in the object"),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        
      }else{

        # Add column on the df with names of cells
        
        vis3df$cells <- rownames(vis3df)
        
        
        for(assay in Assays(rval$seurat)){ # For all the assays in the object
          
          # Remove tag of the assay in column if present
          
          vis3assay <- tolower(assay)
          
          vis3name <- paste0(vis3assay,"_(.*)")
          
          colnames(vis3df) <- gsub(vis3name, "\\1", colnames(vis3df))
          
        }
        
        
        # Return dataframe
        
        return(vis3df)
        
      }
      
    }
    
    
  })
  
  # Return plotly
  
  #reactive eveny
  
  vis3ploting <- eventReactive( c(input$vis3rf, input$vis3unselect1), {
    ns <- session$ns
    
    if(!is.null(input$vis3in_dataPoint_size) && input$vis3in_dataPoint_size >= 0.01 && input$vis3in_dataPoint_size <= 5){
      output$vis3Plot_error <- renderText({NULL})
      
    
    # ######################
    if(!is.null(rval$seurat) & !is.null(vis3df1()) ){ # Check if object and dataframe not null
      
      if(!is.null(input$vis3axisPCA) & !is.null(input$vis3axisPCA_y)){ # Check if x and y axis input selectd
        
        if(input$vis3axisPCA != "" & input$vis3axisPCA_y != ""){
          
          vis3plot <-ggplot(vis3df1(), aes_string(x = paste0("`", input$vis3axisPCA, "`"), 
                                          y = paste0("`", input$vis3axisPCA_y, "`"), key = "cells"))
          
          vis3plot <- vis3plot + guides(alpha = FALSE)
          
          
          
          
          if(!is.null(input$vis3checkBox_col_1)){
            
            if(input$vis3checkBox_col_1 == TRUE){ # Check if need color 
              
              # Add colored point with input color
              
              
              if(!is.null(input$vis3color)){
                
                vis3plot <- vis3plot + geom_point(aes_string(color = paste0("`", input$vis3color, "`")), size = input$vis3in_dataPoint_size)
                
                if(is.numeric(FetchData(rval$seurat,input$vis3color)[1,])){
                  
                  vis3plot <- vis3plot + scale_color_gradient(low = "grey", high = "red")
                  
                }
                
              }else{
                
                vis3plot <- vis3plot + geom_point(size = input$vis3in_dataPoint_size)
                
              }
              
            }else{
              
              # Add point without color 
              
              vis3plot <- vis3plot + geom_point(size = input$vis3in_dataPoint_size)
              
            }
            
          }else{
            
            vis3plot <- vis3plot + geom_point(aes_string(color = colnames(rval$seurat@meta.data)[1]), size = input$vis3in_dataPoint_size)
            
          }
          
          
          
          if(!is.null(input$vis3checkBox_frow_1) & !is.null(input$vis3checkBox_fcol_1)){
            
            if(input$vis3checkBox_frow_1 == TRUE & input$vis3checkBox_fcol_1 == TRUE){ #Check if both facet by row and col
              
              # Add facet by row and col input
              
              vis3plot <- vis3plot + facet_grid(rows = vars(get(input$vis3facet_row)), cols = vars(get(input$vis3facet_col)))
              
            }else if(input$vis3checkBox_frow_1 == TRUE){ # Check if only facet by col
              
              # Add facet wrap
              
              vis3plot <- vis3plot + facet_wrap(~get(input$vis3facet_row))
              
            }
            
            
          }
          
          
          if(!is.null(input$vis3theme1)){
            
            vis3theme <- paste0("theme_",input$vis3theme1)
            
          }else{
            
            vis3theme <- "theme_classic"
            
          }
          
          vis3selection$vis3selected_cells_1 <- NULL
          
          vis3plot <- vis3plot + get(vis3theme)()
          
          
          # transform ggplot to plotly
          
          ggplotly(vis3plot, source = "plot_vis_3")
          
        }
        
      }
      
    }
    
    }# data point limit
    else{shinyalert("Warning!", "The Selected Value for the Data Points Size is Out of Range. Please Select a Value in a Range of 0.01 to 5", type = "warning")
      output$vis3Plot_error <- renderText({"Replot the plot with corrected parameters"})
      NULL
      
      
    }
    
    ##############################
  }) 
  
  
  output$vis3Plot <- renderPlotly({
    vis3ploting()
  } )


  observeEvent(input$vis3rf, {
    ns <- session$ns
    output$vis3exportButton <- renderUI({
      
        downloadButton(ns("vis3down1"), "Download",class = "butt1")
      
      })
    output$vis3exportFormat <- renderUI({

       
      # choices for file download format
      radioButtons(ns("vis3fileFormat1"), "Choose file format to download:", 
        choices = c("jpeg", "png", "pdf"), 
        selected = "jpeg", inline = TRUE )


      })
  })

  # #save file in the shiny file conating directory
 # observeEvent(input$down1, {
observeEvent(input$vis3fileFormat1,{


    output$vis3down1 <- downloadHandler(
      #ns <- session$ns
    # file name

    vis3filename <- paste("plot", input$vis3fileFormat1, sep="."),
    # content
    content = function(file){
      shiny::withProgress(
        message = paste0("Downloading ", vis3filename, " .... "),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(6/10)

          #tempFile <- paste("temp", filename, sep="_")
          # create plot
          export(p = vis3ploting(), file = paste("temp", vis3filename, sep="_"))
            
          # hand over the file
          file.copy(paste("temp", vis3filename, sep="_"),file)
        }
      )
    }
    )
})


#First Violon 1D visualization


  #datapoint size
  output$dataPoint_size3 <- renderUI({
    
    ns <- session$ns
    numericInput(ns("in_dataPoint_size3"), "Set the Size of Data Points in Plot:", value=0.15, min = 0.1, max = 10,  width = "50%")
  })


  output$radiobox_chooseType3 <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      radioButtons(ns("radio_type_plot_3"), label = "Choose Type of Plot",
                   choices = list("Violin" = 1, "Histogram" = 2, "Density" = 3),
                   selected = 1, inline = T)
      
    }else{
      NULL
    }
    
  })
  

  output$radiobox_chooseData3 <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$radio_type_plot_3)){
      
      if(input$radio_type_plot_3 == 1 || input$radio_type_plot_3 == 3){
        
        radioButtons(ns("radio_type3"), label = "For",
                     choices = list("Metadata" = 1, "Feature" = 2),
                     selected = 1, inline = T)
        
      }
      
      
    }else{
      NULL
    }
    
  })
  
  
  
  output$radiobox_chooseFeature3 <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$radio_type3)){
      
      if(input$radio_type3 == 2 && input$radio_type_plot_3 != 2){
        
        radioButtons(ns("radio_feature3"), label = "For",
                     choices = list("Enter a Gene Name" = 1, "Select Gene from HVG List" = 2, "Select Gene from Project Genes List" = 3),
                     selected = 1, inline = T)
        
      }
      
      
    }else{
      NULL
    }
    
  })
  
  
  
  
  output$select_value_3 <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$radio_type_plot_3) && !is.null(input$radio_type3)){
      
      if(input$radio_type_plot_3 == 1 || input$radio_type_plot_3 == 3){
        
        if(input$radio_type3 == 1){ # Check if radio button is metadata
          
          # Recuperate numeric column
          
          choices <- numericCol()
          
          # Create input with the numeric column
          
          selectInput(ns("value_3"), "Select Metadata",
                      choices, selected = choices[1], width = "50%")
          
          
        }else if(input$radio_type3 == 2){ # Check if radio button is feature
          
          if(!is.null(input$radio_feature3)){
            
            if(input$radio_feature3 == 1){
              
              textInput(ns("value_3"), "Enter a Gene Name", width = '25%')
              
            }else if(input$radio_feature3 == 2){
              
              choicesfeature <- feature_hvg()

              # Create select input with the features
              
              selectInput(ns("value_3"), "Choose Feature",
                          choicesfeature, selected = choicesfeature[1], width = "50%")
              
              
            }else if(input$radio_feature3 == 3){
              
              # Recuperate features
              
              choicesfeature <- feature()
              
              # Create select input with the features
              
              selectInput(ns("value_3"), "Choose Feature",
                          choicesfeature, selected = choicesfeature[1], width = "50%")
              
            }
            
          }
          
         
          
        }else{
          NULL
        }
        
      }else{
        
        choices <- factorCol()
        
        # Create input with the numeric column
        
        selectInput(ns("value_3"), "Select Metadata",
                    choices, selected = choices[1], width = "50%")
        
      }
      
      
    }
    
  })
  
  
  
  output$color_3 <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      # Recuperate factor column
      
      choices <- factorCol()
      
      # Create input with the factor column
      
      selectInput(ns("color_3"), "Color by",
                  choices, selected = choices[1], width = "50%")
      
    }else{
      NULL
    }
    
  })
  
  
  
  
  # Radio button to select theme of plot
  
  output$radio_theme3 <- renderUI({
    
    ns <- session$ns
    
    radioButtons(ns("theme3"), label = "Theme",
                 choices = list("Classic" = "classic", "Gray" = "gray", "Void" = "void",
                                "Minimal" = "minimal", "Dark" = "dark"),
                 selected = "classic", inline = TRUE)
    
    
  })
  
  
  
  
  ##### Create output ######  
  
  
  # Reactive to create dataframe with selected input
  
  df3 <- reactive({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      
      if(is.null(input$color_3)){
        color <- colnames(rval$seurat@meta.data)[1]
      }else{
        color <-input$color_3
      }
      
      # Recuperate all the input
      
      params <- c(input$value_3, color)
      
      # Conserve only unique parameters
      
      params <- unique(params)
      
      # Create dataframe with the value of the object
      
      # df <- FetchData(rval$seurat, vars = params, cells = c(selection$selected_cells_1,selection$selected_cells_2,selection$selected_cells_4))
      df <- FetchData(rval$seurat, vars = params)
      
      
      if(ncol(df) < length(unique(params))){
        
        showModal(modalDialog(
          title = "Error in parameters",
          paste0(paste(setdiff(params,colnames(df)), collapse = ","), " are not present in the object"),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        
      }else{
        # Add column on the df with names of cells
        
        df$cells <- rownames(df)
        
        for(assay in Assays(rval$seurat)){ # For all the assays in the object
          
          # Remove tag of the assay in column if present
          
          assay <- tolower(assay)
          
          name <- paste0(assay,"_(.*)")
          
          colnames(df) <- gsub(name, "\\1", colnames(df))
          
        }
        
        # Return dataframe
        
        return(df)
      }
      
    }
    
    
  })
  
  
  
  # Return plotly
  
  ploting2 <- eventReactive(input$rf2, {
    ns <- session$ns
    
    ##### Check for data point size
    if(!is.null(input$in_dataPoint_size3) && input$in_dataPoint_size3 >= 0.01 && input$in_dataPoint_size3 <= 5){
      output$Plot2_error <- renderText({NULL})
    
          
    
    ##########
    
    if(!is.null(rval$seurat) & !is.null(df3()) ){ # Check if object and dataframe not null
      
      if(!is.null(input$radio_type_plot_3) & !is.null(input$value_3)){
        
        if(input$radio_type_plot_3 == 1){
          
          if(!is.null(input$color_3)){
            
            plot <- ggplot(data = df3(), aes_string(x = paste0("`", input$color_3, "`"), y = paste0("`", input$value_3, "`"))) + 
              geom_violin(aes_string(fill = paste0("`", input$color_3, "`"))) +
              geom_point(position = "jitter", aes(key = cells), size = as.numeric (input$in_dataPoint_size3))
            
          }else{
            
            plot <- ggplot(data = df3(), aes_string(x = colnames(rval$seurat@meta.data)[1], y = paste0("`", input$value_3, "`"))) + 
              geom_violin(aes_string(fill = colnames(rval$seurat@meta.data)[1])) +
              geom_point(position = "jitter", aes(key = cells), size =  as.numeric(input$in_dataPoint_size3))
            
          }
          
        }
        
        
        if(input$radio_type_plot_3 == 2){
          
          if(!is.null(input$color_3)){
            
            plot <- ggplot(data = df3(), aes_string(x=paste0("`", input$value_3, "`"), fill = paste0("`", input$color_3, "`"))) + 
              geom_histogram(stat = "count")
            
          }else{
            
            
            plot <- ggplot(data = df3(), aes_string(x=paste0("`", input$value_3, "`"), fill =  colnames(rval$seurat@meta.data)[1])) + 
              geom_histogram(stat = "count")
            
          }
          
        }
        
        
        if(input$radio_type_plot_3 == 3){
          
          if(!is.null(input$color_3)){
            
            plot <- ggplot(df3(), aes_string(x =  paste0("`", input$value_3, "`"), color = paste0("`", input$color_3, "`"))) + 
              geom_density()
            
          }else{
            
            plot <- ggplot(df3(), aes_string(x =  paste0("`", input$value_3, "`"), color = colnames(rval$seurat@meta.data)[1])) + 
              geom_density()
            
          }
          
        }
        
        
        if(!is.null(input$theme3)){
          
          theme <- paste0("theme_",input$theme3)
          
        }else{
          
          theme <- "theme_classic"
          
        }
        
        
        selection$selected_cells_3 <- NULL
        
        plot <- plot + get(theme)()
        
        
        # transform ggplot to plotly
        
        ggplotly(plot, source = "plot_vis_4")
        
      }
      
    }
    
    }# data point limit
    else{shinyalert("Warning!", "The Selected Value for the Data Points Size is Out of Range. Please Select a Value in a Range of 0.01 to 5", type = "warning")
      output$Plot2_error <- renderText({"Replot the plot with corrected parameters"})
      NULL
      
      #updateNumericInput(session, "in_dataPoint_size3", value = 0.15)
    }
    
  }) 
  
  
  
  output$Plot2 <- renderPlotly({
    ploting2()
  } )


  ###working
#needs to install RSelenium for svg image
  #adding the export button
  observeEvent(input$rf2, {
    ns <- session$ns
    output$exportButton2 <- renderUI({
      
        downloadButton(ns("down2"), "Download",class = "butt1")
      
      })
    output$exportFormat2 <- renderUI({

       
      # choices for file download format
      radioButtons(ns("fileFormat2"), "Choose file format to download:", 
        choices = c("jpeg", "png", "pdf"), 
        selected = "jpeg", inline = TRUE )


      })
  })

  # #save file in the shiny file conating directory
 # observeEvent(input$down1, {
observeEvent(input$fileFormat2,{


    output$down2 <- downloadHandler(
      #ns <- session$ns
    # file name

    filename2 <- paste("plot", input$fileFormat2, sep="."),
    # content
    content = function(file){
      shiny::withProgress(
        message = paste0("Downloading ", filename2, " .... "),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(6/10)

          #tempFile <- paste("temp", filename, sep="_")
          # create plot
          export(p = ploting2(), file = paste("temp", filename2, sep="_"))
            
          # hand over the file
          file.copy(paste("temp", filename2, sep="_"),file)
        }
      )
    }
    )
})


#Second Violon 1D visualization
  output$vio2radiobox_chooseType3 <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      radioButtons(ns("vio2radio_type_plot_3"), label = "Choose Type of Plot",
                   choices = list("Violin" = 1, "Histogram" = 2, "Density" = 3),
                   selected = 1, inline = T)
      
    }else{
      NULL
    }
    
  })
  
  
  
  
  output$vio2radiobox_chooseData3 <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$vio2radio_type_plot_3)){
      
      if(input$vio2radio_type_plot_3 == 1 || input$vio2radio_type_plot_3 == 3){
        
        radioButtons(ns("vio2radio_type3"), label = "For",
                     choices = list("Metadata" = 1, "Feature" = 2),
                     selected = 1, inline = T)
        
      }
      
      
    }else{
      NULL
    }
    
  })
  
  
  
  
  output$vio2radiobox_chooseFeature3 <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$vio2radio_type3)){
      
      if(input$vio2radio_type3 == 2 && input$vio2radio_type_plot_3 != 2){
        
        radioButtons(ns("vio2radio_feature3"), label = "For",
                     choices = list("Enter a Gene Name" = 1, "Select Gene from HVG List" = 2, "Select Gene from Project Genes List" = 3),
                     selected = 1, inline = T)
        
      }
      
      
    }else{
      NULL
    }
    
  })
  
  
  
  
  
  
  output$vio2select_value_3 <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$vio2radio_type_plot_3) && 
      !is.null(input$vio2radio_type3)){
      
      if(input$vio2radio_type_plot_3 == 1 || input$vio2radio_type_plot_3 == 3){
        
        if(input$vio2radio_type3 == 1){ # Check if radio button is metadata
          
          # Recuperate numeric column
          
          vio2choices <- numericCol()
          
          # Create input with the numeric column
          
          selectInput(ns("vio2value_3"), "Select Metadata",
                      vio2choices, selected = vio2choices[1], width = "50%")
          
          
        }else if(input$vio2radio_type3 == 2){ # Check if radio button is feature
          

          if(!is.null(input$vio2radio_feature3)){
            
            if(input$vio2radio_feature3 == 1){
              
              textInput(ns("vio2value_3"), "Enter a Gene Name", width = '25%')
              
            }else if(input$vio2radio_feature3 == 2){
              
              choicesfeature <- feature_hvg()
              
              # Create select input with the features
              
              selectInput(ns("vio2value_3"), "Choose Feature",
                          choicesfeature, selected = choicesfeature[1], width = "50%")
              
              
            }else if(input$vio2radio_feature3 == 3){
              
              # Recuperate features
              
              choicesfeature <- feature()
              
              # Create select input with the features
              
              selectInput(ns("vio2value_3"), "Choose Feature",
                          choicesfeature, selected = choicesfeature[1], width = "50%")
              
            }
            
          }

        }else{
          NULL
        }
        
      }else{
        
        vio2choices <- factorCol()
        
        # Create input with the numeric column
        
        selectInput(ns("vio2value_3"), "Select Metadata",
                    vio2choices, selected = vio2choices[1], width = "50%")
        
      }
      
      
    }
    
  })
  
  ###data points
  output$vio2dataPoint_size <- renderUI({
    
    ns <- session$ns
    numericInput(ns("vio2in_dataPoint_size"), "Set the Size of Data Points in Plot:", value=0.15, min = 0.1, max = 10,  width = "50%")
  })
  
  output$vio2color_3 <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      # Recuperate factor column
      
      vio2choices <- factorCol()
      
      # Create input with the factor column
      
      selectInput(ns("vio2color_3"), "Color by",
                  vio2choices, selected = vio2choices[1], width = "50%")
      
    }else{
      NULL
    }
    
  })
  
  
  
  
  # Radio button to select theme of plot
  
  output$vio2radio_theme3 <- renderUI({
    
    ns <- session$ns
    
    radioButtons(ns("vio2theme3"), label = "Theme",
                 choices = list("Classic" = "classic", "Gray" = "gray", "Void" = "void",
                                "Minimal" = "minimal", "Dark" = "dark"),
                 selected = "classic", inline = TRUE)
    
    
  })
  
  
  
  
  ##### Create output ######  
  
  
  # Reactive to create dataframe with selected input
  
  vio2df3 <- reactive({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      
      if(is.null(input$vio2color_3)){
        vio2color <- colnames(rval$seurat@meta.data)[1]
      }else{
        vio2color <-input$vio2color_3
      }
      
      # Recuperate all the input
      
      vio2params <- c(input$vio2value_3, vio2color)
      
      # Conserve only unique parameters
      
      vio2params <- unique(vio2params)
      
      # Create dataframe with the value of the object
      
      # df <- FetchData(rval$seurat, vars = params, cells = c(selection$selected_cells_1,selection$selected_cells_2,selection$selected_cells_4))
      vio2df <- FetchData(rval$seurat, vars = vio2params)
      
      
      if(ncol(vio2df) < length(unique(vio2params))){
        
        showModal(modalDialog(
          title = "Error in parameters",
          paste0(paste(setdiff(vio2params,colnames(vio2df)), collapse = ","), " are not present in the object"),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        
      }else{
        
        # Add column on the df with names of cells
        
        vio2df$cells <- rownames(vio2df)
        
        for(vio2assay in Assays(rval$seurat)){ # For all the assays in the object
          
          # Remove tag of the assay in column if present
          
          vio2assay <- tolower(vio2assay)
          
          vio2name <- paste0(vio2assay,"_(.*)")
          
          colnames(vio2df) <- gsub(vio2name, "\\1", colnames(vio2df))
          
        }
        
        # Return dataframe
        
        return(vio2df)
      }

    }
    
    
  })
  
  
  
  # Return plotly
  
  

  vio2ploting2 <- eventReactive(input$vio2rf2, {
    ns <- session$ns

    
    
    ##### Check for data point size
    if(!is.null(input$vio2in_dataPoint_size) && input$vio2in_dataPoint_size >= 0.01 && input$vio2in_dataPoint_size <= 5){
      output$vio2Plot_error <- renderText({NULL})
      
      
    
    
    if(!is.null(rval$seurat) & !is.null(vio2df3()) ){ # Check if object and dataframe not null
      
      if(!is.null(input$vio2radio_type_plot_3) & !is.null(input$vio2value_3)){
        
        if(input$vio2radio_type_plot_3 == 1){
          
          if(!is.null(input$vio2color_3)){
            
            plot <- ggplot(data = vio2df3(), aes_string(x = paste0("`", input$vio2color_3, "`"), 
              y = paste0("`", input$vio2value_3, "`"))) + 
              geom_violin(aes_string(fill = paste0("`", input$vio2color_3, "`"))) +
              geom_point(position = "jitter", aes(key = cells), size = input$vio2in_dataPoint_size)
            
          }else{
            
            plot <- ggplot(data = vio2df3(), aes_string(x = colnames(rval$seurat@meta.data)[1], 
              y = paste0("`", input$vio2value_3, "`"))) + 
              geom_violin(aes_string(fill = colnames(rval$seurat@meta.data)[1])) +
              geom_point(position = "jitter", aes(key = cells), size = input$vio2in_dataPoint_size)
            
          }
          
        }
        
        
        if(input$vio2radio_type_plot_3 == 2){
          
          if(!is.null(input$vio2color_3)){
            
            plot <- ggplot(data = vio2df3(), aes_string(x=paste0("`", input$vio2value_3, "`"), 
              fill = paste0("`", input$vio2color_3, "`"))) + 
              geom_histogram(stat = "count")
            
          }else{
            
            
            plot <- ggplot(data = vio2df3(), aes_string(x=paste0("`", input$vio2value_3, "`"), 
              fill =  colnames(rval$seurat@meta.data)[1])) + 
              geom_histogram(stat = "count")
            
          }
          
        }
        
        
        if(input$vio2radio_type_plot_3 == 3){
          
          if(!is.null(input$vio2color_3)){
            
            plot <- ggplot(vio2df3(), aes_string(x =  paste0("`", input$vio2value_3, "`"), 
              color = paste0("`", input$vio2color_3, "`"))) + 
              geom_density()
            
          }else{
            
            plot <- ggplot(vio2df3(), aes_string(x =  paste0("`", input$vio2value_3, "`"), 
              color = colnames(rval$seurat@meta.data)[1])) + 
              geom_density()
            
          }
          
        }
        
        
        if(!is.null(input$vio2theme3)){
          
          vio2theme <- paste0("theme_",input$vio2theme3)
          
        }else{
          
          vio2theme <- "theme_classic"
          
        }
        
        
        vis2selection$vis2selected_cells_3 <- NULL
        
        plot <- plot + get(vio2theme)()
        
        
        #transform ggplot to plotly
        
       ggplotly(plot, source = "plot_vis_5")
        
      }
      
      
    }
      
    }# data point limit
    else{shinyalert("Warning!", "The Selected Value for the Data Points Size is Out of Range. Please Select a Value in a Range of 0.01 to 5", type = "warning")
      output$vio2Plot_error <- renderText({"Replot the plot with corrected parameters"})
      NULL
      
     
    }
    
  }) 
  
  
  
  output$vio2Plot2 <- renderPlotly({
    vio2ploting2()
  } )

  #adding the export button
  observeEvent(input$vio2rf2, {
    ns <- session$ns
    output$vio2exportButton <- renderUI({
      
        downloadButton(ns("vio2down1"), "Download",class = "butt1")
      
      })
    output$vio2exportFormat <- renderUI({

       
      # choices for file download format
      radioButtons(ns("vio2fileFormat1"), "Choose file format to download:", 
        choices = c("jpeg", "png", "pdf"), 
        selected = "jpeg", inline = TRUE )


      })
  })

observeEvent(input$vio2fileFormat1,{


    output$vio2down1 <- downloadHandler(
      #ns <- session$ns
    # file name

    vio2filename <- paste("plot", input$vio2fileFormat1, sep="."),
    # content
    content = function(file){
      shiny::withProgress(
        message = paste0("Downloading ", vio2filename, " .... "),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(.5)
          shiny::incProgress(6/10)

          #tempFile <- paste("temp", filename, sep="_")
          # create plot
          export(p = vio2ploting2(), file = paste("temp", vio2filename, sep="_"))
            
          # hand over the file
          file.copy(paste("temp", vio2filename, sep="_"),file)
        }
      )
    }
    )
})


#Third Violon 1D visualization

  output$vio3radiobox_chooseType3 <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      radioButtons(ns("vio3radio_type_plot_3"), label = "Choose Type of Plot",
                   choices = list("Violin" = 1, "Histogram" = 2, "Density" = 3),
                   selected = 1, inline = T)
      
    }else{
      NULL
    }
    
  })
  
  
  
  
  output$vio3radiobox_chooseData3 <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$vio3radio_type_plot_3)){
      
      if(input$vio3radio_type_plot_3 == 1 || input$vio3radio_type_plot_3 == 3){
        
        radioButtons(ns("vio3radio_type3"), label = "For",
                     choices = list("Metadata" = 1, "Feature" = 2),
                     selected = 1, inline = T)
        
      }
      
      
    }else{
      NULL
    }
    
  })
  
  
  
  output$vio3radiobox_chooseFeature3 <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$vio3radio_type3)){
      
      if(input$vio3radio_type3 == 2 && input$vio3radio_type_plot_3 != 2){
        
        radioButtons(ns("vio3radio_feature3"), label = "For",
                     choices = list("Enter a Gene Name" = 1, "Select Gene from HVG List" = 2, "Select Gene from Project Genes List" = 3),
                     selected = 1, inline = T)
        
      }
      
      
    }else{
      NULL
    }
    
  })
  
  
  
  
  output$vio3select_value_3 <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$vio3radio_type_plot_3) && !is.null(input$vio3radio_type3)){
      
      if(input$vio3radio_type_plot_3 == 1 || input$vio3radio_type_plot_3 == 3){
        
        if(input$vio3radio_type3 == 1){ # Check if radio button is metadata
          
          # Recuperate numeric column
          
          vio3choices <- numericCol()
          
          # Create input with the numeric column
          
          selectInput(ns("vio3value_3"), "Select Metadata",
                      vio3choices, selected = vio3choices[1], width = "50%")
          
          
        }else if(input$vio3radio_type3 == 2){ # Check if radio button is feature
          
          
          
          if(!is.null(input$vio3radio_feature3)){
            
            if(input$vio3radio_feature3 == 1){
              
              textInput(ns("vio3value_3"), "Enter a Gene Name", width = '25%')
              
            }else if(input$vio3radio_feature3 == 2){
              
              choicesfeature <- feature_hvg()
              
              # Create select input with the features
              
              selectInput(ns("vio3value_3"), "Choose Feature",
                          choicesfeature, selected = choicesfeature[1], width = "50%")
              
              
            }else if(input$vio3radio_feature3 == 3){
              
              # Recuperate features
              
              choicesfeature <- feature()
              
              # Create select input with the features
              
              selectInput(ns("vio3value_3"), "Choose Feature",
                          choicesfeature, selected = choicesfeature[1], width = "50%")
              
            }
            
          }

          
        }else{
          NULL
        }
        
      }else{
        
        vio3choices <- factorCol()
        
        # Create input with the numeric column
        
        selectInput(ns("vio3value_3"), "Select Metadata",
                    vio3choices, selected = vio3choices[1], width = "50%")
        
      }
      
      
    }
    
  })
  
  
  output$vio3dataPoint_size <- renderUI({
    
    ns <- session$ns
    numericInput(ns("vi03in_dataPoint_size"), "Set the Size of Data Points in Plot:", value=0.15, min = 0.1, max = 10,  width = "50%")
  })
  
  output$vio3color_3 <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      # Recuperate factor column
      
      vio3choices <- factorCol()
      
      # Create input with the factor column
      
      selectInput(ns("vio3color_3"), "Color by",
                  vio3choices, selected = vio3choices[1], width = "50%")
      
    }else{
      NULL
    }
    
  })
  
  
  
  
  # Radio button to select theme of plot
  
  output$vio3radio_theme3 <- renderUI({
    
    ns <- session$ns
    
    radioButtons(ns("vio3theme3"), label = "Theme",
                 choices = list("Classic" = "classic", "Gray" = "gray", "Void" = "void",
                                "Minimal" = "minimal", "Dark" = "dark"),
                 selected = "classic", inline = TRUE)
    
    
  })
  
  
  
  
  ##### Create output ######  
  
  
  # Reactive to create dataframe with selected input
  
  vio3df3 <- reactive({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      
      
      if(is.null(input$vio3color_3)){
        vio3color <- colnames(rval$seurat@meta.data)[1]
      }else{
        vio3color <-input$vio3color_3
      }
      
      # Recuperate all the input
      
      vio3params <- c(input$vio3value_3, vio3color)
      
      # Conserve only unique parameters
      
      vio3params <- unique(vio3params)
      
      # Create dataframe with the value of the object
      
      # df <- FetchData(rval$seurat, vars = params, cells = c(selection$selected_cells_1,selection$selected_cells_2,selection$selected_cells_4))
      vio3df <- FetchData(rval$seurat, vars = vio3params)
      
      
      if(ncol(vio3df) < length(unique(vio3params))){
        
        showModal(modalDialog(
          title = "Error in parameters",
          paste0(paste(setdiff(vio3params,colnames(vio3df)), collapse = ","), " are not present in the object"),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        
      }else{
        # Add column on the df with names of cells
        
        vio3df$cells <- rownames(vio3df)
        
        for(assay in Assays(rval$seurat)){ # For all the assays in the object
          
          # Remove tag of the assay in column if present
          
          vio3assay <- tolower(assay)
          
          vio3name <- paste0(vio3assay,"_(.*)")
          
          colnames(vio3df) <- gsub(vio3name, "\\1", colnames(vio3df))
          
        }
        
        # Return dataframe
        
        return(vio3df)
      }

    }
    
    
  })
  
  
  
  # Return plotly
  
  vio3ploting2 <- eventReactive(input$vio3rf2, {
    ns <- session$ns
    
    if(!is.null(input$vi03in_dataPoint_size) && input$vi03in_dataPoint_size >= 0.01 && input$vi03in_dataPoint_size <= 5){
      output$vio3Plot_error <- renderText({NULL})
      
    
    if(!is.null(rval$seurat) & !is.null(vio3df3()) ){ # Check if object and dataframe not null
      
      if(!is.null(input$vio3radio_type_plot_3) & !is.null(input$vio3value_3)){
        
        if(input$vio3radio_type_plot_3 == 1){
          
          if(!is.null(input$vio3color_3)){
            
            vio3plot <- ggplot(data = vio3df3(), aes_string(x = paste0("`", input$vio3color_3, "`"), y = paste0("`", input$vio3value_3, "`"))) + 
              geom_violin(aes_string(fill = paste0("`", input$vio3color_3, "`"))) +
              geom_point(position = "jitter", aes(key = cells), size = input$vi03in_dataPoint_size)
            
          }else{
            
            vio3plot <- ggplot(data = vio3df3(), aes_string(x = colnames(rval$seurat@meta.data)[1], y = paste0("`", input$vio3value_3, "`"))) + 
              geom_violin(aes_string(fill = colnames(rval$seurat@meta.data)[1])) +
              geom_point(position = "jitter", aes(key = cells), size = input$vi03in_dataPoint_size)
            
          }
          
        }
        
        
        if(input$vio3radio_type_plot_3 == 2){
          
          if(!is.null(input$vio3color_3)){
            
            vio3plot <- ggplot(data = vio3df3(), aes_string(x=paste0("`", input$vio3value_3, "`"), fill = paste0("`", input$vio3color_3, "`"))) + 
              geom_histogram(stat = "count")
            
          }else{
            
            
            vio3plot <- ggplot(data = vio3df3(), aes_string(x=paste0("`", input$vio3value_3, "`"), fill =  colnames(rval$seurat@meta.data)[1])) + 
              geom_histogram(stat = "count")
            
          }
          
        }
        
        
        if(input$vio3radio_type_plot_3 == 3){
          
          if(!is.null(input$vio3color_3)){
            
            vio3plot <- ggplot(vio3df3(), aes_string(x =  paste0("`", input$vio3value_3, "`"), color = paste0("`", input$vio3color_3, "`"))) + 
              geom_density()
            
          }else{
            
            vio3plot <- ggplot(vio3df3(), aes_string(x =  paste0("`", input$vio3value_3, "`"), color = colnames(rval$seurat@meta.data)[1])) + 
              geom_density()
            
          }
          
        }
        
        
        if(!is.null(input$vio3theme3)){
          
          vio3theme <- paste0("theme_",input$vio3theme3)
          
        }else{
          
          vio3theme <- "theme_classic"
          
        }
        
        
        vis3selection$vis3selected_cells_3 <- NULL
        
        vio3plot <- vio3plot + get(vio3theme)()
        
        
        # transform ggplot to plotly
        
        ggplotly(vio3plot, source = "plot_vis_6")
        
      }
      
      
    }
    }
    
  }) 
  
  
  
  output$vio3Plot2 <- renderPlotly({
    vio3ploting2()
  } )


  #adding the export button
observeEvent(input$vio3rf2, {
    ns <- session$ns
    output$vio3exportButton <- renderUI({
      
        downloadButton(ns("vio3down1"), "Download",class = "butt1")
      
      })
    output$vio3exportFormat <- renderUI({

       
      # choices for file download format
      radioButtons(ns("vio3fileFormat1"), "Choose file format to download:", 
        choices = c("jpeg", "png", "pdf"), 
        selected = "jpeg", inline = TRUE )


      })
  })


observeEvent(input$vio3fileFormat1,{


    output$vio3down1 <- downloadHandler(
      #ns <- session$ns
    # file name

    vio3filename <- paste("plot", input$vio3fileFormat1, sep="."),
    # content
    content = function(file){
      shiny::withProgress(
        message = paste0("Downloading ", vio3filename, " .... "),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(6/10)

          #tempFile <- paste("temp", filename, sep="_")
          # create plot
          export(p = vio3ploting2(), file = paste("temp", vio3filename, sep="_"))
            
          # hand over the file
          file.copy(paste("temp", vio3filename, sep="_"),file)
        }
      )
    }
    )
})





#First table visualization

  output$radiobox_chooseTypeTab <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      radioButtons(ns("radio_type_table"), label = "Choose Content of the Table",
                   choices = list("Number of cells" = 1, "Gene expression" = 2),
                   selected = 1, inline = T)
      
    }else{
      NULL
    }
    
  })
  

  output$select_value_Tab <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$radio_type_table)){
      
      if(input$radio_type_table == 1){

          choices <- factorCol()
          
          # Create input with the numeric column
          
          selectInput(ns("value_Tab"), "By",
                      choices, selected = choices[1], width = "50%")

      }else if(input$radio_type_table == 2){

        genes_list <- unique(c(names(rval$genes_list[[rval$seurat_selected]]),names(rval$genes_list[["imported"]])))
        
        selectInput(ns("value_Tab_genes"), "For",
                    genes_list, selected = genes_list[1], width = "50%")
        
      }else{
        NULL
      }
      
    }
    
  })
  

  output$select_meta_Tab_genes <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$radio_type_table)){
      
      if(input$radio_type_table == 2){
        
        choices <- factorCol()
        
        # Create input with the numeric column
        
        selectInput(ns("meta_Tab_genes"), "By",
                    choices, selected = choices[1], width = "50%")
        
      }else{
        NULL
      }
      
    }
    
  })
  

  output$checkbox_Tab <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$radio_type_table)){
      
      if(input$radio_type_table == 1){
      
      # Checkbox false by default
      
      checkboxInput(ns("checkBox_Tab_1"), label = "Split", value = FALSE)
        
      }else if(input$radio_type_table == 2){
        
        NULL
        
      }else{
        NULL
      }
    }
    
  })
  

  output$select_value_Tab_Split <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$radio_type_table) && !is.null(input$checkBox_Tab_1)){
      
      if(input$radio_type_table == 1 && input$checkBox_Tab_1 == T){
        
        choices <- setdiff(factorCol(), input$value_Tab)
        
        # Create input with the numeric column
        
        selectInput(ns("value_Tab_split"), "By",
                    choices, selected = choices[1], width = "50%")
        
      }
      
    }
    
  })
  
  
  
  ##### Create output ######  
  # Reactive to create dataframe with selected input
  
  df_table <- reactive({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){

      df <- rval$seurat@meta.data

      return(df)
      
    }
    
    
  })
  
  
  # Return shiny data table
  
  ploting_DT <- eventReactive(input$rf3, {
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(df_table()) ){ # Check if object and dataframe not null

      if(!is.null(input$radio_type_table) && input$radio_type_table == 1 && !is.null(input$value_Tab)){
        
        if(!is.null(input$checkBox_Tab_1) &&  input$checkBox_Tab_1 == T){
          
          count = df_table() %>% count(!!sym(input$value_Tab_split), !!sym(input$value_Tab))
          
          cast = do.call("cast", args = list(data = count, 
                                             formula = paste(input$value_Tab_split, '~', input$value_Tab), 
                                             fun.aggregate = "mean"))
          
          cast[is.na(cast)] <- 0
          
          cat('<br/>')
          
          rownames(cast) <- cast[,1]
          cast[,1] <- NULL
          
          # Display the number in table
          return(as.data.frame(cast))

        }else{
          return(as.data.frame(t(summary(df_table()[,input$value_Tab]))))


        }

        
      }else if(!is.null(input$radio_type_table) && input$radio_type_table == 2 && !is.null(input$value_Tab_genes)){

        Idents(rval$seurat) <- input$meta_Tab_genes
        
        if(input$value_Tab_genes %in% names(rval$genes_list[[rval$seurat_selected]])){
          
          gene_list <- rval$genes_list[[rval$seurat_selected]][[input$value_Tab_genes]]
          
        }else if(input$value_Tab_genes %in% names(rval$genes_list[["imported"]])){
          
          gene_list <- rval$genes_list[["imported"]][[input$value_Tab_genes]]
          
        }else{
          
          gene_list <- NULL
        }

        
        # We want to compute the log of the mean expression of gene list
        
        # Get normalize data
        fetch <- FetchData(rval$seurat, vars = c(input$meta_Tab_genes,sort(gene_list)), slot = "data")
        
        # Compute exponentiel on gene expression to obtain non log expression
        fetch[,2:ncol(fetch)] <- exp(fetch[,2:ncol(fetch)])
        
        # Calculate mean expression for each gene by metada
        table_expr_genes <- aggregate(fetch[, sort(gene_list)], list(fetch[,1]), mean)
        
        # Transform mean expression in log
        table_expr_genes[,2:ncol(table_expr_genes)] <- log(table_expr_genes[,2:ncol(table_expr_genes)])
        
        # First column to rownames
        rownames(table_expr_genes) <- table_expr_genes[,1]
        
        table_expr_genes[,1] <- NULL
        
        table_expr_genes <- round(table_expr_genes,2)
    
        table_expr_genes <- t(table_expr_genes)
        
        # colnames(table_expr_genes) <- levels(Idents(rval$seurat))

        return(as.data.frame(table_expr_genes))

      }
      
    }
    
  }) 
  
  
  
  output$Plot_DT <- renderDataTable(ploting_DT())
  
  observeEvent(input$rf3, {
    ns <- session$ns

    output$tab1download <- renderUI({
      downloadButton(ns("tab1down1"), 
      "Download CSV",class = "butt1")

    })
  })


  output$tab1down1 <- downloadHandler(
    tab1filename <- "Table.csv",
    # content
    content = function(file){
      shiny::withProgress(
        message = paste0("Downloading ", tab1filename, " .... "),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(6/10)

          write.csv(ploting_DT(), file, row.names = FALSE)
        }
      )
  }
)



# Second table visualization


  
  output$tab2radiobox_chooseTypeTab <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      radioButtons(ns("tab2radio_type_table"), label = "Choose Content of the Table",
                   choices = list("Number of cells" = 1, "Gene expression" = 2),
                   selected = 1, inline = T)
      
    }else{
      NULL
    }
    
  })
  
  
  
  output$tab2select_value_Tab <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$tab2radio_type_table)){
      
      if(input$tab2radio_type_table == 1){

          tab2choices <- factorCol()
          
          # Create input with the numeric column
          
          selectInput(ns("tab2value_Tab"), "By",
                      tab2choices, selected = tab2choices[1], width = "50%")

      }else if(input$tab2radio_type_table == 2){

        tab2genes_list <- unique(c(names(rval$genes_list[[rval$seurat_selected]]),
          names(rval$genes_list[["imported"]])))
        
        selectInput(ns("tab2value_Tab_genes"), "For",
                    tab2genes_list, selected = tab2genes_list[1], width = "50%")
        
      }else{
        NULL
      }
      
    }
    
  })
  

  output$tab2select_meta_Tab_genes <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$tab2radio_type_table)){
      
      if(input$tab2radio_type_table == 2){
        
        tab2choices <- factorCol()
        
        # Create input with the numeric column
        
        selectInput(ns("tab2meta_Tab_genes"), "By",
                    tab2choices, selected = tab2choices[1], width = "50%")
        
      }else{
        NULL
      }
      
    }
    
  })
  
  
  output$tab2checkbox_Tab <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$tab2radio_type_table)){
      
      if(input$tab2radio_type_table == 1){
      
      # Checkbox false by default
      
      checkboxInput(ns("tab2checkBox_Tab_1"), label = "Split", value = FALSE)
        
      }else if(input$tab2radio_type_table == 2){
        
        NULL
        
      }else{
        NULL
      }
    }
    
  })
  
  

  output$tab2select_value_Tab_Split <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$tab2radio_type_table) && !is.null(input$tab2checkBox_Tab_1)){
      
      if(input$tab2radio_type_table == 1 && input$tab2checkBox_Tab_1 == T){
        
        tab2choices <- setdiff(factorCol(), input$tab2value_Tab)
        
        # Create input with the numeric column
        
        selectInput(ns("tab2value_Tab_split"), "By",
                    tab2choices, selected = tab2choices[1], width = "50%")
        
      }
      
    }
    
  })
  
  

  tab2df_table <- reactive({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){

      tab2df <- rval$seurat@meta.data

      return(tab2df)
      
    }
    
    
  })
  
  
 tab2ploting_DT <- eventReactive(input$tab2rf3, {
    
   ns <- session$ns
 
   

    if(!is.null(rval$seurat) && !is.null(tab2df_table()) ){ # Check if object and dataframe not null

      if(!is.null(input$tab2radio_type_table) && input$tab2radio_type_table == 1 && 
        !is.null(input$tab2value_Tab)){
        
        if(!is.null(input$tab2checkBox_Tab_1) &&  input$tab2checkBox_Tab_1 == T){
          
          tab2count = tab2df_table() %>% count(!!sym(input$tab2value_Tab_split), 
             !!sym(input$tab2value_Tab))

          tab2cast = do.call("cast", args = list(data = tab2count, 
                                             formula = paste(input$tab2value_Tab_split, '~', input$tab2value_Tab), 
                                             fun.aggregate = "mean"))
             
           tab2cast[is.na(tab2cast)] <- 0
          
           cat('<br/>')
          
           rownames(tab2cast) <- tab2cast[,1]
           tab2cast[,1] <- NULL
          
           # Display the number in table

          tab2cast_cp <- as.data.frame(tab2cast)           
           
           return(tab2cast_cp)

          
        }else{
          as.data.frame(t(summary(tab2df_table()[,input$tab2value_Tab])))


        }

        
      }else if(!is.null(input$tab2radio_type_table) && input$tab2radio_type_table == 2 && 
        !is.null(input$tab2value_Tab_genes)){

        

        Idents(rval$seurat) <- input$tab2meta_Tab_genes
        
        if(input$tab2value_Tab_genes %in% names(rval$genes_list[[rval$seurat_selected]])){
          
          tab2gene_list <- rval$genes_list[[rval$seurat_selected]][[input$tab2value_Tab_genes]]

         

          #return(tab2gene_list)
          
        }else if(input$tab2value_Tab_genes %in% names(rval$genes_list[["imported"]])){
          
          tab2gene_list <- rval$genes_list[["imported"]][[input$tab2value_Tab_genes]]

          
          
        }else{


          tab2gene_list <- NULL
        }


        # Get normalize data
        tab2fetch <- FetchData(rval$seurat, vars = c(input$tab2meta_Tab_genes,
          sort(tab2gene_list)), slot = "data")
        
        # Compute exponentiel on gene expression to obtain non log expression
        tab2fetch[,2:ncol(tab2fetch)] <- exp(tab2fetch[,2:ncol(tab2fetch)])
        
        # Calculate mean expression for each gene by metada
        tab2table_expr_genes <- aggregate(tab2fetch[, sort(tab2gene_list)], list(tab2fetch[,1]), mean)
        
        # Transform mean expression in log
        tab2table_expr_genes[,2:ncol(tab2table_expr_genes)] <- 
          log(tab2table_expr_genes[,2:ncol(tab2table_expr_genes)])
        
        # First column to rownames
        rownames(tab2table_expr_genes) <- tab2table_expr_genes[,1]
        
        tab2table_expr_genes[,1] <- NULL
        
        tab2table_expr_genes <- round(tab2table_expr_genes,2)
    
        tab2table_expr_genes <- t(tab2table_expr_genes)
        
        # colnames(table_expr_genes) <- levels(Idents(rval$seurat))

        return(as.data.frame(tab2table_expr_genes)) 

      }
      
    }
    
  }) 
  
  
  
  output$tab2Plot_DT <- renderDataTable(
    
    tab2ploting_DT()
    
  )

 observeEvent(input$tab2rf3, {
    ns <- session$ns

    output$tab2download <- renderUI({
      downloadButton(ns("tab2down1"), 
      "Download CSV",class = "butt1")

    })
  })


  output$tab2down1 <- downloadHandler(
    tab2filename <- "Table2.csv",
    # content
    content = function(file){
      shiny::withProgress(
        message = paste0("Downloading ", tab2filename, " .... "),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(6/10)

          write.csv(tab2ploting_DT(), file, row.names = FALSE)
        }
      )
  }
)


#Third table visualization

  output$tab3radiobox_chooseTypeTab <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){
      radioButtons(ns("tab3radio_type_table"), label = "Choose Content of the Table",
                   choices = list("Number of cells" = 1, "Gene expression" = 2),
                   selected = 1, inline = T)
      
    }else{
      NULL
    }
    
  })
  

  output$tab3select_value_Tab <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$tab3radio_type_table)){
      
      if(input$tab3radio_type_table == 1){

          tab3choices <- factorCol()
          
          # Create input with the numeric column
          
          selectInput(ns("tab3value_Tab"), "By",
                      tab3choices, selected = tab3choices[1], width = "50%")

      }else if(input$tab3radio_type_table == 2){

        tab3genes_list <- unique(c(names(rval$genes_list[[rval$seurat_selected]]),
          names(rval$genes_list[["imported"]])))
        
        selectInput(ns("tab3value_Tab_genes"), "For",
                    tab3genes_list, selected = tab3genes_list[1], width = "50%")
        
      }else{
        NULL
      }
      
    }
    
  })
  

  output$tab3select_meta_Tab_genes <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$tab3radio_type_table)){
      
      if(input$tab3radio_type_table == 2){
        
        tab3choices <- factorCol()
        
        # Create input with the numeric column
        
        selectInput(ns("tab3meta_Tab_genes"), "By",
                    tab3choices, selected = tab3choices[1], width = "50%")
        
      }else{
        NULL
      }
      
    }
    
  })
  

  output$tab3checkbox_Tab <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$tab3radio_type_table)){
      
      if(input$tab3radio_type_table == 1){
      
      # Checkbox false by default
      
      checkboxInput(ns("tab3checkBox_Tab_1"), label = "Split", value = FALSE)
        
      }else if(input$tab3radio_type_table == 2){
        
        NULL
        
      }else{
        NULL
      }
    }
    
  })
  

  output$tab3select_value_Tab_Split <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat) && !is.null(input$tab3radio_type_table) && !is.null(input$tab3checkBox_Tab_1)){
      
      if(input$tab3radio_type_table == 1 && input$tab3checkBox_Tab_1 == T){
        
        tab3choices <- setdiff(factorCol(), input$tab3value_Tab)
        
        # Create input with the numeric column
        
        selectInput(ns("tab3value_Tab_split"), "By",
                    tab3choices, selected = tab3choices[1], width = "50%")
        
      }
      
    }
    
  })
  

  # Reactive to create dataframe with selected input
  
  tab3df_table <- reactive({
    
    ns <- session$ns
    
    if(!is.null(rval$seurat)){

      tab3df <- rval$seurat@meta.data

      return(tab3df)
      
    }
    
    
  })
  
  

#  Return shiny data table
  
 tab3ploting_DT <- eventReactive(input$tab3rf3, {
    
   ns <- session$ns
 
   

    if(!is.null(rval$seurat) && !is.null(tab3df_table()) ){ # Check if object and dataframe not null

      if(!is.null(input$tab3radio_type_table) && input$tab3radio_type_table == 1 && 
        !is.null(input$tab3value_Tab)){
        
        if(!is.null(input$tab3checkBox_Tab_1) &&  input$tab3checkBox_Tab_1 == T){
          
          tab3count = tab3df_table() %>% count(!!sym(input$tab3value_Tab_split), 
             !!sym(input$tab3value_Tab))

          tab3cast = do.call("cast", args = list(data = tab3count, 
                                             formula = paste(input$tab3value_Tab_split, '~', input$tab3value_Tab), 
                                             fun.aggregate = "mean"))
             
           tab3cast[is.na(tab3cast)] <- 0
          
           cat('<br/>')
          
           rownames(tab3cast) <- tab3cast[,1]
           tab3cast[,1] <- NULL
          
           # Display the number in table

          tab3cast_cp <- as.data.frame(tab3cast)           
           
           return(tab3cast_cp)

          

        }else{
          as.data.frame(t(summary(tab3df_table()[,input$tab3value_Tab])))


        }

        
      }else if(!is.null(input$tab3radio_type_table) && input$tab3radio_type_table == 2 && 
        !is.null(input$tab3value_Tab_genes)){

        

        Idents(rval$seurat) <- input$tab3meta_Tab_genes
        
        if(input$tab3value_Tab_genes %in% names(rval$genes_list[[rval$seurat_selected]])){
          
          tab3gene_list <- rval$genes_list[[rval$seurat_selected]][[input$tab3value_Tab_genes]]

         

          #return(tab3gene_list)
          
        }else if(input$tab3value_Tab_genes %in% names(rval$genes_list[["imported"]])){
          
          tab3gene_list <- rval$genes_list[["imported"]][[input$tab3value_Tab_genes]]

          
          
        }else{

          
          tab3gene_list <- NULL
        }


        # We want to compute the log of the mean expression of gene list
        
        # Get normalize data
        tab3fetch <- FetchData(rval$seurat, vars = c(input$tab3meta_Tab_genes,
          sort(tab3gene_list)), slot = "data")
        
        # Compute exponentiel on gene expression to obtain non log expression
        tab3fetch[,2:ncol(tab3fetch)] <- exp(tab3fetch[,2:ncol(tab3fetch)])
        
        # Calculate mean expression for each gene by metada
        tab3table_expr_genes <- aggregate(tab3fetch[, sort(tab3gene_list)], list(tab3fetch[,1]), mean)
        
        # Transform mean expression in log
        tab3table_expr_genes[,2:ncol(tab3table_expr_genes)] <- 
          log(tab3table_expr_genes[,2:ncol(tab3table_expr_genes)])
        
        # First column to rownames
        rownames(tab3table_expr_genes) <- tab3table_expr_genes[,1]
        
        tab3table_expr_genes[,1] <- NULL
        
        tab3table_expr_genes <- round(tab3table_expr_genes,2)
    
        tab3table_expr_genes <- t(tab3table_expr_genes)
        
        # colnames(table_expr_genes) <- levels(Idents(rval$seurat))

        return(as.data.frame(tab3table_expr_genes)) 


      }
      
    }
    
  }) 
  
  
  
  output$tab3Plot_DT <- renderDataTable(
    
    tab3ploting_DT()
    
  )

  observeEvent(input$tab3rf3, {
    ns <- session$ns

    output$tab3download <- renderUI({
      downloadButton(ns("tab3down1"), 
      "Download CSV",class = "butt1")

    })
  })


  output$tab3down1 <- downloadHandler(
    tab3filename <- "Table3.csv",
    # content
    content = function(file){
      shiny::withProgress(
        message = paste0("Downloading ", tab3filename, " .... "),
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(6/10)

          write.csv(tab3ploting_DT(), file, row.names = FALSE)
        }
      )
  }
)


  return (rval)

}

