library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyalert)
library(visNetwork)
library(plotly)
library(parcoords)
library(d3heatmap)

shinyUI(
  dashboardPage(
    dashboardHeader(
      # Set height of dashboardHeader
      # tags$li(class = "dropdown",
      #        tags$style(".main-header {max-height: 80px}"),
      #       tags$style(".main-header .logo {height: 80px}")
      #),
      # Use image in title
      title = tags$a(href='http://dbis.rwth-aachen.de/cms',
                     tags$img(src='rwthLogoTest4.png')),
      titleWidth = 350
    ),
    dashboardSidebar(width = 350, 
                     tags$style(".left-side, .main-sidebar {padding-top: 80px}"),
                     sidebarMenu(
                       id = "tabs",
                       
                       menuItem("Home", tabName = "home"), 
                       
                       tags$hr(),
                       checkboxInput("header", "Header", TRUE),
                       
                       textInput("Nrow", "No. of rows", 5),
                       
                       fileInput("file1", "Choose CSV File",
                                 accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                       ),
                       
                       tags$hr(),
                       
                       
                       
                       menuItem("Show Data", tabName = "showData"),
                       
                       menuItem("Show Summary", tabName = "showSummary"), 
                       
                       #menuItem("Correlation Heat Map", tabName = "corrHMViz"),
                       
                       menuItem("Correlation Heat Map", tabName = "corrHMVizProxy",
                                
                                menuSubItem("Categorical Correlation Heat Map", tabName = "catCorrHMViz"),
                                
                                menuSubItem("Numerical Correlation Heat Map", tabName = "numCorrHMViz")
                                
                       ),
                       
                       
                       menuItem("Complete Feature Graph", tabName = "compFeatGraphProxy",
                                
                                menuSubItem("Complete Categorical Feature Graph", tabName = "compCatGraph"),
                                
                                menuSubItem("Complete Numerical Feature Graph", tabName = "compNumGraph")
                              
                       ),
                       
                       
                       menuItem("Cluster Results", tabName = "processData"),
                       
                       
                       
                       menuItem("Visualize Clusters", tabName = "catClustersProxy",
                                
                                menuSubItem("Show Categorical Cluster Graph", tabName = "catClusters"),
                                menuSubItem("Show Individual Categorical Clusters", tabName = "catIndividualCluster"),
                                menuSubItem("Show Numerical Cluster Graph", tabName = "numClusters"),
                                menuSubItem("Show Individual Numerical Clusters", tabName = "numIndividualCluster")
                       ),
                       
                       #menuItem("Show Cat Clusters", tabName = "ClusterDataVis"),
                       
                       menuItem("Selected Features", tabName = "selectedFeat"),
                       
                       
                       menuItem("Representative Feature Graph", tabName = "repFeatGraphProxy",
                                
                                menuSubItem("Representative Categorical Feature Graph", tabName = "repCatGraph"),
                                
                                menuSubItem("Representative Numerical Feature Graph", tabName = "repNumGraph")
                                
                       ),
                       
                       
                       
                       menuItem("Parallel Coordinate Viz", tabName = "parallelViz"),
                       
                       menuItem("Matrix Scatterplot Viz", tabName = "matrixScatterViz"),
                       
                       menuItem("Categorical Bar Plots Viz", tabName = "barPlotViz"),
                       
                       menuItem("Group Plots Viz", tabName = "grpBarPlotViz"),
                       
                       menuItem("Scatter plot Viz", tabName = "scatterViz"),
                       
                       menuItem("3D Scatter plot Viz", tabName = "3DscatterViz"),
                       
                       menuItem("Visualize Cyclic Bar Plots", tabName = "visualizeFeat4")
                       
                       
                       
                     )
                     
    ),
    dashboardBody(
      tags$head(tags$style(HTML(
        '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: black;
        }
        '))),
      tags$script(HTML('
                       $(document).ready(function() {
                       $("header").find("nav").append(\'<span class="myClass"> Feature Clustering and Visualization of High-dimensional data using Clique Cover Theory  </span>\');
                       })
                       ')),
      
      shinyDashboardThemes(
        theme = "blue_gradient"
      ),
      tabItems(
        
        tabItem(tabName = "home",
                
                fluidRow(
                  box(width = 12, uiOutput("myList") ) #textOutput('homeText'))
                )
        ),
        
        
        tabItem(tabName = "showData",
                
                fluidRow(
                  #useShinyalert(),
                  #tableOutput('table')
                 # box(width = 12, tableOutput('table'))
                  column(width = 12,
                         box(
                           title = " ", width = NULL, status = "primary",
                           div(style = 'overflow-x: scroll', tableOutput('table'))
                         )
                  )
                )
        ),
        
        tabItem(tabName = "showSummary",
                
                fluidRow(
                  useShinyalert(),
                  column(12, style = "background-color:#4d3a7d;",verbatimTextOutput("summary1")),
                  
                  tags$head(tags$style("#summary1{background-color: #D3D3D3;
                                 
                                       font-style: italic;
                                       }"
                         )
                  )
                  #verbatimTextOutput("summary")
                  #box(width = 12, textOutput('summary'))
                  #box(width = 8, tableOutput('summary'))
                ),
                fluidRow(
                  useShinyalert(),
                  column(12, style = "background-color:#4d3a7d;",verbatimTextOutput("summary2")),
                  
                  tags$head(tags$style("#summary2{background-color: white;
                                       
                                       font-style: italic;
                                       }"
                         )
                  )
                  
                  ),
                fluidRow(
                  useShinyalert(),
                  column(12, style = "background-color:#4d3a7d;",verbatimTextOutput("summary3")),
                  
                  tags$head(tags$style("#summary3{background-color: #D3D3D3;
                                       
                                       font-style: italic;
                                       }"
                         )
                  )
                  
                  )
        ),
        
        
        tabItem(tabName = "processData",
                fluidRow(
                  column(6, htmlOutput('process'), tags$head(tags$style("#process table {background-color: white; }", media="screen", type="text/css"))),
                  column(6, htmlOutput('process1'), tags$head(tags$style("#process1 table {background-color: white; }", media="screen", type="text/css")))
                )
        ),
        
        
        tabItem(tabName = "catCorrHMViz",
                fluidPage(
                  mainPanel( 
                                 
                    d3heatmapOutput("catHeatmap", width = "100%", height="800px")
                  )
                 
                  
                )    
        ),
        
        tabItem(tabName = "numCorrHMViz",
                fluidPage(
                  mainPanel(
                    d3heatmapOutput("numHeatmap", width = "100%", height="800px")
                  )
                  
                )    
        ),
        
        
        tabItem(tabName = "compCatGraph",
                fluidPage(
                  visNetworkOutput("compCatGraphViz",  width = "100%", height = "1000px")
                )
        ), 
        
        tabItem(tabName = "compNumGraph",
                fluidPage(
                  visNetworkOutput("compNumGraphViz",  width = "100%", height = "1000px")
                )
        ), 
        
        
        
        
        
        tabItem(tabName = "catClusters",
                fluidPage(
                  uiOutput("inputCatCluster"),
                  
                            visNetworkOutput("catClustersViz",  width = "100%", height = "1000px")
                           )
        ),      
        
        tabItem(tabName = "numClusters",
                fluidPage(
                  
                  uiOutput("inputNumCluster"),
                  
                  visNetworkOutput("numClustersViz",  width = "100%", height = "1000px")
                )
        ),      
        
        tabItem(tabName = "catIndividualCluster",
                fluidRow(
                  column(6,
                         visNetworkOutput('catVis1')),
                  
                  column(6,
                         visNetworkOutput('catVis2'))
                ),
                fluidRow(
                  column(6,
                         visNetworkOutput('catVis3')),
                  column(6,
                         visNetworkOutput('catVis4'))
                ),
                fluidRow(
                  column(6,
                         visNetworkOutput('catVis5')),
                  column(6,
                         visNetworkOutput('catVis6'))
                ),
                fluidRow(
                  column(6,
                         visNetworkOutput('catVis7')),
                  column(6,
                         visNetworkOutput('catVis8'))
                ),
                fluidRow(
                  column(6,
                         visNetworkOutput('catVis9')),
                  column(6,
                         visNetworkOutput('catVis10'))
                ),
                fluidRow(
                  column(6,
                         visNetworkOutput('catVis11')),
                  column(6,
                         visNetworkOutput('catVis12'))
                ),
                fluidRow(
                  column(6,
                         visNetworkOutput('catVis13')),
                  column(6,
                         visNetworkOutput('catVis14'))
                ),
                fluidRow(
                  column(6,
                         visNetworkOutput('catVis15')),
                  column(6,
                         visNetworkOutput('catVis16'))
                )
                
        ), 
        
        tabItem(tabName = "numIndividualCluster",
                fluidRow(
                  column(6,
                         visNetworkOutput('numVis1')),
                  
                  column(6,
                         visNetworkOutput('numVis2'))
                ),
                fluidRow(
                  column(6,
                         visNetworkOutput('numVis3')),
                  column(6,
                         visNetworkOutput('numVis4'))
                ),
                fluidRow(
                  column(6,
                         visNetworkOutput('numVis5')),
                  column(6,
                         visNetworkOutput('numVis6'))
                ),
                fluidRow(
                  column(6,
                         visNetworkOutput('numVis7')),
                  column(6,
                         visNetworkOutput('numVis8'))
                ),
                fluidRow(
                  column(6,
                         visNetworkOutput('numVis9')),
                  column(6,
                         visNetworkOutput('numVis10'))
                ),
                fluidRow(
                  column(6,
                         visNetworkOutput('numVis11')),
                  column(6,
                         visNetworkOutput('numVis12'))
                ),
                fluidRow(
                  column(6,
                         visNetworkOutput('numVis13')),
                  column(6,
                         visNetworkOutput('numVis14'))
                ),
                fluidRow(
                  column(6,
                         visNetworkOutput('numVis15')),
                  column(6,
                         visNetworkOutput('numVis16'))
                )
                
        ), 
        
        
        tabItem(tabName = "selectedFeat",
                fluidRow(
                  column(6, htmlOutput('selectedCatFeat'), tags$head(tags$style("#selectedCatFeat table {background-color: white; }", media="screen", type="text/css"))),
                  column(6, htmlOutput('selectedNumFeat'), tags$head(tags$style("#selectedNumFeat table {background-color: white; }", media="screen", type="text/css")))
                )
        ),
        
        
        tabItem(tabName = "repCatGraph",
                fluidPage(
                  visNetworkOutput("repCatGraphViz",  width = "100%", height = "1000px")
                )
        ), 
        
        tabItem(tabName = "repNumGraph",
                fluidPage(
                  visNetworkOutput("repNumGraphViz",  width = "100%", height = "1000px")
                )
        ), 
        
        tabItem(tabName = "parallelViz",
                (fluidRow(
                  column(width=12,  box("Parallel Coordinate Plot", width = "900px", height = "700px",parcoordsOutput( "parcoords",
                                                                                                                       width = "850px", height = "650px"))
                         #,parcoordsOutput( "parcoords", width = "900px", height = "700px" )
                  )
                  #,column(width=6
                  #       ,plotOutput( "iris_pairs", width = "400px" )
                  #)
                ))
        ),
        
        
        tabItem(tabName = "matrixScatterViz",
                (fluidRow(
                  column(width=12, box("Matrix Scatter Plot", width = "900px", height = "700px",uiOutput("pairsplot"))
                  )
                  #,column(width=6
                  #       ,plotOutput( "iris_pairs", width = "400px" )
                  #)
                ))
                
                
                
                
        ),
        
        tabItem(tabName = "barPlotViz",
                
                fluidRow(
                  column( 12, div(style = "height:100px;background-color: white;"), 
                          plotOutput("barplot", width = "900px", height = "1100px") 
                  )
                  
                ) 
                
        ),
        
        tabItem(tabName = "grpBarPlotViz",
                fluidPage(
                  
                  uiOutput("inputBar1"),
                  uiOutput("inputBar2"),
                  uiOutput("inputBar3"),

                  radioButtons("radio", label = h3("Select Type"),
                               choices = list("Group Bar Plot" = "group", "Stacked Bar Plot" = "stack"), 
                               selected = "group"),
                  
                  plotlyOutput("grpBarResult")
                )
                
                
        ),  
        
        
        tabItem(tabName = "scatterViz",
                fluidPage(
                  
                  uiOutput("inputScatter1"),
                  uiOutput("inputScatter2"),
          

                  plotlyOutput("scatterResult")
                )
                
               
        ),      
        
        
        tabItem(tabName = "3DscatterViz",
                fluidPage(
                  uiOutput("input3DScatter1"),
                  uiOutput("input3DScatter2"),
                  uiOutput("input3DScatter3"),
                  uiOutput("input3DScatter4"),
                  
                  plotlyOutput("scatter3DResult")
                )
                
                # fluidPage(
                #   plotlyOutput("plot"),
                #   verbatimTextOutput("event")
                # )
        ),      
        
        
        
        tabItem(tabName = "visualizeFeat4",
                
                fluidRow(
                  column( 12, div(style = "height:300px;background-color: white;"), 
                          plotOutput("cyclicbarplot", width = "600px", height = "600px") 
                  )
                  
                ) 
                
        )
        
        
        
      )
                       )
      )
  
      )