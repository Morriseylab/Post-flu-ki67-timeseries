library(shinydashboard)
library(shiny)
library(shinyBS)
library(plotly)
library(shinyjs)
library(rgl)
library(SPIA)
options(shiny.sanitize.errors = FALSE)
options(shiny.maxRequestSize=600*1024^2) 
ui <- dashboardPage(
  dashboardHeader(title = "Interactive scRNA Browser",titleWidth = 350),
  dashboardSidebar(width = 350,
                   div(style="overflow-y: scroll"),
                   tags$style(type="text/css",
                              ".shiny-output-error { visibility: hidden; }",
                              ".shiny-output-error:before { visibility: hidden; }"
                   ),
                   tags$head(tags$style(HTML(".sidebar { height: 250vh; overflow-y: auto; }
                                             .shiny-notification{position: fixed;top: 33%;left: 45%;right: 30%;}
                                             " )
                   )),
                   
                   sidebarMenu(
                     menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
                     uiOutput("projectlist"),
                     menuItem('Compare UMAP', tabName = 'tsneplot', icon = shiny::icon("angle-double-right")),
                     menuItem('Violin Plot', tabName = 'vlnplot', icon = shiny::icon("angle-double-right"))
                     
                   )
                
  ),#end dashboardSidebar
  
  ################################ DASHBOARD BODY ######################################################################
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    useShinyjs(),
    tabItems(
      tabItem(tabName = "dashboard",
              box(
                width = 12, status = "primary",solidHeader = TRUE,
                title = "Abstract",
                span(textOutput("desc"), style="font-size:20px"),br(),
                span(textOutput("geo"), style="font-size:20px")
              )
      ),
      ######################################################################################################################################
      tabItem(tabName = "tsneplot",
              box(title = "Compare Dimension Reduction Plots",solidHeader = T,width=12,status='primary',
                  fluidRow(
                    column(6,selectInput("categorya2", "Select a Category",c('Categories' = "var",'Cluster' = "clust", 'Gene Expression' = "geneexp"),selected = "clust")),
                    column(6,selectInput("categoryb2", "Select a Category",c('Categories' = "var",'Cluster' = "clust", 'Gene Expression' = "geneexp"),selected = "geneexp"))
                  ),
                  sliderInput("pointa2", "Point Size:",min = 0, max = 5, value = 1,step=.25),
                  conditionalPanel(
                    condition = "input.categorya2 == 'geneexp' || input.categoryb2 == 'geneexp'",
                    # fluidRow(
                    #   column(6,selectInput("genecolor1", "Pick color 1 for Gene Expression Plot",colors(),selected = "grey")),
                    #   column(6,selectInput("genecolor2", "Pick color 2 for Gene Expression Plot",colors(),selected = "blue"))
                    # )
                  ),
                  fluidRow(
                    column(6,conditionalPanel(
                      condition = "input.categorya2 == 'var'",
                      uiOutput("tsnea2") # Generate list of variables for left plot
                    ),
                    conditionalPanel(
                      condition = "input.categorya2 == 'geneexp'",uiOutput("gene1aui")
                    )
                    ),
                    column(6,conditionalPanel(
                      condition = "input.categoryb2 == 'var'",
                      uiOutput("tsneb2")  # Generate list of variables for right plot
                    ),
                    conditionalPanel(
                      condition = "input.categoryb2 == 'geneexp'",uiOutput("gene2aui")
                    )
                    )),
                  fluidRow(
                    column(6,checkboxInput("checklabel1", label = "Check for cell  group labelling", value = TRUE)),
                    column(6,checkboxInput("checklabel2", label = "Check for cell  group labelling", value = TRUE))
                  )
                  ),
              box(title = "Plots",solidHeader = TRUE,width=12,status='primary',
                  plotOutput("comptsne2", height = 600),
                  downloadButton('downloadtsneplot', 'Download Dimension Reduction plot')
              )
      ),#end of tsnetab
      ######################################################################################################################################
      tabItem(tabName = "vlnplot",
              box(title = "Violin",solidHeader = TRUE,width=12,status='primary',
                  fluidRow(
                    column(6,uiOutput("groupvln")),
                    column(6,uiOutput("vlnsplitby"))
                  ),
                  fluidRow(
                    column(4,checkboxInput("stacked", label = "Check to include more genes (Limit 5)", value = FALSE)),
                    column(6,conditionalPanel(
                      condition = "input.stacked == false",uiOutput("vlngene")
                    ),
                    conditionalPanel(
                      condition = "input.stacked == true",uiOutput("stackvlngene")
                    ))
                  ),
                  plotOutput("plotvlnplot", height = 600),
                  downloadButton('downloadvlnplot', 'Download Violin plot')
              )
      )#End of vlnplot tab
    )
  )
)


