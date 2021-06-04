library(shiny)
library("beeswarm")
library(dplyr)
library(stringr)
library(shinythemes)

options(shiny.maxRequestSize=30*1024^2)

# Define UI ----
ui <- fluidPage(
  theme = shinytheme("paper"),
  titlePanel("CellSize TrackR"),
  
  sidebarLayout(
    sidebarPanel("",
                 fileInput("file1", "Import a csv",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                 fluidRow(
                   width = 8,
                   column (
                     width = 6,
                     numericInput(
                       'timemin',
                       'time min:',
                       1,
                       min = 1
                     )
                   ),
                   column (
                     width = 6,
                     numericInput(
                       'timemax',
                       'time max:',
                       19,
                       min = 2
                     )
                   )
                 ),
                 textInput("fieldnum", "field numbers (separated by comma, ex: 1,2,3)","1"),
                 numericInput(
                   'cellsize',
                   'cell size threshold:',
                   300,
                   min = 1
                 ),
                 fluidRow(
                    width = 8,
                    column (
                      width = 6,
                      textInput('xlabel','x label', 'Time Point (h)')
                    ),
                    column (
                      width = 6,
                      textInput('ylabel', 'y label', 'Cell Size')
                    )
                  ),
                 textInput('graphlabel:','graph label','IVIG'),
                 actionButton('plot_button','Plot',style='background-color: #FFF; /* White */;
                             padding:5px; font-size:110%; width:100px; align:centering; border-radius: 12px;')
                 
                 ),
    mainPanel("My Plot",
              plotOutput("plot0",inline=FALSE)
              
              
              
              )
  )
)

# Define server logic ----
server <- function(input, output) {
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  
  field.nums <- reactive({
    r = c()
    for ( s in str_split(input$fieldnum, ",") ){
      s <- trimws(s)
      s <- as.numeric(s)
      r <- append(r, s)
      
    }
    r[r!=""]
  })
  
  # TODO: modify parameters below
  dot_size = 0.03 # adjust the dot size in the plot
  dot_space = 0.9 # adjust dot space
  field_num <- reactive({
    r = c()
    for ( s in str_split(input$fieldnum, ",") ){
      s <- trimws(s)
      s <- as.numeric(s)
      r <- append(r, s)
      
    }
    r[r!=""]
  })
    
  time_range_l<- reactive({
    input$timemin  # TODO: time point range left
  }) 
  
  time_range_r <- reactive({
    input$timemax  # TODO: time point range right
  })
  
  xlabel <- reactive({
    input$xlabel
  })
  
  ylabel <- reactive({
    input$ylabel
  })
 
  graph_label <- reactive({
    input$graphlabel
  })
 
  cellsize_threshold <- reactive({
    input$cellsize 
  })
 
  # get the data set
  data <- reactive({
    df <- NULL
    validate(
      need(input$file1 != "", "  Notice: Please select a data file !!")
    )
    if(is.null(input$file1)) {
      return(NULL)
    } else {
      df <- read.csv(input$file1$datapath, header = TRUE)
    }
    
    return(df[rowSums(is.na(df)) <= 5,])   # remove all rows with NA
    # df[rowSums(is.na(df)) != ncol(df),]
  })  
  

  
  observeEvent(input$plot_button, {
    print('Here')
    output$plot0 <- renderPlot(
      {
        # 1. filtering, based on field number&time range
        subset <- data() %>% filter(Field.Number %in% field_num()) %>% 
          filter(Time.Point >= time_range_l()) %>% 
          filter(Time.Point <= time_range_r()) %>%
          filter(Cell.Size <= cellsize_threshold())
        # Plotting
        beeswarm(Cell.Size~Time.Point, data=subset,  pch=19, method="swarm",
                 cex=dot_size, xlab=xlabel(), ylab=ylabel(),
                 main=graph_label(), spacing=dot_space, xaxt="n")
        axis(1, at=c(time_range_l():time_range_r()), labels=NULL, cex.axis=0.8)
      }
    )

  })
}

# Run the app ----
shinyApp(ui = ui, server = server)