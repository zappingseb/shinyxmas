library(shiny)
library(shinyjs)
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://use.fontawesome.com/releases/v5.6.3/css/all.css",
              crossorigin="anonymous")
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css",
              crossorigin="anonymous")
  ),
  useShinyjs(),
  # App title ----
  titlePanel("Very shiny holidays!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of balls on the tree
      sliderInput(inputId = "bins",
                  label = "Number of balls:",
                  min = 2,
                  max = 44,
                  value = 9),
      
      # Slider for the relative size of the tree
      sliderInput(inputId = "size",
                  label = "Size of the tree:",
                  min = 1,
                  max = 4,
                  value = 1.8,step = 0.1),
      # Slider for the speed of snow (20 means 1 second for snow fall, 1 means 20 seconds for snow fall)
      sliderInput(inputId = "snow",
                  label = "Snow speed:",
                  min = 1,
                  max = 20,
                  value = 5,step = 1),
      
      # Checkboxes for the star
      fluidRow(
        column(6,
               
          checkboxInput("checkbox", label = "Star", value = TRUE)
               ),
        column(6,
          checkboxInput("checkbox_move", label = "Move the star", value = TRUE)
               )
      ),
      
      downloadButton('downloadData', 'Download as HTML'),
      tags$br(),tags$br(),
      fluidRow(
        
        column(12,"Contact the app author (Sebastian Wolf) at:")
        
      ),
      tags$br(),
      # Additional information about the app author
      fluidRow(
        column(3,HTML('<a target="_new" href="https://linkedin.com/in/zappingseb"><i class="fab fa-linkedin"></i>&nbsp;zappingseb</a>')),
        column(3,HTML('<a target="_new" href="https://github.com/zappingseb/shinyxmas"><i class="fab fa-github"></i>&nbsp;zappingseb</a>')),
        column(3,HTML('<a target="_new" href="https://medium.com/@zappingseb"><i class="fab fa-medium"></i>&nbsp;zappingseb</a>')),
        column(3,HTML('<a target="_new" href="https://www.mail-wolf.de"><i class="fas fa-globe"></i>&nbsp;mail-wolf.de</a>')) 
      ),
      tags$br(),tags$br(),
      fluidRow(
        column(12,
               
               
        div("The X-mas tree was provided by ",
            HTML('<a target="_new" href="https://codepen.io/dodozhang21/pen/imIvg">dodozhang21</a>'),
            "at codepen.io"
            )
        
      )
      )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      div(class="wrapper",
          
      htmlOutput(outputId = "panel")
          )
      
    )
  )
)