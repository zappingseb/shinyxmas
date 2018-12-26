library(dplyr)
library(stringr)
library(glue)

# Read in a CSS template file
style_template_text <<- read.delim("style_template.txt",row.names=NULL,stringsAsFactors = F,header=F)

# Read in the box-shadow values from codepen as data.frames 
css_animation <<- lapply(list.files(pattern="color_\\d{1,3}.txt"),
                        function(myfile){
                          color_data <- read.delim(myfile,sep = " ",row.names = NULL,header=F,stringsAsFactors = F)
                          color_data <- color_data[,-1]
                          color_data[,"V6"] <- gsub(",","",color_data[,"V6"]) %>% gsub(pattern=";",replacement="")
                          color_data
                        }
)

# Name the list of box shadow tables by the names of the txt files
names(css_animation) <- gsub(".txt","",list.files(pattern="color_\\d{1,3}.txt"))

# Default settings of rows being balls from codepen
balls <- c(33,52,66,71,12,31,6,35,68)

# Finding rows that were not set to be balls
green <- which(css_animation[["color_0"]][,"V6"]=="#649632")

# All rows that do not contain borders of the tree in box-shadow
possible_balls <- c(balls,green)


#' Fill box-shadow information in a style template
#' 
#' Needs global variable style_tempalate_text
#' 
#' @param list_of_styles \code{list} A list with the elements
#'   color_0,
#'   color_25,
#'   color_50,
#'   color_75,
#'   color_100 -->
#' Each list element contains a data frame with 5 columns
#' where column 1 and 2 are the positions of the 
#' color, column 5 is the HEX color value itself
#'    
#' @author Sebastian Wolf
derive_css <- function(list_of_styles){
  
  # paste each table into a one line string and assign this to a global variable
  lapply(names(list_of_styles),function(color_name){
    
    assign(x=color_name,
           paste0(
             apply(list_of_styles[[color_name]],1,paste,collapse=" "),collapse=", "
             
           )#paste0
           ,envir = .GlobalEnv)#assign
    
  })
  
  # use glue to substitute the variables inside "style_template_text" by
  # the global variables
  my_text_glued <- apply(
    style_template_text,1,function(x){
      glue(as.character(x))
    })
  
  # collapse the style CSS into one string to insert it into
  # an HTML file finally
  return(paste0(gsub("\n"," ",my_text_glued),collapse=" "))
}

#' Function to replace box-shadow colors by random number of balls
#' 
#' This function replaces randomely lines in the list_of_styles data
#' frames by colors of balls. The color of the ball is chosen
#' randomely while the row (in CSS file) which is taken is defined
#' by row.ids. All other rows that describe pixels within the
#' tree will be painted green.
#' 
#' @param list_of_styles \code{list} A list with the elements
#'   color_0,
#'   color_25,
#'   color_50,
#'   color_75,
#'   color_100 -->
#' Each list element contains a data frame with 5 columns
#' where column 1 and 2 are the positions of the 
#' color, column 5 is the HEX color value itself
#'    
#' @param row.ids \code{numeric} A list of rows (to represent pixels of the tree)
#'  that shall be replaced with balls
#'  @param possible_balls \code{numeric} A vector of rows that
#'    possibly represent parts of the x-mas tree
#' @author Sebastian Wolf
color_balls <- function(row.ids, list_of_styles, possible_balls){
  
  # Derive ball colors for animation
  
  ball_colors <- c("red","yellow","blue","pink","orange")
  
                           #0          #25        #50        #75        #100
  balls <- list("red"    = c("#eb0000", "#649632", "#eb0000", "#649632", "#eb0000" ),
                "yellow" = c("#f5cd2d", "#FF8C00", "#649632", "#FF8C00", "#f5cd2d" ),
                "blue"   = c("#96cbcf", "#649632", "#1E90FF", "#1E90FF", "#96cbcf"),
                "pink"   = c("#FF00FF", "#eb0000", "#649632", "#FF00FF", "#FF00FF"),
                "orange" = c("#649632", "#FF8C00", "#FFA07A", "#FF8C00", "#FFA07A"),
                "noball" = c("#649632", "#649632", "#649632", "#649632", "#649632")
                )
  
  # Randomely select row.ids ball colors
  
  ball_colors_take <- sample(ball_colors,length(row.ids),replace=T)

  # color the row ids for the different animation steps
  # by the desired color
        
  list_of_styles[["color_0"]][row.ids,"V6"] <- 
    unname(unlist(lapply(balls[ball_colors_take],function(x)x[1])))
  
  list_of_styles[["color_25"]][row.ids,5] <- 
    unlist(lapply(balls[ball_colors_take],function(x)x[2]))
  
  list_of_styles[["color_50"]][row.ids,5] <- 
    unlist(lapply(balls[ball_colors_take],function(x)x[3]))
  
  list_of_styles[["color_75"]][row.ids,5] <- 
    unlist(lapply(balls[ball_colors_take],function(x)x[4]))
  
  list_of_styles[["color_100"]][row.ids,5] <- 
    unlist(lapply(balls[ball_colors_take],function(x)x[5]))
  
  # color all non animated fields green
  
  green_rows <- setdiff(possible_balls,row.ids)
  
  list_of_styles[["color_0"]][green_rows,5] <- rep("#649632",length(green_rows))
  list_of_styles[["color_25"]][green_rows,5] <- rep("#649632",length(green_rows))  
  list_of_styles[["color_50"]][green_rows,5] <- rep("#649632",length(green_rows))  
  list_of_styles[["color_75"]][green_rows,5] <- rep("#649632",length(green_rows))  
  list_of_styles[["color_100"]][green_rows,5] <- rep("#649632",length(green_rows))  
  
  return(list_of_styles)
  
}

server <- function(input,output,session){
  #-------------
  # In case of Animation changes
  observeEvent({input$bins
    input$snow},{
    
      
    # Change the snowspeed
    assign("snowspeed",100/input$snow,envir = .GlobalEnv)
    
    # Paint rows with balls in random colors
    css_list <- color_balls(row.ids = sample(possible_balls,input$bins,replace = F),
                            list_of_styles = css_animation,
                            possible_balls = possible_balls
    )
    
    # Derive a CSS string from randomely colored balls
    style_css <- derive_css(css_list)
    
    # Repaint the xmasTree
    output$panel <- renderUI(
      HTML(
        '<div class="xmasTree"/></div>' 
      )
    )#renderUI
    
    # Change the style of the CSS in the head
    shinyjs::runjs(
      glue('$("head").find(\"style:contains(\'keyframes\')\").last()')
    )
    shinyjs::runjs(
      glue("$('head').append(\"<style>{style_css}</style>\");")
    )
  })
 
  #-------------
  # In case of the star being wanted or not
  observeEvent(input$checkbox,{
    
    # Add CSS to the header that contains a font-awesome star  character
    if(input$checkbox){
      shinyjs::runjs(
        "$('head').append(\"<style>.xmasTree:before{content:'\\\\f005'}</style>\");"
        
      )
      
      # or NOT
    }else{
      shinyjs::runjs(
        "$('head').append(\"<style>.xmasTree:before{content:''}</style>\");"
        
      )
    }
    
  })
  
  #-------------
  #
  # In case of an unwanted animation of the start, delete the animation by adding a CSS with defect animation
  observeEvent(input$checkbox_move,{
    
    if(input$checkbox_move){
      shinyjs::runjs(
        "$('head').append(\"<style>.xmasTree:before{animation:  flash 1s ease  infinite forwards;}</style>\");"
        
      )
    }else{
      shinyjs::runjs(
        "$('head').append(\"<style>.xmasTree:before{animation:defect}</style>\");"
        
      )
    }
    
  })
  
  #-------------
  # In case of a size change of the tree change the font-size of the wrapper div
  observeEvent(input$size,{
    size <- input$size
    shinyjs::runjs(glue("$('.wrapper').css('font-size','{size}em');"))
    
  })
  
  
}