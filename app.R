#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(bslib)
library(echarts4r)

cars <- as_tibble(mtcars, rownames = NA) %>% 
        rownames_to_column() %>%
        rename(car_type=rowname)


# Define UI for application that draws a histogram
ui <- fluidPage(
# Application title
  theme = bs_theme(bootswatch = "minty"),
    titlePanel("UI Reactivity example"),
    h5("Choose Number of cylinders to view vehicles available"),
    h5("top plot: ggplot, bottom: echarts4r"),
      fluidRow(
        column(4,
          wellPanel(radioButtons("cyls",
                                "Number of cylinders:",
                                c(4,6,8)),
                    checkboxGroupInput("names","Vehicle Name",choices=cars$car_type,selected=cars$car_type[1])
          ) #wellPanel
        ),# column
    
        column(8,
          wellPanel( plotOutput("distPlot")
          ) #wellPanel
        )#column
      ),#fluidRow
    
      fluidRow(
        column(4,
        ),
        column(8,
         wellPanel(echarts4rOutput("eplot")
          
          )#wellPanel
        )#column
      )#fluidRow
)#fluidPage




server <- function(input, output) {

  # This reactive block filters the cars tibble on the radiobutton input number of cylinders
  cyl_filter <- reactive({cars %>%       
      filter(cyl == input$cyls)
  }) 
  
  # This block observes the event of cyl_filter() and updates the choices of vehicle names based on cylinders
  observeEvent(cyl_filter(), {
    car_choices<-unique(cyl_filter()$car_type)
    updateCheckboxGroupInput(inputId = "names", choices = car_choices, selected=car_choices[1])
  })
  
  # Not 100% sure about this step, but it creates the reactive tibble I map my plots to
  selected <- reactive({cyl_filter() %>%  
      req(input$names) %>%
      filter(car_type==input$names)
  })  
  
  output$distPlot <- renderPlot({
      
      selected() %>%
        ggplot(aes(mpg, disp, color=car_type))+
        geom_point(size=5)+
        labs(y = "displacement",x="mpg")
      
      
    },res=96)
  
  output$eplot <- renderEcharts4r({
    selected() |>
      e_charts (mpg) |>
      e_scatter(disp, symbol_size=15)|>
      e_tooltip(axisPointer = list(
        type = "cross"))
    
    
  })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
