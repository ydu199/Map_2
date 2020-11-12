#Date needed for shinyApp:
library(shiny)
library(dplyr)
library(magrittr)
library(ggplot2)
#hurr<-read.csv("/Users/duyu/Desktop/MA615/hur.csv")
hurr1<-read.csv("hurrp.csv")
hurr2<-read.csv("hurrd.csv")
hurr1<-hurr1[,2:9]
hurr2<-hurr2[,2:9]
library(plotly)
library(maps)
state <- map_data("state")
library(scales)
  
ui <-fluidPage(
  
  titlePanel(
    fluidRow(
      column(8, 
             h3("Mapping with FEMA data"),
             h5("by Yu Du. All the codes can be approached from",
                a(href="https://github.com/ydu199/Map_2.git ",
                  "My Github"))
      ))),
  
  
  sidebarLayout(
    tabsetPanel(
      conditionalPanel(
        'input.dataset === "hurr1"'),
      
      conditionalPanel(
        'input.dataset === "hurr2"',
      )
    ),
    
    navbarPage(
      title="",
      tabPanel("Introduction", 
               br(), 
               p("The data is collected from the", a(href="https://www.fema.gov/openfema-data-page/public-assistance-funded-projects-details-v1",
                                                                                                 "FEMA Website"),",selecting the year on 2009 to 2020 and incident type on Hurricane. More maps and explanations on variables will on the next few pages."),
               br(),
      ),
      
      tabPanel("Maps",
               tabsetPanel( 
                 
                 tabPanel("Table1 for total project amount",
                          # Create a new Row in the UI for selectInputs
                          fluidRow(
                            column(4,
                                   selectInput("year",
                                               "Year:",
                                               c("All",
                                                 unique(as.character(hurr1$year))))
                            ),
                            column(4,
                                   selectInput("state",
                                               "State:",
                                               c("All",
                                                 unique(as.character(hurr1$state))))
                            )
                          ),
                          # Create a new row for the table.
                          DT::dataTableOutput("table1")),
                 
                 tabPanel("Table2 for total federal share obligated",
                   
                          fluidRow(
                            column(4,
                                   selectInput("year",
                                               "Year:",
                                               c("All",
                                                 unique(as.character(hurr2$year))))
                            ),
                            column(4,
                                   selectInput("state",
                                               "State:",
                                               c("All",
                                                 unique(as.character(hurr2$state))))
                            )
                          ),
                          
                          
                          # Create a new row for the table.
                          DT::dataTableOutput("table2")),                     

                 
                 tabPanel("ggplot for Table1",
                          p("projectAmount: The estimated total cost of the Public Assistance grant project in dollars, without administrative costs. This amount is based on the damage survey."),
                         
                          
                          plotOutput("plot1")),
                 
                 tabPanel("ggplot for Table2",
                          p("federalShareObligated: The Public Assistance grant funding available to the grantee (State) in dollars, for sub-grantee's approved Project Worksheet."),
                          
                         
                            
                            
                            
                        
                          plotOutput("plot2")),          
                 
                 tabPanel("ggplotly for Table1",
                        
                         
                          
                          plotlyOutput("plot3")),
                 
                 tabPanel("ggplotly for Table2",
                          p("ggplotly provides clearer visualization: Some states have larger total project Amount than total federal share obligated."),
                          
                          
                          plotlyOutput("plot4"))
               )
      )
    )
  )
)
         

server <- function(input, output) {
  
  # Filter data based on selections
  output$table1 <- DT::renderDataTable(DT::datatable({
    data <- hurr1
    if (input$year!= "All") {
      data <- data[data$year == input$year,]
    }
    if (input$state != "All") {
      data <- data[data$state == input$state,]
    }
    
    data
  }))
  
  output$table2 <- DT::renderDataTable(DT::datatable({
    data2 <- hurr2
    if (input$year!= "All") {
      data2 <- data2[hurr2$year == input$year,]
   
    }
    
    if (input$state != "All") {
      data <- data[hurr2$state == input$state,]
    }
    
    data2
  }))
  
  output$plot1<-renderPlot({
  hurr1$project<-as.numeric(hurr1$project)
  hurr1$Amount=cut(hurr1$project, breaks = c(0,2e+02,2e+04,2e+06,2e+08,2e+10),
                    include.lowest = TRUE)
    ggplot1<-ggplot() + geom_polygon(data =hurr1, aes(long, lat, group = group,
                                                    fill = Amount), 
                                   color = "grey", size = 0.2, alpha = 1.6) + 
      geom_polygon(data = state, aes(long,lat, group = group),
                   color = "black", fill = "white", size = 0.2, alpha = 0.3) +
      scale_fill_brewer(palette = "Blues") +
      ggtitle("Total Project Amount in 2009-2020") +theme(plot.title = element_text(hjust = 0.5))
    ggplot1
    
    
  })
  
  output$plot2<-renderPlot({
    hurr2$federal<-as.numeric(hurr2$federal)
    hurr2$Amount = cut(hurr2$federal, breaks = c(0,2e+02,2e+04,2e+06,2e+08,2e+10),
                       include.lowest = TRUE)
    
    ggplot2<-ggplot() + geom_polygon(data =hurr2, aes(x = long, y = lat, group = group,
                                                    fill = Amount), 
                                   color = "grey", size = 0.2, alpha = 1.6) + 
      geom_polygon(data = state, aes(x = long, y = lat, group = group),
                   color = "black", fill = "white", size = 0.2, alpha = 0.3) +
      scale_fill_brewer(palette = "Blues") +
      ggtitle("Total Federal Share Obligated in 2009-2020") +theme(plot.title = element_text(hjust = 0.5))
    ggplot2 
  
  })
  
  output$plot3<-renderPlotly({
    hurr1$Amount= cut(hurr1$project, breaks = c(0,2e+02,2e+04,2e+06,2e+08,2e+10),
                      include.lowest = TRUE)
    plot4<-ggplot(data =hurr1, aes(long,lat,group = group)) + geom_polygon(aes(
    fill = Amount), color = alpha("white",1/2), size = 0.05) + 
    geom_polygon(data = state,color = "grey", fill=NA) +
    scale_fill_brewer(palette = "Blues") +
    ggtitle("Total Project Amount in 2009 -2020") +theme_void()
    fig<-ggplotly(plot4)
    fig

    
  })
  
  output$plot4<-renderPlotly({
    hurr2$Amount= cut(hurr2$federal,breaks = c(0,2e+02,2e+04,2e+06,2e+08,2e+10),
                      include.lowest = TRUE)
    plot5<-ggplot(data =hurr2, aes(long, lat,group=group)) + geom_polygon(aes(
      fill = Amount), color = alpha("white",1/2), size = 0.05) + 
      geom_polygon(data = state,color = "grey", fill=NA) +
      scale_fill_brewer(palette = "Blues") +
      ggtitle("Total Federal Share Obligated in 2009-2020") +theme_void()
    fig<-ggplotly(plot5)
    fig
  }) 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
