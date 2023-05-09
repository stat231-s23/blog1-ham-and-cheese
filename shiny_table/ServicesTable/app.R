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
library(dplyr)
library(haven)
library(DT)

mhcld_tab_18_39 <- read_csv("table_services_18_39.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  titlePanel("Mental Health Services Data By Gender, Employment, and/or Education"),
  
  # Sidebar to choose how to filter data (One each for gender, employment, and education)
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Gender:",
                  c("All", "Male", "Female", "Missing/no diagnosis"), selected = "All"),
      selectInput("education", "Education:",
                  c("All", "Special education", "0 to 8", "9 to 11", "12 or GED", "More than 12", "Missing/no data"), selected = "All"),
      selectInput("employment", "Employment:",
                  c("All", "Missing/no data", "Full-time", "Part-time", "Employed full-time/part-time not differentiated", "Unemployed", "Not in labor force"), selected = "All"),
    ),

    # Show table
    mainPanel(
      DT::dataTableOutput("sumTable")
    )
  )
)

# Define server logic required for table
server <- function(input, output) {
  

  output$sumTable = DT::renderDataTable({
    mhcld_servicesum_table <- mhcld_tab_18_39 %>%
      filter(if (input$gender != "All") GENDER == input$gender else TRUE) %>%
      filter(if (input$education != "All") EDUC == input$education else TRUE) %>%
      filter(if (input$employment != "All") EMPLOY == input$employment else TRUE) %>%
      group_by(STATE) %>%
      summarize(SPHSERVICE_count = sum(SPHSERVICE == "Served in a state psychiatric hospital"),
                CMPSERVICE_count = sum(CMPSERVICE == "Served in a SMHA-funded/operated community-based program"),
                OPISERVICE_count = sum(OPISERVICE == "Served in 'other psychiatric inpatient center"),
                RTCSERVICE_count = sum(RTCSERVICE == "Served in a residential treatment center"),
                IJSSERVICE_count = sum(IJSSERVICE == "Served by an institution under the justice system")) %>%

    
      arrange(STATE)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
