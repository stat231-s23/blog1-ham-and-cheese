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
library(glue)
library(gt)

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
      ungroup() %>%
      rowwise() %>%
      mutate(
        total_count = sum(SPHSERVICE_count, CMPSERVICE_count, OPISERVICE_count, RTCSERVICE_count, IJSSERVICE_count),
        SPHSERVICE_percent = round((SPHSERVICE_count / total_count) * 100, 2),
        CMPSERVICE_percent = round((CMPSERVICE_count / total_count) * 100, 2),
        OPISERVICE_percent = round((OPISERVICE_count / total_count) * 100, 2),
        RTCSERVICE_percent = round((RTCSERVICE_count / total_count) * 100, 2),
        IJSSERVICE_percent = round((IJSSERVICE_count / total_count) * 100, 2),
        `State-psychiatric hospital` = glue("{SPHSERVICE_percent}% (n = {SPHSERVICE_count})"),
        `SMHA-funded/operated community-based program` = glue("{CMPSERVICE_percent}% (n = {CMPSERVICE_count})"),
        `Other psychiatric inpatient center` = glue("{OPISERVICE_percent}% (n = {OPISERVICE_count})"),
        `Residential treatment center` = glue("{RTCSERVICE_percent}% (n = {RTCSERVICE_count})"),
        `Institution under the justice system` = glue("{IJSSERVICE_percent}% (n = {IJSSERVICE_count})")) %>%
      
      select(STATE,`State-psychiatric hospital`, `SMHA-funded/operated community-based program`, `Other psychiatric inpatient center`, `Residential treatment center`, `Residential treatment center`) %>%
      
      
     
      arrange(STATE)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
