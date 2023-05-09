# this code reproduces the scatterplot panel only from the electric skateboards app
library(shiny)
library(tidyverse)
library(ggrepel)
library(tidytuesdayR)
library(shinythemes)
library(scales)
library(sf)
library(viridis)

###############
# import data #
###############
samhsa_comorbidity_18_24 <- read_csv("data/samhsa_comorbidity_18_24.csv")
samhsa_comorbidity_25_35 <- read_csv("data/samhsa_comorbidity_25_35.csv")
samhsa_comorbidity_35_40 <- read_csv("data/samhsa_comorbidity_35_40.csv")
samhsa_comorbidity = bind_rows(samhsa_comorbidity_18_24, samhsa_comorbidity_25_35, samhsa_comorbidity_35_40)
us_map <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  st_as_sf()

###################################################
# define choice values and labels for user inputs #
###################################################
# define vectors for choice values and labels 

cond_choice_values <- c(
  "Trauma- and stressor-related disorders", 
  "Anxiety disorders", 
  "Attention deficit/hyperactivity disorder (ADHD)",
  "Conduct disorders",
  "Delirium, dementia",
  "Bipolar disorders",
  "Depressive disorders",
  "Oppositional defiant disorders",
  "Pervasive developmental disorders",
  "Personality disorders",
  "Schizophrenia or other psychotic disorders",
  "Alcohol or substance use disorders",
  "Other disorders/conditions"
                        )


############
#    ui    #
############
ui <- navbarPage(
  title="Demographic Review on Student Mental Health",
  
  tabPanel(
    title = "Choropleth",
    
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId = "pt_cond"
                     , label = "Choose condition:"
                     , choices = cond_choice_values
                     , selected = "Anxiety disorders"),
        shinythemes::themeSelector()
      ),
      mainPanel(
        plotOutput(outputId = "choropleth")
      )
    )
  )
)

############
# server   #
############
server <- function(input,output){
  
  # create choropleth
  
  output$choropleth <- renderPlot({

    # create state-by-state summary data
    samhsa_condition <- samhsa_comorbidity %>%
      # filter out those without at least one diagnosis
      filter(MH1 == input$pt_cond) %>%
      # group by state, create value for percentage w/ comorbid diagnoses within each group
      group_by(STATE) %>%
      summarize(count = n(), comorb_count = sum(MH2 != "Missing/no diagnosis"), comorb_percent = comorb_count / count) %>%
      # add geometric data
      right_join(us_map, by = c("STATE" = "ID"))
    
    # create plot with labels
    ggplot(samhsa_condition, aes(geometry=geom, fill = comorb_percent)) +
      geom_sf() +
      theme_void() +
      labs(fill = "% with additional diagnosis"
           , title = "Comorbidity of Mental Health Diagnoses"
           , subtitle = "ages 18-39"
           , caption = "Source: SAMHSA Mental Health Client Level Data, 2020") +
      scale_fill_viridis(option = "magma", direction = -1, labels = percent)
  })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)