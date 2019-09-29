#
# Name: Alex Ackerman
# Date: 09/28/2019
# Course: DA-6233-2B5
# Title: Small Business Owners 
#

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(shinydashboard)

pums <- read_csv("/Users/hackrman/Dropbox/UTSA/Data Analytics Visualization and Communication/finalProject/pums_csv/pums.csv")
owner1 <- pums %>% filter(PCT1 == 100)

fipsStateCodes <- read_csv("/Users/hackrman/Dropbox/UTSA/Data Analytics Visualization and Communication/finalProject/fips_state.csv")
fipsStateCodes <- add_row(fipsStateCodes, fips_code = "S1", post_code = "AK and WY", state = "Alaska and Wyoming")
fipsStateCodes <- add_row(fipsStateCodes, fips_code = "S2", post_code = "DE and DC", state = "Delaware and District of Columbia")
fipsStateCodes <- add_row(fipsStateCodes, fips_code = "S3", post_code = "ND and SD", state = "North Dakota and South Dakota")
fipsStateCodes <- add_row(fipsStateCodes, fips_code = "S4", post_code = "RI and VE", state = "Rhode Island and Vermont")

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        plotOutput("statePlot"),
        width = 8
      ),
      
      box(
        title = "Input",
        textInput("stateSelection",
                  "Enter a State"),
        width = 4
      )
    ),
    
    fluidRow(
      box(
        plotOutput("racePlot"),
        width = 8
      ),
      
      box(
        title = "Input",
        selectInput("raceSelection",
                  "Choose a Race",
                  choices = list(
                    Asian = "Asian",
                    Black = "Black",
                    White = "White",
                    `Native American and/or Alaska Native` = "Native American and/or Alaska Native",
                    `Native Hawaiian and/ or Other Pacific Islander` = "Native Hawaiian and/ or Other Pacific Islander",
                    `Some Other Race` = "Some Other Race"
                  )),
        width = 4
      )
    ),
    
    fluidRow(
      box(
        plotOutput("industryPlot"),
        width = 8
      ),
      
      box(
        title = "Input",
        selectInput("raceSelection2",
                    "Choose a Race to Remove",
                    choices = list(
                      Asian = "A",
                      Black = "B",
                      White = "W",
                      `Native American and/or Alaska Native` = "I",
                      `Native Hawaiian and/ or Other Pacific Islander` = "P",
                      `Some Other Race` = "S"
                    )),
        selectInput("industrySelection",
                    "Choose an Industry",
                    choices = list(
                      `Agriculture, Forestry, Fishing and Hunting` = 11,
                      Mining = 21,
                      Utilities = 22,
                      Construction = 23,
                      Manufacturing = 33,
                      `Wholesale Trading` = 42,
                      `Retail Trade` = 44,
                      `Transportation and Warehousing` = 48,
                      Information = 51,
                      `Finance and Insurance` = 52,
                      `Real Estate Rental and Leasing` = 53,
                      `Professional, Scientific, and Technical Services` = 54,
                      `Management of Companies and Enterprises` = 55,
                      `Administrative and Support and Waste Management and Remediation Services` = 56,
                      `Education Services` = 61,
                      `Health Care and Social Assistance` = 62,
                      `Arts, Entertainment, and Recreation` = 71,
                      `Accommodation and Food Services` = 72,
                      `Other Services (except Public Administration)` = 81,
                      `Public Administration` = 92
                    )),
        width = 4
      )
    )
  )
)

server <- function(input, output) {
  
    output$statePlot <- renderPlot({
        index <-fipsStateCodes %>% filter(state == input$stateSelection)
        stateData <- owner1 %>% filter(FIPST == index$fips_code)
        
        ggplot(stateData) +
            geom_bar(
                aes(fct_infreq(RACE1)),
                position = position_dodge(),
                color = "black",
                fill = "red"
            ) +
            labs(
                x = "Races",
                y = "Number of Businesses",
                title = paste("Number of Businesses in", input$stateSelection, "based on Race")
            ) +
            scale_y_continuous(
              labels = scales::comma
            ) +
            scale_x_discrete(
              name = "Races",
              breaks = c("A", "A S", "AP", 
                         "B", "B S", "B P", "B A", "BI",
                         "I", "I S", "I P", "IA",
                         "P", "PS", "S",
                         "W", "W S", "W P", "W A", "W I", "WB"),
              labels = c("Asian", "Asian AND Some Other Race", "Asian AND Native Hawaiian and/ or Other Pacific Islander",
                         "Black", "Black AND Some Other Race", "Black AND Native Hawaiian and/ or Other Pacific Islander", "Black AND Asian", "Black AND Native American and/or Alaska Native",
                         "Native American and/or Alaska Native", "Native American and/or Alaska Native AND Some Other Race", "Native American and/or Alaska Native AND Native Hawaiian and/ or Other Pacific Islander", "Native American and/or Alaska Native AND Asian",
                         "Native Hawaiian and/ or Other Pacific Islander", "Native Hawaiian and/ or Other Pacific Islander AND Some Other Race", "Some Other Race",
                         "White", "White AND Some Other Race", "White AND Native Hawaiian and/ or Other Pacific Islander", "White AND Asian", "White and Native American and/or Alaska Native", "White AND Black")
            ) +
            theme(
              axis.text.x = element_text(vjust = 1, hjust=1, angle = 90, size = 6)
            )
    })
    
    output$racePlot <- renderPlot({
      
      raceVariable <- switch(input$raceSelection,
                             "Asian" = "A",
                             "Black" = "B",
                             "White" = "W",
                             "Native American and/or Alaska Native" = "I",
                             "Native Hawaiian and/ or Other Pacific Islander" = "P",
                             "Some Other Race" = "S")
      
      raceData <- owner1 %>% filter(RACE1 == raceVariable)
      
      ggplot(raceData) +
        geom_bar(
          aes(fct_rev(fct_infreq(FIPST))),
          color = "black",
          fill = "green"
        ) +
        scale_x_discrete(
          breaks = fipsStateCodes$fips_code,
          labels = fipsStateCodes$post_code
        ) + 
        scale_y_continuous(
          labels = scales::comma
        ) +
        labs(
          x = "State",
          y = "Number of Businesses",
          title = paste("Number of", input$raceSelection, "Business Owners Separated by State")
        ) +
        theme_minimal() + 
        coord_flip()
    })
    
    output$industryPlot <- renderPlot({
      
      industryData <- owner1 %>% filter(SECTOR == input$industrySelection)
      industryData <- industryData %>% filter(RACE1 != input$raceSelection2)
      
      ggplot(industryData) +
        geom_bar(
          aes(fct_infreq(RACE1)),
          color = "black",
          fill = "yellow",
        ) +
        scale_y_continuous(
          labels = scales::comma
        ) +
        labs(
          y = "Number of Businesses",
          title = "Number of Businesses in the United States Based on Race"
        ) +
        scale_x_discrete(
          name = "Races",
          breaks = c("A", "A S", "AP", 
                     "B", "B S", "B P", "B A", "BI",
                     "I", "I S", "I P", "IA",
                     "P", "PS", "S",
                     "W", "W S", "W P", "W A", "W I", "WB"),
          labels = c("Asian", "Asian AND Some Other Race", "Asian AND Native Hawaiian and/ or Other Pacific Islander",
                     "Black", "Black AND Some Other Race", "Black AND Native Hawaiian and/ or Other Pacific Islander", "Black AND Asian", "Black AND Native American and/or Alaska Native",
                     "Native American and/or Alaska Native", "Native American and/or Alaska Native AND Some Other Race", "Native American and/or Alaska Native AND Native Hawaiian and/ or Other Pacific Islander", "Native American and/or Alaska Native AND Asian",
                     "Native Hawaiian and/ or Other Pacific Islander", "Native Hawaiian and/ or Other Pacific Islander AND Some Other Race", "Some Other Race",
                     "White", "White AND Some Other Race", "White AND Native Hawaiian and/ or Other Pacific Islander", "White AND Asian", "White and Native American and/or Alaska Native", "White AND Black")
        ) +
        theme(
          axis.text.x = element_text(vjust = 1, hjust=1, angle = 90)
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)




















