#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(htmltools)

load("data/piaac.RData")
load("data/piaac_countries.RData")

countries <- countries2
countries.list <- read_csv("data/piaac_countries.csv")
data <- master.df.final

# Order factors
data$earnhrdcl <- ordered(data$earnhrdcl, levels = c("Lowest Decile", "2nd Decile", "3rd Decile", "4th Decile", "5th Decile",
                          "6th Decile", "7th Decile", "8th Decile", "9th Decile", "Highest Decile", "Unknown"))

data$age <- ordered(data$age, levels = c("24 Or Less", "25-34", "35-44", "45-54", "55 Plus"))
data$num_HH <- ordered(data$num_HH, levels = c("1", "2", "3", "4", "5", "6"))
levels(data$num_HH)[levels(data$num_HH) == "6"] <- "6+"

# Drop low level of NAs and unknown factors
data <- data %>% 
    drop_na(emp)

data <- subset(data, emp != "Unknown")
data <- subset(data, gender != "Unknown")

data$gender <- droplevels(data$gender)
data$emp <- droplevels(data$emp)

# Define UI for application 

header <- dashboardHeader(title = "PIAAC Discovery")

sidebar <- dashboardSidebar(
            sidebarMenu(id = "sidebar1", 
                menuItem("Map", tabName = "mapTab", icon = icon("map")),
                menuItem("Graphs", tabName = "graphsTab", icon = icon("chart-bar")),
                menuItem("Information", tabName = "infoTab", icon = icon("info-circle")),
                hr(),
                p("Filters", class = "filterSpacing"),
                filters("filter1", data, countries.list)
            )
        )

body <- dashboardBody(
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
            ),
            tabItems(
                tabItem(tabName = "mapTab",
                    h2("Data Map"),
                    mapUI("map1")
                ),
                tabItem(tabName = "graphsTab",
                    h2("Graph Data"),
                    p("Coming Soon!")
                ),
                tabItem(tabName = "infoTab",
                    h2("Information"),
                    h3("Disclaimer"),
                    p("This application is not associated with OECD or PIAAC.
                      All data is available at ",
                      a("https://www.oecd.org/skills/piaac/data/.", href = "https://www.oecd.org/skills/piaac/data/"),
                      "Additionally, data has not been weighted for analysis. Any standard errors 
                      produced from the data are biased. Please use your judgment when investigating subgroups 
                      within and between countries, as sample sizes aren't large enough for accurate conclusions in many cases.
                      In other words, have fun exploring but be cautious in drawing conclusions! The purpose of the app is 
                      to guide research interest rather than test hypotheses. See ", a("About PIAAC", href = "https://www.oecd.org/skills/piaac/about/"),
                      "for more information about the survey."),
                    h3("About the data"),
                    p("Data is taken from Cycle 1, Round 1 of PIAAC survey distribution. As such, 22 countries are represented. 
                      Round 1 took place in 2012-14. Variables are described below:"),
                    p(strong("Outcomes:"), "Outcomes in the dataset are literacy and numeracy. To account for bias, PIAAC psychometricians used 
                      plausible values instead of point estimates to estimate values for literacy and numeracy constructs. The value in the data is 
                      the average across all 10 plausible values for each participant."),
                    p(strong("Gender:"), "The PIAAC survey classifies gender into Male, Female, or Unknown. Only 2 responses were 'Unknown' and therefore 
                      removed from the dataset."),
                    p(strong("Employment:"), "The survey lists employment, unemployment, not in the labor force, and unknown as potential categories. For brevity, 
                      I combined 'Unemployed' and 'Not in the labor force' into a single 'Not Employed' category."),
                    p(strong("Learning at Work"), " and ", strong("Ready to Learn"), " are both constructs derived from items on the background questionnaire. 
                      The items are scaled so that the mean value is appx. 2 and each difference of 1 is a standard deviation.")
                )
            )
        )

ui <- dashboardPage(header, sidebar, body)

# Define server logic 
server <- function(input, output, session) {
    # Filter logic
    fs <- filterServer("filter1", countries.list, data)
    
    # Map logic
    mapServer("map1", countries, data, countries.list, fs)
}

# Run the application 
shinyApp(ui = ui, server = server)
