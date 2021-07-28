#
# Map Ui
library(leaflet)

mapUI <- function(id) {
  fluidRow(
    box(
      width = 12,
      title = textOutput(NS(id, "outcomeHeading")),
      leafletOutput(NS(id, "dataMap"))
    ),
    box(
      width = 12,
      title = "Exclusions",
      solidHeader = TRUE,
      status = "danger",
      collapsible = TRUE,
      collapsed = TRUE,
      tags$div(
        id = "resetButtonContainer",
        class = "col-md-12",
        actionBttn(NS(id, "resetFilter"), "Reset Filters", style = "bordered", color = "danger", icon = icon("sliders"))
      ),
      tags$div(
        class = "col-md-12",
        infoBoxOutput(NS(id, "ageFiltered"), width = 4),
        infoBoxOutput(NS(id, "genderFiltered"), width = 4),
        infoBoxOutput(NS(id, "empFiltered"), width = 4)
      ),
      tags$div(
        class = "col-md-12",
        infoBoxOutput(NS(id, "hhFiltered"), width = 4),
        infoBoxOutput(NS(id, "paredFiltered"), width = 4),
        infoBoxOutput(NS(id, "imparFiltered"), width = 4)
      ),
      tags$div(
        class = "col-md-12",
        infoBoxOutput(NS(id, "earnFiltered"), width = 4),
        infoBoxOutput(NS(id, "lawFiltered"), width = 4),
        infoBoxOutput(NS(id, "rtlFiltered"), width = 4)
      )
    )
  )
}