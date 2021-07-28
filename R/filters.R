#
# Filters Module
require(shinydashboard)
require(shinyWidgets)

filters <- function(id, data, countries.list) {
  ns <- NS(id)
  
  demographics.list <- c("Gender" = "gender", "Age" = "age", "Household Size" = "household")
  outcomes.list <- c("Literacy" = "lit_score", "Numeracy" = "num_score")
  sd.choices <- c("<= -3 SD" = -3, "-2 SD" = -2, "-1 SD" = -1, "Mean" = "mean", "1 SD" = 1,
                  "2 SD" = 2, ">= 3 SD" = 3)
  
  sd.range <- function(values, choice) {
    if (choice == "mean") return(mean(values))
    
    return(sd(values, na.rm = TRUE))
  }
  
  tags$div(class = "filterSpacing",
      dropMenu(
        actionLink("oL", "Outcome"),
        prettyRadioButtons(ns("outcomeSelect"), label = "Outcome", 
                           choices = outcomes.list,
                           selected = outcomes.list[["Literacy"]],
                           icon = icon("check"),
                           bigger = TRUE),
        placement = "right",
        options = dropMenuOptions(zIndex = 9999)
      ),
      dropMenu(
        actionLink("cL", "Countries"),
          multiInput(
            inputId = ns("countrySelect"), label = "Countries",
            choiceValues = countries.list$code,
            choiceNames = mapply(countries.list$piaac_name, countries.list$flagURL, FUN = function(country, flagUrl) {
              HTML(paste(
                tags$img(src=flagUrl, width=20, height=15),
                country
              ))
            }, SIMPLIFY = FALSE, USE.NAMES = FALSE),
            selected = countries.list$code,
            options = list(non_selected_header = "Available:",
                           selected_header = "Selected:")
          ),
          actionBttn(ns("miSelectAll"), "Select All", style = "pill", size = "sm"),
          actionBttn(ns("miDeselectAll"), "Deselect All", style = "pill", size = "sm"),
        placement = "right"
      ),
        dropMenu(
          actionLink("aL", "Age"),
          prettyCheckboxGroup(ns("ageCheck"), label = "Age", 
                              choices = levels(data$age),
                              selected = levels(data$age)),
          placement = "right"
        ),
        dropMenu(
          actionLink("gL", "Gender"),
          prettyCheckboxGroup(ns("genderCheck"), label = "Gender", 
                              choices = levels(data$gender),
                              selected = levels(data$gender)),
          placement = "right"
        ),
        dropMenu(
          actionLink("eL", "Employment"),
          prettyCheckboxGroup(ns("empCheck"), label = "Employment", 
                              choices = levels(data$emp),
                              selected = levels(data$emp)),
          placement = "right"
        ),
        dropMenu(
          actionLink("nhL", "Number in Household"),
          checkboxInput(ns("hhEnabled"), "Enabled"),
          conditionalPanel(
            condition = "input.hhEnabled == 1",
            sliderTextInput(ns("hhSlider"),
                            label = "Number in Household\nChoose a range:",
                            choices = levels(data$num_HH),
                            selected = c("1","3")),
            ns = NS(id)
          ),
          placement = "right"
        ),
        dropMenu(
          actionLink("peL", "Parent's Education"),
          prettyCheckboxGroup(ns("paredCheck"), label = "Parent's Education", 
                              choices = levels(data$pared),
                              selected = levels(data$pared)),
          placement = "right"
        ),
        dropMenu(
          actionLink("piL", "Parent's Immigration Status"),
          prettyCheckboxGroup(ns("imparCheck"), label = "Parent's Immigration Status", 
                              choices = levels(data$impar),
                              selected = levels(data$impar)),
          placement = "right"
        ),
        dropMenu(
          actionLink("eaL", "Earnings"),
          prettyCheckboxGroup(ns("earnCheck"), label = "Decile of participant's earnings in relation to their country", 
                              choices = levels(data$earnhrdcl),
                              selected = levels(data$earnhrdcl)),
          placement = "right"
        ),
        dropMenu(
          actionLink("lawL", "Learning at Work"),
          checkboxInput(ns("lawEnabled"), "Enabled"),
          conditionalPanel(
            condition = "input.lawEnabled == 1",
            sliderInput(
              inputId = ns("lawSlider"),
              label = "Choose a range:", 
              min = min(data$learnatwork, na.rm = TRUE),
              max = max(data$learnatwork, na.rm = TRUE),
              value = c(c(mean(data$learnatwork, na.rm = TRUE) - 1, mean(data$learnatwork, na.rm = TRUE) + 1)),
              round = -2
            ),
            ns = NS(id)
          ),
          placement = "right"
        ),
        dropMenu(
          actionLink("rtlL", "Ready to Learn"),
          checkboxInput(ns("rtlEnabled"), "Enabled"),
          conditionalPanel(
            condition = "input.rtlEnabled == 1",
            sliderInput(
              inputId = ns("rtlSlider"),
              label = "Choose a range:", 
              min = min(data$readytolearn, na.rm = TRUE),
              max = max(data$readytolearn, na.rm = TRUE),
              value = c(mean(data$readytolearn, na.rm = TRUE) - 1, mean(data$readytolearn, na.rm = TRUE) + 1),
              round = -2
            ),
            ns = NS(id)
          ),
          placement = "right"
        )
    )
}

filterServer <- function(id, allCountries, data) {
  moduleServer(id, function(input, output, session) {
    ## Select/Deselect Countries
    observeEvent(input$miSelectAll, {
      updateMultiInput(
        session = session,
        inputId = "countrySelect",
        selected = allCountries$code
      )
    })
    observeEvent(input$miDeselectAll, {
      updateMultiInput(
        session = session,
        inputId = "countrySelect",
        selected = ""
      )
    })
    
    ## Return list of reactives for other modules
    vars <- reactiveValues()
    vars$countrySelect = reactive(input$countrySelect)
    vars$outcomeSelect = reactive(input$outcomeSelect)
    vars$ageSelect = reactive(input$ageCheck)
    vars$genderSelect = reactive(input$genderCheck)
    vars$empSelect = reactive(input$empCheck)
    vars$hhEnabled = reactive(input$hhEnabled)
    vars$hhSelect = reactive(input$hhSlider)
    vars$paredSelect = reactive(input$paredCheck)
    vars$imparSelect = reactive(input$imparCheck)
    vars$earnSelect = reactive(input$earnCheck)
    vars$lawEnabled = reactive(input$lawEnabled)
    vars$lawSelect = reactive(input$lawSlider)
    vars$rtlEnabled = reactive(input$rtlEnabled)
    vars$rtlSelect = reactive(input$rtlSlider)
    vars$session = session
    
    observe({
      
      ## Send filtered data
      vars$filteredData <- data %>% 
        filter(if(!is.null(input$countrySelect)) code %in% input$countrySelect else code = "") %>% 
        filter(age %in% input$ageCheck,
               emp %in% input$empCheck,
               gender %in% input$genderCheck,
               pared %in% input$paredCheck,
               impar %in% input$imparCheck,
               earnhrdcl %in% input$earnCheck
               ) %>% 
        filter(if(input$hhEnabled == 1) num_HH %in% input$hhSlider else num_HH %in% levels(num_HH)) %>% 
        filter(if(input$rtlEnabled == 1) between(readytolearn, input$rtlSlider[1], input$rtlSlider[2]) else readytolearn %in% readytolearn) %>% 
        filter(if(input$lawEnabled == 1) between(learnatwork, input$lawSlider[1], input$lawSlider[2]) else learnatwork %in% learnatwork)
        
      
    })
    
    return(vars)
    
    
  })
}