#
# Server logic for map data
library(leaflet)
library(RColorBrewer)
library(sp)
library(rgeos)

mapServer <- function(id, map, data, key, filters) {
  moduleServer(id, function(input, output, session) {
    
    outcome <- c("num_score" = "Numeracy", "lit_score" = "Literacy")
    
    
    output$dataMap <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 2, maxZoom = 8)) %>% 
        addTiles() %>% 
        setView(lat = 49, lng = 43, zoom = 2)
    })
    
    # Not the DRYest way
    observeEvent(input$resetFilter, {
      updateMultiInput(
        session = filters$session,
        inputId = "countrySelect",
        selected = key$code
      )
      updatePrettyCheckboxGroup(
        session = filters$session,
        inputId = "ageCheck",
        selected = levels(data$age)
      )
      updatePrettyCheckboxGroup(
        session = filters$session,
        inputId = "genderCheck",
        selected = levels(data$gender)
      )
      updatePrettyCheckboxGroup(
        session = filters$session,
        inputId = "empCheck",
        selected = levels(data$emp)
      )
      updateCheckboxInput(
        session = filters$session,
        inputId = "hhEnabled",
        value = FALSE
      )
      updatePrettyCheckboxGroup(
        session = filters$session,
        inputId = "paredCheck",
        selected = levels(data$pared)
      )
      updatePrettyCheckboxGroup(
        session = filters$session,
        inputId = "imparCheck",
        selected = levels(data$impar)
      )
      updatePrettyCheckboxGroup(
        session = filters$session,
        inputId = "earnCheck",
        selected = levels(data$earnhrdcl)
      )
      updateCheckboxInput(
        session = filters$session,
        inputId = "lawEnabled",
        value = FALSE
      )
      updateCheckboxInput(
        session = filters$session,
        inputId = "rtlEnabled",
        value = FALSE
      )
    })
    
    observeEvent(filters$countrySelect(),{
        leafletProxy("dataMap") %>% clearShapes()
      }, ignoreInit = TRUE,
      ignoreNULL = FALSE)
    
    filterBox <- function(data, var, sel, title, check = NULL) {
      react <- reactiveVal()
      observeEvent({
        filters[[sel]]()
        if(!is.null(check)) filters[[check]]()
        }, {
        react(
          tags$div(
            if (!is.null(check)) {
              if (isFALSE(filters[[check]]())) {
                ""
              } else {
                if (is.numeric(data[[var]])) {
                  list(tags$p(
                    paste0("Excluded less than: ", round(filters[[sel]]()[1], 2))
                  ),
                  tags$p(
                    paste0("Excluded more than: ", round(filters[[sel]]()[2], 2))
                  ))
                } else {
                  i <- match(filters[[sel]](), levels(data[[var]]))
                  lapply(levels(data[[var]])[-c(i[1]:i[2])], tags$p)
                }
              }
            } else {
              lapply(levels(data[[var]])[levels(data[[var]]) %notin% filters[[sel]]()], tags$p)
            }
          )
        )
      }, ignoreInit = TRUE,
      ignoreNULL = FALSE)
      
      return(renderInfoBox({
          infoBox(
            title,
          react(),
          icon = icon("list"),
          color = "red"
          )
        })
      )
    }
    
    output$ageFiltered <- filterBox(data, "age", "ageSelect", "Age")
    output$genderFiltered <- filterBox(data, "gender", "genderSelect", "Gender")
    output$empFiltered <- filterBox(data, "emp", "empSelect", "Employment")
    output$hhFiltered <- filterBox(data, "num_HH", "hhSelect", "Number in Household", "hhEnabled")
    output$paredFiltered <- filterBox(data, "pared", "paredSelect", "Parent's Education")
    output$imparFiltered <- filterBox(data, "impar", "imparSelect", "Parent Immigration")
    output$earnFiltered <- filterBox(data, "earnhrdcl", "earnSelect", "Earnings")
    output$lawFiltered <- filterBox(data, "learnatwork", "lawSelect", "Learn At Work", "lawEnabled")
    output$rtlFiltered <- filterBox(data, "readytolearn", "rtlSelect", "Ready To Learn", "rtlEnabled")
   
    observe({
      
      countryOutcomes <- filters$filteredData %>% 
        group_by(code) %>% 
        summarize(num_score = mean(num_score), lit_score = mean(lit_score)) %>% 
        inner_join(key)
      
      map <- sp::merge(map, countryOutcomes, by="country")
      map <- subset(map, code %in% filters$filteredData$code)
      
      mapColor <- brewer.pal(9, "Oranges")
      pal <- colorRampPalette(mapColor)
      
      output$outcomeHeading <- renderText(outcome[[filters$outcomeSelect()]])
      
      outcomePal <- colorNumeric(
        palette = pal(12),
        domain = countryOutcomes[[filters$outcomeSelect()]]
      )
      
      if (length(map$country) > 0) {
        info <- HTML(
          paste('<p><strong>Countries:</strong>', length(unique(filters$filteredData$country)), 
                '</p><p><strong>N:</strong>', nrow(filters$filteredData), '</p>')
        )
        
        map$popup <- with(map,
                        paste0(
                          "<strong>", map$piaac_name, "</strong><br>",
                          "Mean ", outcome[[filters$outcomeSelect()]], " Score: ", round(map[[filters$outcomeSelect()]], 2)
                        )
        )
        
        leafletProxy("dataMap", 
                data = map) %>% 
          clearControls() %>% clearPopups() %>%
          addPolygons(weight = 1, opacity = 1, fillOpacity = .7, color = "#000000",
                      fillColor = outcomePal(countryOutcomes[[filters$outcomeSelect()]]),
                      popup = ~popup
          ) %>% 
          addLegend_decreasing("bottomright", pal = outcomePal, 
                    values = countryOutcomes[[filters$outcomeSelect()]],
                    title = paste("Mean", outcome[[filters$outcomeSelect()]], "Score"),
                    opacity = 1,
                    decreasing = TRUE
          ) %>% 
          addControl(info, position = "topright")
      } else {
          leafletProxy("dataMap") %>% 
            clearControls() %>% clearPopups() %>% clearShapes()
        }
      
    })
  })
}