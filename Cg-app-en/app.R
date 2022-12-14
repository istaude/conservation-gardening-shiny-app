library(shiny)
library(rsconnect)
library(DT)
library(data.table)
library(tidyverse)
library(sf)
library(leaflet)
library(rnaturalearth)
library(readxl)
library(shinyWidgets)
library(bs4Dash)
library(shinyjs)
library(scales)




# front end ---------------------------------------------------------------
ui <- dashboardPage(
  
  header = dashboardHeader(status = "olive"),
  dark = NULL, 
  footer = dashboardFooter(
    left = a(
      href = "https://github.com/istaude/conservation-gardening-shiny-app",
      target = "_blank",
      "Github Repository"
    )
  ),
  
  sidebar = dashboardSidebar(
    
    skin = "light",
    status = "olive",
    elevation = 4,
    collapsed = F,
    minified = T,
    expandOnHover = TRUE,
    fixed = TRUE,
    id = NULL,
    customArea = NULL,
    
    sidebarMenu(
      id = "sidebarmenu",
      menuItem(
        "Home",
        tabName = "home",
        icon = icon("heart")
      ),
      menuItem(
        "Plant lists",
        tabName = "pflanzenlisten",
        icon = icon("seedling")
      ),
      menuItem(
        "Red Lists",
        tabName = "rotelisten",
        icon = icon("map")
      ),
      menuItem(
        "Producers",
        tabName = "produzenten",
        icon = icon("tractor")
      ),
      menuItem(
        "Knowledge gaps",
        tabName = "wissen",
        icon = icon("question")
      )
    )
  ),
  
  
  body = dashboardBody(
    useShinyjs(),
    
    
    # title page --------------------------------------------------------------
    tabItems(
      
      tabItem(  
        tabName = "home",
        bs4Jumbotron(
          title = "Plant lists for Conservation Gardening",
          lead = "This is a Shiny App that provides Conservation Gardening plant
          lists for each German federal state. Using each state's Red Lists, we identify declining
          and endangered species. We integrate this information with data from 
          NaturaDB, a garden plant database, to find plants that are amenable
          to gardening and list their site requirements.",
          status = "warning",
          btnName = NULL,
          actionButton("switch_tab", 
                       "Find Plants", 
                       status = "danger",
                       outline = TRUE, 
                       flat = TRUE, 
                       size = "lg")
        )
        
      ),
      
      
      
      
      
      
      # cg species --------------------------------------------------------------
      tabItem(  
        tabName = "pflanzenlisten",
        
        fluidRow(
          box(
            title = "Info box",
            closable = FALSE,
            width = 12,
            solidHeader = FALSE,
            collapsible = FALSE,
            "Plant lists for Conservation Gardening can be accessed and downloaded 
            here for each German federal state. You can sort by red list status 
            (endangerment), light, water, nutrient and soil requirements, height 
            and biodiversity (i.e., whether the plant is used by bees, butterflies, 
            birds and/or mammals), flower color, frost tolerance and suitability 
            of plants for roof and balcony greening."
          )
        ),
        
        fluidRow(
          box(width = 12,
              status = "warning",
              title = "Federal state",
              selectInput(inputId = "dataset", label = NULL,
                          choices = c("Baden-W??rttemberg", 
                                      "Bavaria" ,
                                      "Berlin" ,
                                      "Brandenburg",
                                      "Bremen/Lower Saxony" ,
                                      "Hamburg" ,
                                      "Hesse" ,
                                      "Mecklenburg-Western Pomerania" ,
                                      "North Rhine-Westphalia" ,
                                      "Rhineland-Palatinate" ,
                                      "Saarland" ,
                                      "Saxony",
                                      "Saxony-Anhalt",
                                      "Schleswig-Holstein", 
                                      "Thuringia") 
              ),
              downloadButton("downloadData", "Download plant list for state")
          )
        ),
        
        fluidRow(
          
          box(width = 2,
              status = "warning",
              title = "Selection",
              
              selectInput(inputId = "rlkat", label = "Endangerment", selectize = F,
                          choices = c("All", "0", "1", "2", "3", "R", "G", "V")
              ),
              
              selectInput(inputId = "licht", label = "Light", selectize = F,
                          c("All", "Sun", "Half-shade", "Shade")
              ),
              
              selectInput(inputId = "wasser", label = "Water", selectize = F,
                          c("All", "dry", "moist", "wet", "aquatic")
              ),
              
              selectInput(inputId = "nstoffe", label = "Nutrients", selectize = F,
                          c("All", "N-poor", "intermediate", "N-rich")
              ),
              
              selectInput(inputId = "ph", label = "PH-value", selectize = F,
                          c("All", "acidic", "alkaline")
              ),
              
              selectInput(inputId = "boden",  label = "Soil", selectize = F,
                          c("All",  "permeable", "humus", "loamy", "normal")
              ),
              
              selectInput(inputId = "frost", label = "Freezing tolerance", selectize = F,
                          c("All", paste("Climatic zone", c(1:9)))
              ),
              
              selectInput(inputId = "h??he", label = "Height (cm)", selectize = F,
                          c("All", "0-40 cm", "40-60 cm", "60-100 cm", "100+ cm")
              ),
              
              selectInput(inputId = "farbe", label = "Flower color", selectize = F,
                          c(
                            "All",
                            "white"  ,
                            "blue"    ,
                            "violet"   ,
                            "red"   ,
                            "yellow"    ,
                            "rose"    ,
                            "pink"    ,
                            "green"   ,
                            "silvergrey" ,
                            "purple"   ,
                            "brown",
                            "black"  ,
                            "orange"
                          )
              ),
              
              selectInput("biodiv", label = "Biodiversity", selectize = F,
                          choices = c("All", "Bees", "Butterflies", "Birds", "Mammals")
              ),
              
              selectInput("dach", label = "Roof and Balcony", selectize = F,
                          choices = c("All", "Roof", "Balcony")
              )
              
              
              
          ),
          
          box(
            title = "Conservation Gardening Plants",
            width = 10,
            solidHeader = T,
            status = "warning",
            closable = F,
            collapsible = F,
            DT::dataTableOutput("table")
          )
        )
        
      ),
      
      
      
      
      
      
      # red list overview -------------------------------------------------------
      tabItem(  
        tabName = "rotelisten",
        
        fluidRow(
          box(
            title = "Info box",
            closable = FALSE,
            width = 12,
            solidHeader = FALSE,
            collapsible = FALSE,
            "Here you can get an overview of the endangerment status of vascular 
            plants in each German federal states. You can also download a master 
            list (i.e., the synthesized Red Lists of the 16 federal states). 
            This master list has three columns: State, Species Name, Threat Level; 
            it includes only species with these threat categories: 0 (Extinct or Lost), 
            1 (Critically Endangered), 2 (Endangered), 3 (Vulnerable), 
            G (Endangered - Unknown Extent), R (Rare), V (Near Threatened).
            This database is the basis for Conservation Gardening 
            and was integrated with the data stream from NaturaDB.
            ",
            footer = downloadButton("downloadData_rl", "Download Red List synthesis")
          )
        ),
        
        fluidRow(
          box(
            title = "Visual summary of threat status",
            "Click on states for more information on threat status.",
            closable = TRUE,
            width = 12,
            status = "warning",
            solidHeader = T,
            collapsible = TRUE,
            leafletOutput("map", height="80vh"),
            maximizable = T
          )
        )
      ),
      
      
      
      
      
      
      # producers ---------------------------------------------------------------
      tabItem(
        tabName = "produzenten",
        
        fluidRow(
          box(
            title = "Info box",
            width = 12,
            solidHeader = FALSE,
            collapsible = FALSE,
            closable = FALSE,
            a("Many Conservation Gardening plants are already available from nurseries
              that specialize in wildflowers, such as G??rtnerei Strickler, Hof Berg-Garten,
              Staudeng??rtnerei Spatz und Frank, Blauetikett Borntr??ger, and 
              Staudeng??rtnerei Gei??meier. Enter the name of your plant in the search
              form and see if it is produced by the above producers. 
              Click on the link in the URL column to be redirected directly to the producer.
              If this does not work, copy the link into an Internet browser, 
              or search for the plant directly on the producer's site."),

            br(),
            br(),
            
            a("We would also like to emphasize that it would be desirable for private gardens
              to use certified Regio seeds. A list of producers certified by the VWW 
              (Verband der deutschen Wildsamen- und Wildpflanzenproduzenten e.V.) can be found"),
            a("here.", href="https://www.natur-im-vww.de/bezugsquellen/"),
            a( "Often, however, these producers do not offer individual plants but 
               ready-made seed mixtures, mostly for use in restoration. Since we 
               have made this Shiny App mainly for private gardeners, we do not have these
               companies in our database.")
            
          )
        ),
        
        
        fluidRow(
          box(
            title = "Where to buy Conservation Gardening plants",
            width = 12,
            solidHeader = T,
            status = "warning",
            closable = F,
            collapsible = F,
            DT::dataTableOutput("table_produzent")
          )
        )
        
      ),
      
      
      
      # knowledge gaps ----------------------------------------------------------
      tabItem(
        tabName = "wissen",
        
        # species non cg
        fluidRow(
          box(
            title = "Knowledge gaps",
            closable = FALSE,
            width = 12,
            solidHeader = T,
            status = "warning",
            collapsible = FALSE,
            "Here we offer a download for plants that are red-listed but not
            in NaturaDB. At this time, we are identifying these plants as not amenable
            to Conservation Gardening based on our current knowledge/data. 
            In the future, it would be important to find out how (if at all) 
            these species could be seeded/planted in gardens.",
            footer = downloadButton("downloadData_noncg", "Download non-cg plants")
          )
        ),
        
        # species cg but not produced
        fluidRow(
          box(
            title = "Production gaps",
            closable = FALSE,
            width = 12,
            solidHeader = T,
            status = "warning",
            collapsible = FALSE,
            "Finally, we offer a download for plants that are suitable for 
            Conservation Gardening but are not currently in production 
            (at least not by the producers in our database).",
            footer = downloadButton("downloadData_notproduced", "Download production gaps")
          )
        )
        
      )
      
      
    )
  )
)











# back end ----------------------------------------------------------------
server <- function(input, output, session) {
  
  
  
  # title page --------------------------------------------------------------
  observeEvent(input$switch_tab, {
    updateTabItems(session, "sidebarmenu", selected = "pflanzenlisten")
  })
  
  
  
  
  
  
  # red list map ------------------------------------------------------------
  output$map <- renderLeaflet({
    
    threat_summary <- read_csv("./data-shiny/red_lists_summary.csv")
    germany <- st_read("./data-shiny/germany.shp")
    

    
    germany <- full_join(germany, threat_summary, by = c("name" = "Bundesland"))
    germany$label <- paste(germany$Bundesland2, 
                           "<br><i>Assessed taxa:</i>", germany$`Bewertete etablierte Taxa`,
                           "<br><i>Number endangered (0-3, G, R):</i>", germany$Gef??hrdet,
                           "<br><i>Percent endangered:</i>", label_percent()(round(germany$Prozentual,2)),
                           "<br><i>0 - Extinct or Lost: </i>", germany$`0`,
                           "<br><i>1 - Critically Endangered: </i>", germany$`1`,
                           "<br><i>2 - Endangered: </i>", germany$`2`,
                           "<br><i>3 - Vulnerable: </i>", germany$`3`,
                           "<br><i>G - Endangered - Unknown Extent: </i>", germany$G,
                           "<br><i>R - Rare: </i>", germany$R,
                           "<br><i>V - Near Threatened: </i>", germany$V,
                           "<br><i> Year of publication of Red List: </i>", germany$Publikationsjahr)
    
    
    pal <- colorNumeric(palette = "YlOrRd", domain = germany$Gef??hrdet)
    map <- leaflet(germany) %>% addTiles()
    
    map %>% 
      addPolygons(fillColor = ~pal(Gef??hrdet),
                  color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = ~label
      ) %>%
      addLegend("bottomright", pal = pal, values = ~Gef??hrdet,
                title = "Endangered taxa",
                opacity = 1
      ) %>% 
      setView(lat = 51.96, lng = 10, zoom = 6)
  })  
  
  
  
  
  
  
  # data explorer -----------------------------------------------------------
  
  d <- read_csv("./data-shiny/shiny_data.csv")
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    
    switch(input$dataset,
           "Baden-W??rttemberg" = d %>% filter(State == "Baden-W??rttemberg"),
           "Bavaria" = d %>% filter(State == "Bavaria"),
           "Berlin" = d %>% filter(State == "Berlin"),
           "Brandenburg" = d %>% filter(State == "Brandenburg"),
           "Bremen/Lower Saxony" = d %>% filter(State == "Bremen/Lower Saxony"),
           "Hamburg" = d %>% filter(State == "Hamburg"),
           "Hesse" = d %>% filter(State == "Hesse"),
           "Mecklenburg-Western Pomerania" = d %>% filter(State == "Mecklenburg-Western Pomerania"),
           "North Rhine-Westphalia" = d %>% filter(State == "North Rhine-Westphalia"),
           "Rhineland-Palatinate" = d %>% filter(State == "Rhineland-Palatinate"),
           "Saarland" = d %>% filter(State == "Saarland"),
           "Saxony" = d %>% filter(State == "Saxony"),
           "Saxony-Anhalt" = d %>% filter(State == "Saxony-Anhalt"),
           "Schleswig-Holstein" = d %>% filter(State == "Schleswig-Holstein"),
           "Thuringia" = d %>% filter(State == "Thuringia")
    )
  })
  
  
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- datasetInput()
    
    if (input$rlkat != "All") {
      data <- data %>% filter(Endangerment %like% input$rlkat)
    }
    
    if (input$licht != "All") {
      data <- data %>% filter(Light %like% input$licht)
    }
    
    if (input$wasser != "All") {
      data <- data %>% filter(Water %like% input$wasser)
    }
    
    if (input$nstoffe != "All") {
      data <- data %>% filter(Nutrients %like% input$nstoffe)
    }
    
    if (input$ph != "All") {
      data <- data %>% filter( `PH-value` %like% input$ph)
    }
    
    if (input$boden != "All") {
      data <- data %>% filter(Soil %like% input$boden)
    }
    
    if (input$frost != "All") {
      data <- data %>% filter(`Freezing tolerance` %like% input$frost)
    }
    
    if (input$farbe != "All") {
      data <- data %>% filter(`Flower color` %like% input$farbe)
    }
    
    
    if (input$h??he != "All") {
      data <- data %>% filter(h_binned == input$h??he) 
    }
    
    if (input$biodiv != "All") {
      data <- data[ !is.na(data[, input$biodiv]),]
    }
    
    
    if (input$dach != "All") {
      data <- data[ !is.na(data[, input$dach]),]
    }
    
    
    data %>% 
      select(-h_binned)
    
  }, 
  
  
  rownames = FALSE, 
  
  options = list(bFilter=TRUE, bSort = "Name",
                 scroller = TRUE,
                 scrollX = TRUE,
                 autoWidth = TRUE,
                 dom = "ftp",
                 "sDom" = "rt",
                 pageLength = 9,
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': 'white'});",
                   "}")) 
  ))
  
  
  
  
  
  
  # download cg species for each federal state  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("plantscg_", input$dataset, Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_excel_csv2(datasetInput(), file)
    }
  )
  
  
  
  
  
  
  # download species that are not amenable to cg  
  d_non_cg <- read_csv("./data-shiny/shiny_data_noncg.csv")
  
  output$downloadData_noncg <- downloadHandler(
    filename = function() {
      paste("plants_notcg-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_excel_csv2(d_non_cg, file)
    }
  )
  
  
  
  
  
  
  # download species that are not amenable to cg  
  d_not_produced <- read_csv("./data-shiny/not-produced.csv")
  
  output$downloadData_notproduced <- downloadHandler(
    filename = function() {
      paste("cgplants-notproduced-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_excel_csv2(d_not_produced, file)
    }
  )
  
  
  
  
  
  
  # download the master file that combines all red lists from the 16 fed states  
  d_rl <- read_csv("./data-shiny/shiny_data_rl.csv")
  
  output$downloadData_rl <- downloadHandler(
    filename = function() {
      paste("redlist_16fedstates", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_excel_csv2(d_rl, file)
    }
  )
  
  
  
  
  
  
  # the producer data frame --------------------------------------------------
  d_producer <- read_csv("./data-shiny/seller_cg.csv")
  
  output$table_produzent <- renderDataTable(
    DT::datatable({
      data <- d_producer %>% 
        mutate(URL = ifelse(is.na(URL)==T, "not available",
                            paste0("<a href='", URL,"' target='_blank'>", URL,"</a>"))
        ) %>% 
        mutate(Producer = ifelse(is.na(Producer)==T, "not available",
                                  Producer)
        )
      data
    },
    escape = FALSE,
    rownames = FALSE, 
    options = list(bFilter=TRUE, bSort = "Name",
                   scroller = TRUE,
                   scrollX = TRUE,
                   autoWidth = F,
                   dom = "ftp",
                   "sDom" = "rt",
                   pageLength = 12,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'white'});",
                     "}"))
    )
  )  
  
  
  
}

shinyApp(ui, server)