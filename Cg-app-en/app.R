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
    right = a(
      href = "https://github.com/istaude/conservation-gardening-shiny-app",
      target = "_blank",
      "Github"
    ),
    left = a(
      href = "https://doi.org/10.1038/s41598-023-39432-8",
      target = "_blank",
      "Paper"
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
          to gardening and list their site requirements. We compare the resulting species lists
          with the assortments of several wild plant producers in Germany and provide information
          on the commercial availability of these species. For more information, 
          see: Munschek, M., Witt, R., Kaltofen, K. et al. Putting conservation gardening into 
          practice. Sci Rep 13, 12671 (2023).",
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
            "You can conveniently access and download plant lists for Conservation Gardening,
            for each German federal state, right here. There are several sorting options,
            including Red List status (endangerment), light, water, nutrient, and soil requirements, plant height, 
            biodiversity (indicating whether the plant attracts bees, 
            butterflies, birds, and/or mammals), flower color 
            and suitability for rooftop and balcony greening."
          )
        ),
        
        fluidRow(
          box(width = 12,
              status = "warning",
              title = "Federal state",
              selectInput(inputId = "dataset", label = NULL,
                          choices = c("Baden-Württemberg", 
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
              
              selectInput(inputId = "höhe", label = "Height (cm)", selectize = F,
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
            "Here, you can access a comprehensive overview of the conservation status 
            of vascular plants in each of the German federal states. Additionally, you
            have the option to download a master list, which comprises the consolidated
            Red Lists of all 16 federal states. This master list encompasses three 
            columns: State, Species Name, and Threat Level. It specifically includes species
            falling under the following threat categories: 0 (Extinct or Lost),
            1 (Critically Endangered), 2 (Endangered), 3 (Vulnerable), 
            G (Endangered - Unknown Extent), R (Rare), and V (Near Threatened).
            This database forms the foundation for Conservation Gardening 
            and has been integrated with data sourced from NaturaDB.
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
            a("Numerous Conservation Gardening plants are readily accessible through
              specialized wildflower nurseries like Gärtnerei Strickler, Hof Berg-Garten,
              Staudengärtnerei Spatz und Frank, Blauetikett Bornträger, and
              Staudengärtnerei Geißmeier. To locate your desired plant, simply
              input its name into the search field and check if any of the aforementioned
              nurseries carry it. Should you encounter any issues with the provided link
              in the URL column, you can manually copy and paste it into your internet
              browser or directly search for the plant on the nursery's website."),

            br(),
            br(),
            
            a("We'd like to emphasize the importance of private gardens opting 
            for certified Regio seeds. You can access a list of producers who have received 
            certification from the VWW (Verband der deutschen Wildsamen- und Wildpflanzenproduzenten e.V.)
            by clicking on the following link"),
            a("here.", href="https://www.natur-im-vww.de/bezugsquellen/"),
            a( "However, note that these certified producers often 
               primarily offer pre-packaged seed mixtures, primarily intended for 
               restoration purposes. Given that our Shiny App primarily caters to 
               private gardeners, we haven't included these companies in our database.")
            
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
            "We provide a download option for plants that are red-listed but not present
            in NaturaDB. Currently, we categorize these plants as not suitable for
            Conservation Gardening based on our existing knowledge and data. However,
            it's crucial for future endeavors to explore how these species can
            potentially be introduced and cultivated in gardens.",
            footer = downloadButton("downloadData_noncg", "Download non-CG plants")
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
            "Additionally, we provide a download option for plants that are suitable
            for Conservation Gardening but are presently not in production, at
            least not by the producers in our database. ",
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
                           "<br><i>Number endangered (0-3, G, R):</i>", germany$Gefährdet,
                           "<br><i>Percent endangered:</i>", label_percent()(round(germany$Prozentual,2)),
                           "<br><i>0 - Extinct or Lost: </i>", germany$`0`,
                           "<br><i>1 - Critically Endangered: </i>", germany$`1`,
                           "<br><i>2 - Endangered: </i>", germany$`2`,
                           "<br><i>3 - Vulnerable: </i>", germany$`3`,
                           "<br><i>G - Endangered - Unknown Extent: </i>", germany$G,
                           "<br><i>R - Rare: </i>", germany$R,
                           "<br><i>V - Near Threatened: </i>", germany$V,
                           "<br><i> Year of publication of Red List: </i>", germany$Publikationsjahr)
    
    
    pal <- colorNumeric(palette = "YlOrRd", domain = germany$Gefährdet)
    map <- leaflet(germany) %>% addTiles()
    
    map %>% 
      addPolygons(fillColor = ~pal(Gefährdet),
                  color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = ~label
      ) %>%
      addLegend("bottomright", pal = pal, values = ~Gefährdet,
                title = "Endangered taxa",
                opacity = 1
      ) %>% 
      setView(lat = 51.96, lng = 10, zoom = 6)
  })  
  
  
  
  
  
  
  # data explorer -----------------------------------------------------------
  
  d <- read_csv("./data-shiny/shiny_data.csv") %>% select(-`Freezing tolerance`)
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    
    switch(input$dataset,
           "Baden-Württemberg" = d %>% filter(State == "Baden-Württemberg"),
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
    
    if (input$farbe != "All") {
      data <- data %>% filter(`Flower color` %like% input$farbe)
    }
    
    
    if (input$höhe != "All") {
      data <- data %>% filter(h_binned == input$höhe) 
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