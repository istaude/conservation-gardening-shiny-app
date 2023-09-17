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
        "Pflanzenlisten",
        tabName = "pflanzenlisten",
        icon = icon("seedling")
      ),
      menuItem(
        "Rote Listen",
        tabName = "rotelisten",
        icon = icon("map")
      ),
      menuItem(
        "Produzenten",
        tabName = "produzenten",
        icon = icon("tractor")
      ),
      menuItem(
        "Wissenslücken",
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
          title = "Pflanzenlisten für Conservation Gardening",
          lead = 
      "Dies ist eine Shiny App, die Conservation Gardening Pflanzenlisten 
      für jedes Bundesland bereitstellt. Anhand der Roten Listen der einzelnen 
      Bundesländer identifizieren wir rückläufige und gefährdete Arten. Wir 
      integrieren diese Informationen mit Daten aus NaturaDB, einer 
      Gartenpflanzen-Datenbank, um Pflanzen auszuwählen, die für den Garten 
      geeignet sind und ihre Standortanforderungen aufzulisten. Wir vergleichen die resultierenden
      Artenlisten mit den Sortimenten von mehreren Wildpflanzenproduzenten in Deutschland und
      stellen Informationen zur kommerziellen Verfügbarkeit dieser Arten bereit. Für mehr Informationen
      siehe: Munschek, M., Witt, R., Kaltofen, K. et al. Putting conservation gardening into practice. 
      Sci Rep 13, 12671 (2023).",
          status = "warning",
          btnName = NULL,
          actionButton("switch_tab", 
                       "Finde Pflanzen", 
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
            "Pflanzenlisten für Conservation Gardening können hier für jedes Bundesland 
            aufgerufen und heruntergeladen werden. Sie können nach Status auf der Roten Liste (Gefährdung),
            Licht-, Wasser-, Nährstoff- und Bodenansprüchen, Höhe und Biodiversität 
            (d. h. ob die Pflanze von Bienen, Schmetterlingen, Vögeln und Säugetieren genutzt wird), 
            Blütenfarbe und Eignung der Pflanzen für Dach- und Balkonbegrünung sortieren."
          )
        ),
        
        fluidRow(
          box(width = 12,
              status = "warning",
              title = "Bundesland",
              selectInput(inputId = "dataset", label = NULL,
                          choices = c("Baden-Württemberg",
                                      "Bayern" ,
                                      "Berlin" ,
                                      "Brandenburg",
                                      "Bremen/Niedersachsen",
                                      "Hamburg" ,
                                      "Hessen" ,
                                      "Mecklenburg-Vorpommern" ,
                                      "Nordrhein-Westfalen" ,
                                      "Rheinland-Pfalz" ,
                                      "Saarland" ,
                                      "Sachsen",
                                      "Sachsen-Anhalt",
                                      "Schleswig-Holstein", 
                                      "Thüringen") 
              ),
              downloadButton("downloadData", "Download Pflanzenliste für Bundesland")
          )
        ),
        
        fluidRow(
          
          box(width = 2,
              status = "warning",
              title = "Auswahl",
              
              selectInput(inputId = "rlkat", label = "Gefährdung", selectize = F,
                          choices = c("Alle", "0", "1", "2", "3", "R", "G", "V")
              ),
              
              selectInput(inputId = "licht", label = "Licht", selectize = F,
                          c("Alle", "Sonne", "Halbschatten", "Schatten")
              ),
              
              selectInput(inputId = "wasser", label = "Wasser", selectize = F,
                          c("Alle", "trocken", "frisch", "feucht", "Wasserpflanze")
              ),
              
              selectInput(inputId = "nstoffe", label = "Nährstoffe", selectize = F,
                          c("Alle", "nährstoffarm", "normal", "nährstoffreich")
              ),
              
              selectInput(inputId = "ph", label = "PH-Wert", selectize = F,
                          c("Alle", "sauer", "basisch")
              ),
              
              selectInput(inputId = "boden",  label = "Boden", selectize = F,
                          c("Alle",  "durchlässig", "humus", "lehmig", "normal")
              ),
              
              selectInput(inputId = "höhe", label = "Höhe (cm)", selectize = F,
                          c("Alle", "0-40 cm", "40-60 cm", "60-100 cm", "100+ cm")
              ),
              
              selectInput(inputId = "farbe", label = "Blütenfarbe", selectize = F,
                          c(
                            "Alle",
                            "weiß"  ,
                            "blau"    ,
                            "violett"   ,
                            "rot"   ,
                            "gelb"    ,
                            "rosa"    ,
                            "pink"    ,
                            "grün"   ,
                            "silbergrau" ,
                            "lila"   ,
                            "braun",
                            "schwarz"  ,
                            "orange"
                          )
              ),
              
              selectInput("biodiv", label = "Biodiversität", selectize = F,
                          choices = c("Alle", "Bienen", "Schmetterlinge", "Vögel", "Säugetiere")
              ),
              
              selectInput("dach", label = "Dach und Balkon", selectize = F,
                          choices = c("Alle", "Dachbegrünung", "Balkon")
              )
              
              
              
          ),
          
          box(
            title = "Conservation Gardening Pflanzen",
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
            "Hier erhalten Sie einen Überblick über den Gefährdungsstatus von Gefäßpflanzen 
            in den einzelnen Bundesländern. Zudem haben Sie die Möglichkeit, eine Masterliste
            herunterzuladen, die aus den integrierten Roten Listen der 16 Bundesländer besteht.
            Diese Masterliste enthält drei Spalten: Bundesland, Artname und Gefährdungsgrad. 
            Sie umfasst ausschließlich Arten, die den Gefährdungskategorien 0 (Ausgestorben 
            oder verschollen), 1 (Vom Aussterben bedroht), 2 (Stark gefährdet), 3 (Gefährdet),
            G (Gefährdung unbekannten Ausmaßes), R (Extrem selten) und V (Vorwarnliste) angehören.
            ",
            footer = downloadButton("downloadData_rl", "Download Rote Listen der 16 Bundesländer")
          )
        ),
        
        fluidRow(
          box(
            title = "Bildliche Zusammenfassung der Roten Listen der deutschen Bundesländer",
            "Klicken Sie auf Bundesländer für weitere Informationen zum Gefährdungsstand.",
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
            closable = FALSE,
            width = 12,
            solidHeader = FALSE,
            collapsible = FALSE,
            a("Viele Conservation Gardening Pflanzen sind bereits verfügbar von 
            Gärtnereien, die auf Wildblumen spezialisert sind, z. B. 
            Gärtnerei Strickler, Hof Berg-Garten, Staudengärtnerei Spatz und Frank,
            Blauetikett Bornträger und Staudengärtnerei Geißmeier. 
            Geben Sie den Namen Ihrer Pflanze in die Suchmaske ein und schauen Sie, 
            ob sie von den oben genannten Betrieben produziert wird.
            Klicken Sie auf den Link in der URL-Spalte, um direkt zum Produzenten, weitergeleitet
            zu werden. Wenn diese Weiterleitung nicht funktioniert, kopieren Sie den Link in einen
            Internetbrowser, oder suchen Sie direkt auf der Seite des Produzenten nach der Pflanze."),
          
          
            br(),
            br(),
            
            a("Wir möchten betonen, dass es auch für Privatgärten wünschenswert wäre,
                zertifiziertes Regio-Saatgut zu verwenden. Eine Liste der Produzenten, die vom VWW 
                (Verband deutscher Wildsamen- und Wildpflanzenproduzenten e.V.) zertifiziert
                sind finden Sie"),
            a("hier.", href="https://www.natur-im-vww.de/bezugsquellen/"),
            a( "Oftmals bieten diese Produzenten jedoch keine 
                Einzelpflanzen an, sondern fertige Saatgutmischungen, meist für den Einsatz in der 
                Renaturierung. Da wir diese Shiny App vor allem für Privatgärtner/-innen gemacht haben, 
                haben wir diese Firmen nicht als Produzenten für Einzelpflanzen in unserer Datenbank. 
                Dies wird sich ändern, sobald im Rahmen der Renaturierungspolitik vermehrt Regio-Saatgut 
                produziert und für Kleinkunden zur Verfügung gestellt wird.")
            
          )
        ),
        
        
        fluidRow(
          box(
            title = "Bezugsquellen von Conservation Gardening Pflanzen",
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
            title = "Wissenslücken",
            closable = FALSE,
            width = 12,
            solidHeader = T,
            status = "warning",
            collapsible = FALSE,
            "Hier bieten wir einen Download für Pflanzen an, die auf den Roten Listen stehen, aber nicht in NaturaDB aufgeführt sind.
            Dies sind also Pflanzen, die aufgrund der bisherigen Datenlage nicht für Conservation Gardening geeignet sind. 
            In Zukunft wäre es wichtig herauszufinden, wie (wenn überhaupt) diese Arten in Gärten gepflanzt werden könnten.",
            footer = downloadButton("downloadData_noncg", "Download ungeeignete Pflanzen")
          )
        ),
        
        # species cg but not produced
        fluidRow(
          box(
            title = "Produktionslücken",
            closable = FALSE,
            width = 12,
            solidHeader = T,
            status = "warning",
            collapsible = FALSE,
            "Schließlich bieten wir noch einen Download für Pflanzen an, die zwar für Conservation Gardening geeignet sind, aber 
            momentan nicht produziert werden (zumindest nicht von den Produzenten in unserer Datenbank).",
            footer = downloadButton("downloadData_notproduced", "Download Produktionslücken")
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
    
    threat_summary <- read_excel("./data-shiny/red_lists_summary.xlsx")
    germany <- st_read("./data-shiny/germany.shp")
    
    germany <- full_join(germany, threat_summary, by = c("name" = "Bundesland"))
    germany$label <- paste(germany$name, 
                           "<br><i>Bewertete Taxa:</i>", germany$`Bewertete etablierte Taxa`,
                           "<br><i>Anzahl gefährdet (0-3, G, R):</i>", germany$Gefährdet,
                           "<br><i>Prozent gefährdet:</i>", label_percent()(round(germany$Prozentual,2)),
                           "<br><i>0 - Ausgestorben oder verschollen: </i>", germany$`0`,
                           "<br><i>1 - Vom Aussterben bedroht: </i>", germany$`1`,
                           "<br><i>2 - Stark gefährdet: </i>", germany$`2`,
                           "<br><i>3 - Gefährdet: </i>", germany$`3`,
                           "<br><i>G - Gefährdung unbekannten Ausmaßes: </i>", germany$G,
                           "<br><i>R - Extrem selten: </i>", germany$R,
                           "<br><i>V - Vorwarnliste: </i>", germany$V,
                           "<br><i>Publikationsjahr von Rote Liste: </i>", germany$Publikationsjahr)
    
    
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
                title = "Gefährdete Taxa",
                opacity = 1
      ) %>% 
      setView(lat = 51.96, lng = 10, zoom = 6)
  })  
  
  
  
  
  
  
  # data explorer -----------------------------------------------------------
  
  d <- read_csv("./data-shiny/shiny_data.csv") %>% select(-Frostverträglich)
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    
    switch(input$dataset,
           "Baden-Württemberg" = d %>% filter(Bundesland == "Baden-Württemberg"),
           "Bayern" = d %>% filter(Bundesland == "Bayern"),
           "Berlin" = d %>% filter(Bundesland == "Berlin"),
           "Brandenburg" = d %>% filter(Bundesland == "Brandenburg"),
           "Bremen/Niedersachsen" = d %>% filter(Bundesland == "Bremen/Niedersachsen"),
           "Hamburg" = d %>% filter(Bundesland == "Hamburg"),
           "Hessen" = d %>% filter(Bundesland == "Hessen"),
           "Mecklenburg-Vorpommern" = d %>% filter(Bundesland == "Mecklenburg-Vorpommern"),
           "Nordrhein-Westfalen" = d %>% filter(Bundesland == "Nordrhein-Westfalen"),
           "Rheinland-Pfalz" = d %>% filter(Bundesland == "Rheinland-Pfalz"),
           "Saarland" = d %>% filter(Bundesland == "Saarland"),
           "Sachsen" = d %>% filter(Bundesland == "Sachsen"),
           "Sachsen-Anhalt" = d %>% filter(Bundesland == "Sachsen-Anhalt"),
           "Schleswig-Holstein" = d %>% filter(Bundesland == "Schleswig-Holstein"),
           "Thüringen" = d %>% filter(Bundesland == "Thüringen")
    )
  })
  
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- datasetInput()
    
    if (input$rlkat != "Alle") {
      data <- data %>% filter(Gefährdung %like% input$rlkat)
    }
    
    if (input$licht != "Alle") {
      data <- data %>% filter(Licht %like% input$licht)
    }
    
    if (input$wasser != "Alle") {
      data <- data %>% filter(Wasser %like% input$wasser)
    }
    
    if (input$nstoffe != "Alle") {
      data <- data %>% filter(Nährstoffe %like% input$nstoffe)
    }
    
    if (input$ph != "Alle") {
      data <- data %>% filter( `PH-Wert` %like% input$ph)
    }
    
    if (input$boden != "Alle") {
      data <- data %>% filter(Boden %like% input$boden)
    }
    
    if (input$farbe != "Alle") {
      data <- data %>% filter(Blütenfarbe %like% input$farbe)
    }
    
    
    if (input$höhe != "Alle") {
      data <- data %>% filter(h_binned == input$höhe) 
    }
    
    if (input$biodiv != "Alle") {
      data <- data[ !is.na(data[, input$biodiv]),]
    }
    
    
    if (input$dach != "Alle") {
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
                 pageLength = 12,
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': 'white'});",
                   "}")) 
  ))
  
  
  
  
  
  
  # download cg species for each federal state  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("pflanzencg_", input$dataset, Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_excel_csv2(datasetInput(), file)
    }
  )
  
  
  
  
  
  
  # download species that are not amenable to cg  
  d_non_cg <- read_csv("./data-shiny/shiny_data_noncg.csv")
  
  output$downloadData_noncg <- downloadHandler(
    filename = function() {
      paste("planzen_nichtcg-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_excel_csv2(d_non_cg, file)
    }
  )
  
  
  
  
  
  
  # download species that are not amenable to cg  
  d_not_produced <- read_csv("./data-shiny/not-produced.csv")
  
  output$downloadData_notproduced <- downloadHandler(
    filename = function() {
      paste("cgplanzen_nichtproduziert-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_excel_csv2(d_not_produced, file)
    }
  )
  
  
  
  
  
  
  # download the master file that combines all red lists from the 16 fed states  
  d_rl <- read_csv("./data-shiny/shiny_data_rl.csv")
  
  output$downloadData_rl <- downloadHandler(
    filename = function() {
      paste("rotelisten_16bundesländer", Sys.Date(), ".csv", sep="")
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
        mutate(URL = ifelse(is.na(URL)==T, "nicht verfügbar",
                            paste0("<a href='", URL,"' target='_blank'>", URL,"</a>"))
        ) %>% 
        mutate(Produzent = ifelse(is.na(Produzent)==T, "nicht verfügbar",
                                  Produzent)
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