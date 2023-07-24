


library(shiny)
library(dbscan)
library(leaflet)
library(cluster)
library(ggplot2)
library(sf)
library(plotly)
library(readr)
library(dplyr)
library(DT)
library(rAmCharts)
library(colourpicker)
library(shinythemes)
# Chargement des données depuis le fichier CSV
data <- read.csv("data/ACLED-Western_Africa.csv")

# Clusterisation des données
clusters <- dbscan(data[, c("latitude", "longitude")], eps = 0.1, minPts = 5)
data$cluster <- as.factor(clusters$cluster)

# Chargement des données géographiques des pays au format .shp
shapefile_data <- st_read("data/Pays_WGS84.shp")

# UI de l'application 1
ui_app1 <- fluidPage(
  titlePanel("Visualisation des événements en Afrique de l'Ouest"),
  navbarPage(
    theme = "www/css/bootstrap.min.css",
    "ACLED Western Africa",
    # Premier onglet - Carte par pays
    tabPanel(
      "Carte du monde par évènements",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("event_filter", "Choisir un événement :", choices = unique(data$type), selected = unique(data$type)),
          br(),
          actionButton("filter_button", "Filtrer")
        ),
        mainPanel(
          leafletOutput("map")
        )
      )
    ),
    # Deuxième onglet - Filtrage des événements
    tabPanel(
      "Filtrage des événements",
      sidebarLayout(
        sidebarPanel(
          selectInput("country_filter", "Choisir un pays :", choices = unique(data$pays)),
          br(),
          selectInput("event_type_filter", "Choisir un type d'événement :", choices = unique(data$type)),
          br(),
          sliderInput("year_filter", "Choisir une année :", min = min(data$annee), max = max(data$annee), value = c(min(data$annee), max(data$annee)))
        ),
        mainPanel(
          fluidRow(
            column(
              width = 6,
              h3("Boxplot par événement"),
              plotOutput("boxplot")
            ),
            column(
              width = 6,
              h3("Carte filtrée"),
              leafletOutput("filtered_map")
            )
          )
        )
      )
    )
  )
)

# Server de l'application 1
server_app1 <- function(input, output, session) {
  
  # Carte par pays
  output$map <- renderLeaflet({
    filtered <- subset(data, type %in% input$event_filter)
    
    leaflet(filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~cluster,
        opacity = 0.8,
        fillOpacity = 0.8,
        label = ~paste("Pays :", pays, "<br>Type :", type, "<br>Année :", annee),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # Filtrage des événements
  filteredData <- reactive({
    subset(data, pays == input$country_filter & type == input$event_type_filter & annee >= input$year_filter[1] & annee <= input$year_filter[2])
  })
  
  # Carte filtrée
  output$filtered_map <- renderLeaflet({
    filtered <- filteredData()
    selected_country <- input$country_filter
    filtered_shapefile <- subset(shapefile_data, NOM == selected_country)
    
    leaflet(filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~cluster,
        opacity = 0.8,
        fillOpacity = 0.8,
        label = ~paste("Pays :", pays, "<br>Type :", type, "<br>Année :", annee),
        clusterOptions = markerClusterOptions()
      ) %>%
      addPolygons(
        data = filtered_shapefile,
        fillColor = "blue",
        color = "black",
        weight = 2,
        fillOpacity = 0.4,
        layerId = ~NOM
      )
  })
  
  # Boxplot par événement du pays sélectionné
  output$boxplot <- renderPlot({
    filtered <- filteredData()
    
    ggplot(filtered, aes(x = type, y = annee, fill = pays)) +
      geom_boxplot() +
      labs(title = paste("Boxplot par événement -", input$country_filter), x = "Type d'événement", y = "Année") +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.margin = margin(50, 10, 10, 10)
      )
  })
}

# UI de l'application 2
ui_app2 <- fluidPage(
  theme = "www/css/bootstrap.min.css",
  titlePanel("ACLED Western Africa"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country1", "Choisir un ou plusieurs pays :", choices = unique(data$pays), multiple = TRUE, selected = unique(data$pays)[1]),
      br(),
      selectInput("eventType1", "Choisir un type d'événement :", choices = unique(data$type), multiple = TRUE, selected = unique(data$type)),
      br(),
      actionButton("reset1", "Réinitialiser la carte")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Carte", leafletOutput("map1")),
        tabPanel("Statistiques", tableOutput("countryStats"), plotlyOutput("comparePlot"))
      )
    )
  )
)

# Server de l'application 2
server_app2 <- function(input, output, session) {
  
  # Filtrer les données en fonction du pays et du type d'événement sélectionnés
  filteredData1 <- reactive({
    if (is.null(input$country1) || is.null(input$eventType1) || length(input$eventType1) == 0) {
      NULL
    } else {
      filter(data, pays %in% input$country1, type %in% input$eventType1)
    }
  })
  
  # Charger un échantillon aléatoire de données
  observeEvent(input$reset1, {
    updateSelectInput(session, "country1", selected = unique(data$pays)[1])
    updateSelectInput(session, "eventType1", selected = unique(data$type))
  })
  
  # Afficher la carte avec les marqueurs
  output$map1 <- renderLeaflet({
    filtered <- filteredData1()
    
    if (is.null(filtered)) {
      return(NULL)
    }
    
    sample_size <- 1000  # Nombre d'observations à échantillonner
    data_sample <- filtered %>% sample_n(sample_size)
    
    leaflet(data_sample) %>%
      addTiles() %>%
      fitBounds(min(data_sample$longitude), min(data_sample$latitude), max(data_sample$longitude), max(data_sample$latitude)) %>%
      addCircleMarkers(
        lat = ~latitude,
        lng = ~longitude,
        radius = 5,
        color = "red",
        opacity = 0.8,
        fillOpacity = 0.8,
        clusterOptions = markerClusterOptions(),
        label = ~paste("Pays :", pays, "Type :", type)
      )
  })
  
  # Générer le tableau de statistiques du pays sélectionné
  output$countryStats <- renderTable({
    filtered <- filteredData1()
    
    if (is.null(filtered)) {
      return(NULL)
    }
    
    countryStats <- filtered %>%
      group_by(pays) %>%
      summarise(
        total_events = n(),
        earliest_year = min(annee),
        latest_year = max(annee)
      ) %>%
      left_join(
        filtered %>%
          count(pays, type) %>%
          group_by(pays) %>%
          filter(n == max(n)) %>%
          summarise(most_common_type = paste(type, collapse = ", ")),
        by = "pays"
      ) %>%
      left_join(
        filtered %>%
          count(pays, type) %>%
          group_by(pays) %>%
          filter(n == min(n)) %>%
          summarise(least_common_type = paste(type, collapse = ", ")),
        by = "pays"
      ) %>%
      mutate(percentage = paste(round(total_events / sum(total_events) * 100, 2), "%"))
    
    countryStats
  })
  
  # Générer le diagramme en bande comparatif des pays sélectionnés
  output$comparePlot <- renderPlotly({
    filtered <- filteredData1()
    
    if (is.null(filtered)) {
      return(NULL)
    }
    
    num_countries <- length(input$country1)
    
    countryStats <- filtered %>%
      filter(pays %in% input$country1) %>%
      group_by(pays, type) %>%
      summarise(count = n())
    
    if (num_countries == 1) {
      p <- ggplot(countryStats, aes(x = type, y = count)) +
        geom_bar(stat = "identity") +
        labs(title = paste("Comparaison des types d'événements pour", input$country1))
      
      ggplotly(p)
    } else if (num_countries > 1) {
      p <- ggplot(countryStats, aes(x = type, y = count, fill = pays)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Comparaison des types d'événements par pays")
      
      ggplotly(p)
    } else {
      return(NULL)
    }
  })
}


# UI
ui_app3 <- fluidPage(
  theme = "www/css/bootstrap.min.css",
  titlePanel("ACLED Western Africa"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country2", "Choisir un pays :", choices = unique(data$pays)),
      br(),
      selectInput("eventType2", "Choisir un type d'événement :", choices = unique(data$type), multiple = TRUE, selected = unique(data$type)),
      br(),
      actionButton("reset2", "Réinitialiser la carte")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Graphique", leafletOutput("map2"), dataTableOutput("typeTable")),
        tabPanel("Tableau", dataTableOutput("eventTable"))
      )
    )
  )
)

# Server
server_app3 <- function(input, output, session) {
  
  # Filtrer les données en fonction du pays et du type d'événement sélectionnés
  filteredData2 <- reactive({
    if (is.null(input$country2) || is.null(input$eventType2) || length(input$eventType2) == 0) {
      NULL
    } else {
      filter(data, pays == input$country2, type %in% input$eventType2)
    }
  })
  
  # Générer la carte du pays sélectionné et le tableau statistique des types d'événements
  output$map2 <- renderLeaflet({
    filtered <- filteredData2()
    
    if (is.null(filtered)) {
      return(NULL)  # Si aucun pays n'est sélectionné ou aucun type d'événement n'est choisi, ne pas afficher la carte
    }
    
    # Obtenir les types d'événements uniques
    eventTypes <- unique(filtered$type)
    
    # Définir la palette de couleurs
    colors <- colorFactor("Set1", domain = eventTypes)
    
    # Créer une carte Leaflet
    leaflet(data = filtered) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lat = ~latitude,
        lng = ~longitude,
        radius = 5,
        color = ~colors(type),
        fillOpacity = 0.8
      ) %>%
      addLegend(
        position = "bottomright",
        colors = colors(eventTypes),
        labels = eventTypes,
        title = "Types d'événements"
      ) %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  # Générer le tableau statistique des types d'événements
  output$typeTable <- renderDataTable({
    filtered <- filteredData2()
    
    if (is.null(filtered)) {
      return(NULL)  # Si aucun pays n'est sélectionné ou aucun type d'événement n'est choisi, ne pas afficher le tableau
    }
    
    # Calculer la part des types d'événements relativement au pays sélectionné
    typeSummary_country <- filtered %>%
      group_by(type) %>%
      summarise(count_country = n()) %>%
      mutate(percent_country = count_country / sum(count_country) * 100)
    
    # Calculer la part des types d'événements relativement à tous les pays
    typeSummary_all <- data %>%
      filter(type %in% unique(filtered$type)) %>%
      group_by(type) %>%
      summarise(count_all = n())
    
    # Joindre les deux résumés statistiques
    typeSummary <- left_join(typeSummary_country, typeSummary_all, by = "type") %>%
      mutate(percent_all = count_country / count_all * 100)
    
    datatable(typeSummary, options = list(pageLength = 10))
  })
  
  
  # Générer le tableau des événements filtrés
  output$eventTable <- renderDataTable({
    filtered <- filteredData2()
    
    if (is.null(filtered)) {
      return(NULL)  # Si aucun pays n'est sélectionné ou aucun type d'événement n'est choisi, ne pas afficher le tableau
    }
    
    datatable(filtered[, c("type", "date", "pays", "latitude", "longitude")], options = list(pageLength = 10))
  })
  
  # Réinitialiser la carte et les filtres
  observeEvent(input$reset2, {
    updateSelectInput(session, "country2", selected = NULL)
    updateSelectInput(session, "eventType2", selected = unique(data$type))
  })
  
  # Sélectionner toutes les modalités de la variable eventType au démarrage de l'application
  observeEvent(session, {
    updateSelectInput(session, "eventType2", selected = unique(data$type))
  }, once = TRUE)
}

# UI de la page de garde
ui_garde <-  fluidPage( 
  theme = "www/css/bootstrap.min.css",
  tags$div(
    style = "text-align: center; padding: 50px;",
    tags$h1("Bienvenue dans mon Application Shiny !", style = "color: #333;"),
    tags$p("Cette application est Presentée dans le cadre du cours de R.", style = "font-size: 18px; color: #666;"),
    tags$p("Sous l'encadement de ", tags$span("M Aboubacar HEMA", style = "font-weight: bold;"), "Analyst Reseach", style = "font-size: 16px; color: #888;"),
    tags$p("Cette application est la Partie 3 du projet de Statistique sur R qui nous a été soumi.", style = "font-size: 16px; color: #666;"),
    tags$button("EXPLORATION", class = "btn btn-primary", style = "font-size: 16px; margin-top: 20px;", onclick = "showApp()"),
    tags$p("© ", tags$span("NZIALI TCHAMOU Durel Valdes", style = "font-weight: bold;"), "ISE-1 Maths 2023", style = "font-size: 14px; color: #666; margin-top: 30px;")
  )

)

# Server de la page de garde
server_garde <- function(input, output, session) {

}


img_path <- "assets/Page_Garde_Rshiny_1.png"
img <- list(src = img_path, alt = "Image")

# UI de la page de garde
ui_4 <-  fluidPage( 

  imageOutput("image_display")

  
)

# Server de la page de garde
server_4 <- function(input, output, session) {
  output$image_display <- renderImage({
    img
  })
  
}


# UI principale
ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("ACLED Western Africa"),
  
  ui_garde,
  tabsetPanel(
    tabPanel("Accueil", ui_4),
    tabPanel("Section donnée", ui_app3),
    tabPanel("Section evenement", ui_app1),
    tabPanel("Section Pays", ui_app2)
  )  
  
)



# Server principal
server <- function(input, output, session) {

  
  server_app3(input, output, session)
  server_app1(input, output, session)
  server_app2(input, output, session)
  server_garde(input, output, session)
  server_4(input, output, session)
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)



