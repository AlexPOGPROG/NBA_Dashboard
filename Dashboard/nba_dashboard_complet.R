library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(viridis)
library(shinythemes)
library(shinyWidgets)

# Dossier des fichiers CSV
base_path <- "data"

# Liste des saisons disponibles
all_seasons <- c(
  "1983-84", "1984-85", "1985-86", "1986-87", "1987-88", "1988-89",
  "1989-90", "1990-91", "1991-92", "1992-93", "1993-94", "1994-95",
  "1995-96", "1996-97", "1997-98", "1998-99", "1999-00", "2000-01",
  "2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2006-07",
  "2007-08", "2008-09", "2009-10", "2010-11", "2011-12", "2012-13",
  "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19",
  "2019-20", "2020-21", "2021-22", "2022-23", "2023-24"
)
seasons_with_shots <- all_seasons[which(all_seasons == "2013-14"):length(all_seasons)]

# UI
ui <- navbarPage(
  title = "NBA Dashboard",
  theme = shinytheme("flatly"),

  tabPanel("Accueil",
           fluidPage(
             titlePanel("🏀 Analyse NBA : Impact des tirs à 3 points"),
             br(),
             p("Ce tableau de bord interactif permet d'explorer l'impact des tirs à 3 points sur la performance des équipes NBA. Vous pouvez sélectionner une saison, une équipe ou un joueur, et visualiser les statistiques de tir, les cartes de chaleur, les bilans domicile/extérieur, etc."),
             br(),
             downloadButton("download_csvs", "📥 Télécharger les CSV")
           )
  ),

  tabPanel("Visualisations",
    sidebarLayout(
      sidebarPanel(
        pickerInput("season", "Saison", choices = rev(all_seasons), selected = "2023-24"),
        radioButtons("selection_type", "Vue par :", choices = c("Player", "Team")),
        conditionalPanel(
          condition = "input.selection_type == 'Player'",
          uiOutput("player_selector")
        ),
        conditionalPanel(
          condition = "input.selection_type == 'Team'",
          uiOutput("team_selector")
        )
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Taille par Poste", plotOutput("height_position_plot")),
          tabPanel("Carte des Tirs", plotOutput("shot_chart")),
          tabPanel("Victoires/Défaites", plotOutput("team_wins")),
          tabPanel("Domicile vs Extérieur", plotOutput("home_vs_away")),
          tabPanel("Tirs vs Victoire", plotOutput("shot_position_vs_victory")),
          tabPanel("Heatmap", plotOutput("shot_heatmap_victory"))
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Téléchargement de l'archive CSV (tous les fichiers depuis base_path)
  output$download_csvs <- downloadHandler(
    filename = function() {
      paste("nba_data_archive", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      tmp <- tempfile()
      zip::zipr(zipfile = file, files = list.files(base_path, full.names = TRUE))
    }
  )

  
  # Exemple : chargement statique de données biographiques
  player_bios <- read_csv(file.path(base_path, "nba_player_bios.csv")) %>%
    mutate(
      height = sapply(height, function(h) {
        if (is.na(h)) return(NA)
        parts <- str_split(h, "-", simplify = TRUE)
        if (ncol(parts) == 2) {
          feet <- as.numeric(parts[, 1])
          inches <- as.numeric(parts[, 2])
          return(round((feet * 12 + inches) * 2.54, 1))
        }
        return(NA)
      }),
      weight = round(weight * 0.453592, 1)
    )

  # Chargement des fichiers par saison
  season_game_logs <- reactive({
    req(input$season)
    read_csv(file.path(base_path, paste0("nba_game_logs", input$season, ".csv")))
  })

  season_shot_data <- reactive({
    req(input$season %in% seasons_with_shots)
    read_csv(file.path(base_path, paste0("nba_player_shotchart", input$season, ".csv")))
  })

  output$player_selector <- renderUI({
    req(input$season %in% seasons_with_shots)
    data <- season_shot_data()
    selectInput("selected_player", "Choisissez un joueur :", choices = sort(unique(data$PLAYER_NAME)))
  })

  output$team_selector <- renderUI({
    data <- season_game_logs()
    selectInput("selected_team", "Choisissez une équipe :", choices = sort(unique(data$TEAM_NAME)))
  })

  selected_player_data <- reactive({
    req(input$selection_type == "Player", input$season %in% seasons_with_shots)
    season_shot_data() %>%
      filter(PLAYER_NAME == input$selected_player, !is.na(LOC_X), !is.na(LOC_Y))
  })

  selected_team_data <- reactive({
    req(input$selection_type == "Team")
    season_game_logs() %>% filter(TEAM_NAME == input$selected_team)
  })

  output$height_position_plot <- renderPlot({
    data <- player_bios %>% filter(!is.na(height), !is.na(position))
    ggplot(data, aes(x = position, y = height, fill = position)) +
      geom_violin(trim = FALSE, alpha = 0.6) +
      geom_boxplot(width = 0.1, fill = "white") +
      theme_minimal() +
      labs(title = "Taille des joueurs par poste", x = "Poste", y = "Taille (cm)")
  })

  output$shot_chart <- renderPlot({
    data <- selected_player_data()
    if (nrow(data) == 0) return(NULL)
    ggplot(data, aes(x = LOC_X, y = LOC_Y)) +
      geom_point(aes(color = EVENT_TYPE), alpha = 0.6) +
      scale_color_manual(values = c("Made Shot" = "green", "Missed Shot" = "red")) +
      coord_fixed() +
      theme_minimal() +
      ggtitle(paste("Shot Chart -", input$selected_player))
  })

  output$team_wins <- renderPlot({
    data <- selected_team_data()
    wins_data <- data %>% group_by(WL) %>% summarise(count = n())
    ggplot(wins_data, aes(x = WL, y = count, fill = WL)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("W" = "green", "L" = "red")) +
      theme_minimal() +
      ggtitle(paste("Bilan de", input$selected_team))
  })

  output$home_vs_away <- renderPlot({
    data <- selected_team_data() %>% mutate(HomeAway = ifelse(str_detect(MATCHUP, "@"), "Away", "Home"))
    ggplot(data, aes(x = GAME_DATE, y = PTS, color = WL)) +
      geom_point(size = 3) +
      facet_wrap(~ HomeAway, scales = "free_x") +
      scale_color_manual(values = c("W" = "green", "L" = "red")) +
      theme_minimal() +
      ggtitle(paste("Home vs Away -", input$selected_team))
  })

  output$shot_position_vs_victory <- renderPlot({
    req(input$season %in% seasons_with_shots)
    team_games <- selected_team_data() %>% select(GAME_ID, WL)
    team_shots <- season_shot_data() %>%
      filter(GAME_ID %in% team_games$GAME_ID, !is.na(LOC_X), !is.na(LOC_Y), !is.na(SHOT_TYPE), !is.na(SHOT_ZONE_BASIC)) %>%
      left_join(team_games, by = "GAME_ID") %>%
      filter(!is.na(WL))
    validate(need(nrow(team_shots) > 0, "Pas de tirs valides pour cette équipe"))

    summary <- team_shots %>% group_by(SHOT_TYPE, SHOT_ZONE_BASIC, WL) %>% summarise(shot_count = n(), .groups = "drop")
    ggplot(summary, aes(x = SHOT_ZONE_BASIC, y = shot_count, fill = SHOT_TYPE)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~ WL) +
      theme_minimal() +
      labs(title = paste("Tirs vs Victoire -", input$selected_team), x = "Zone de tir", y = "Nombre de tirs") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$shot_heatmap_victory <- renderPlot({
    req(input$season %in% seasons_with_shots)
    team_games <- selected_team_data() %>% select(GAME_ID, WL)
    team_shots <- season_shot_data() %>%
      filter(GAME_ID %in% team_games$GAME_ID, !is.na(LOC_X), !is.na(LOC_Y)) %>%
      left_join(team_games, by = "GAME_ID") %>%
      filter(!is.na(WL))
    validate(need(nrow(team_shots) > 0, "Pas de tirs valides pour cette équipe"))

    ggplot(team_shots, aes(x = LOC_X, y = LOC_Y)) +
      stat_density2d(aes(fill = ..level..), geom = "polygon", contour = TRUE, alpha = 0.8) +
      scale_fill_viridis_c(option = "D") +
      coord_fixed() +
      facet_wrap(~ WL) +
      theme_minimal() +
      labs(title = paste("Heatmap des tirs -", input$selected_team), x = "Court X", y = "Court Y", fill = "Densité") +
      annotate("path",
               x = 19.05 * cos(seq(0, 2 * pi, length.out = 100)),
               y = 19.05 * sin(seq(0, 2 * pi, length.out = 100)),
               color = "orange", size = 1.2)
  })

}

shinyApp(ui = ui, server = server)
