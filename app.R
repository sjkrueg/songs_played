
# import libraries
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(viridis)
library(ggrepel)
library(datetime)
library(lubridate)
library(tidyr)
library(purrr)
library(rlang)

library(markdown)
library(cowplot)


# Create here path for relative directory
# Set here directory
here::i_am("app.R") 
library(here)
path <- here()
data_path <- here("data")

# Import data
cols <- c("date_time", "artist", "song", "album")
cols_t <- c("cccc")

alexa_songs <- list.files(path = data_path, pattern = "^Songs_played", full.names = T) %>% 
  map(~read_csv(., col_names=cols, col_types=cols_t), as.data.frame) %>% 
  list_rbind

# Copy for raw data export
# raw_data <- alexa_songs[base::sample(nrow(alexa_songs), 30), ]
# sample_n(songs_selected(), 30), file, row.names = FALSE
# raw_data <- slice_sample(alexa_songs, 30)
raw_data <- alexa_songs

# Clean data
alexa_songs$date_time <- strptime(alexa_songs$date_time, format = "%B %e, %Y at %I:%M%p")

# Force Eastern Time, may not be necessary
alexa_songs$date_time <- as.POSIXct(alexa_songs$date_time)
alexa_songs$date_time <- force_tz(alexa_songs$date_time, tzone = "America/New_York")
alexa_songs <- alexa_songs[order(alexa_songs$date_time),] 
alexa_songs <- separate(data = alexa_songs, col = artist, into = c("main_artist", "featuring"), sep = " \\[feat. | \\(feat. ")

# Clean artist field and create featuring field (Does not split duets)
alexa_songs$featuring <- gsub('\\]', '', alexa_songs$featuring)
alexa_songs$featuring <- gsub('\\)', '', alexa_songs$featuring)

# Clean album field (special consideration for lynyrd skynyrd album)
alexa_songs$album <- gsub("\\(Pronounced \\'Leh\\-\\'Nérd \\'Skin\\-\\'Nérd\\)", 
                          "Pronounced \\'Leh\\-\\'Nérd \\'Skin\\-\\'Nérd", 
                          alexa_songs$album)
alexa_songs$album <- trimws(sub("\\[.*|\\(.*", "", alexa_songs$album))

# Clean song field to filter out explicit, clean, etc. tags
alexa_songs$song <- trimws(sub("\\[Exp.*|\\[Cle.*|\\(Alb.*|\\(Remas.*|\\(Main.*|\\(Live.*|\\(Orig.*|\\(Amazon.*|\\(From.*|\\(LP.*|\\(\\d{4}.*", 
                               "", 
                               alexa_songs$song))

# Create min and max date values for filter selection
min_date <- min(as_date(alexa_songs$date_time))
max_date <- max(as_date(alexa_songs$date_time))

# Add additional time fields so calculations only need to be made once
alexa_songs <- mutate(alexa_songs, 
                      Date = date(date_time),
                      Week_Day = wday(date_time, label = TRUE),
                      Week = epiweek(date_time),
                      Month = month(date_time, label = TRUE),
                      Year = year(date_time),
                      Time = as.time(strftime(date_time, format = "%H:%M")),
                      id = row_number())

# Keep only songs that were not skipped (if two songs have the same time-stamp, the one with a lower id was skipped)
alexa_songs <- alexa_songs %>% 
  group_by(date_time) %>% 
  top_n(1, id) %>% 
  ungroup()

# id column no longer needed
alexa_songs$id <- NULL

# radio selection buttons in ui for time grouping values
time_group_pairs <- c("Day" = "Date",
                      "Week" = "Week",
                      "Month" = "Month",
                      "Day of Week" = "Week_Day")

##########
# SERVER #
##########

# Generic line initiating the SERVER 

server <- shinyServer(function(input, output) {
  
  #############
  # Reactives #
  #############
  
  # define any reactive elements of the app
  
  # Creates a date filtered version of the alexa_songs dataset
  songs_selected <- reactive({ 
    req(input$date_range)
    date_interval <- interval(force_tz(as_datetime(min(input$date_range)), tzone = "America/New_York"), 
             force_tz(as_datetime(max(input$date_range) + 1), tzone = "America/New_York"))
    alexa_songs[alexa_songs$date_time %within% date_interval,] })
  
  # Creates a list of the top ten most played songs in sons_selected(), used in top_plot on tab1
  top_songs <- reactive({
    # Create tribble of top 10 songs played in songs_selected
    top_songs <- songs_selected() %>%
      count(main_artist, song, sort = T) %>%
      top_n(10) %>%
      unite(art_song, main_artist, song, sep = " - ", remove = F) %>%
      head(10)
    
    # Add newline character to field used for labelling for two line labels
    top_songs$art_song <- gsub(" \\[feat.", "\nFeat.", top_songs$art_song)
    top_songs$art_song <- gsub('\\]', '', top_songs$art_song)
    top_songs$art_song <- gsub('\\[Explicit', '\\[Explicit\\]', top_songs$art_song)
    
    # Return top_songs for use in app
    top_songs
  })
  
  # Creates a df to be displayed underneath the bump chart on tab2
  bump_rank <- reactive({ 
    req(input$y_axis)

    art_alb <- sym(input$y_axis)
    
    songs_selected() %>%
      group_by(!! art_alb) %>%
      mutate(sum_played = n_distinct(date_time)) %>%
      group_by(!! art_alb, Year, sum_played) %>%
      summarise(played_count = n_distinct(date_time)) %>% 
      group_by(Year) %>%
      mutate(rank = rank(-played_count, ties.method="min")) ##%>%
      ##subset(played_count > 1 & rank <= 5)
    
  })
  
  rank_plot_options <- reactive({
    req(input$y_axis)
    
    # Convert input to symbol for dplyr
    g_by <- input$y_axis
    g_by <- sym(g_by)
    
    # Create tribble of rankings by number of songs played each Year
    songs_selected_rank <- songs_selected() %>%
      group_by(!! g_by, Year) %>%
      summarise(played_count = n_distinct(date_time)) %>%
      group_by(Year) %>%
      mutate(rank = rank(-played_count, ties.method="min")) 
    
    # Select only songs played more than once and have a ranking of 5 or better
    songs_selected_rank <- subset(songs_selected_rank, played_count > 1 & rank <= 5)
    
    if (input$y_axis == "main_artist") {
      ssq <- songs_selected_rank$main_artist
      title_text <- 'Top Five Artists Each Year'
    } else {
      ssq <- songs_selected_rank$album
      title_text <- 'Top Five Albums Each Year'
    }
    
    # Label song only first time it appears and final month
    song_first_appearance <- songs_selected_rank[!duplicated(ssq),]
    song_first_appearance <- song_first_appearance[which(song_first_appearance$Year < max(songs_selected_rank$Year)),]
    song_final_year <- songs_selected_rank[which(songs_selected_rank$Year == max(songs_selected_rank$Year)),]
    label_vect <- unique(songs_selected_rank$Year)
    
    # Build bump chart
    rank_plot <- ggplot(songs_selected_rank, aes(x=as.integer(Year), y=rank)) +
      geom_line(aes_(colour = g_by), 
                size = 1.5, 
                alpha = .7) +
      geom_point(shape = 21, 
                 stroke = 2,
                 size = 5, 
                 colour = "white",
                 aes_(fill=g_by)) + 
      geom_label_repel(data = song_first_appearance, 
                       aes_(label=g_by), 
                       size = 2.5, 
                       fontface = "bold", 
                       color = "#2f2f2f", 
                       nudge_x = -.075) + 
      geom_label_repel(data = song_final_year, 
                       aes_(x = quote(as.integer(Year)),
                            y = quote(rank), 
                            label = g_by),
                       size = 2.5, 
                       color = '#2f2f2f', 
                       fontface = "bold", 
                       nudge_x = .04) +
      scale_y_reverse(lim = c(5,1), 
                      breaks = scales::pretty_breaks(n = 5)) +
      scale_x_continuous(limits = c(min(as.integer(songs_selected_rank$Year)), max(as.integer(songs_selected_rank$Year)) + .2),
                         expand = c(.05, .05),
                         breaks = as.integer(min(label_vect)):as.integer(max(label_vect)),
                         labels = sort(label_vect)) +
      ggtitle(title_text) +
      labs(x = "Year", 
           y = "Rank") +
      theme_minimal() +
      theme_bw() +
      theme(panel.background = element_rect(fill = "#ffffff"),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(15, 0, 10, 0, "pt")),
            legend.title = element_blank(),
            axis.text = element_text(size = 10), 
            axis.title = element_text(size = 14), 
            panel.border = element_blank(), 
            legend.position = "none", 
            panel.grid.minor.x = element_blank(), 
            panel.grid.minor.y = element_blank(),
            axis.ticks.x=element_blank(), 
            axis.ticks.y=element_blank(), 
            panel.grid.major.y = element_blank(), 
            panel.grid.major.x = element_line(color = "grey80", linetype = "56"))
    
    rank_plot
    
  })
  
  freq_plot_options <- reactive({ 
    req(input$time_type)
    xvar_name <- names(time_group_pairs)[time_group_pairs == input$time_type]
    
    # establish column grouping based on input
    if (input$time_type %in% c('Date', 'Week')) {
      fill_col <- 'Week_Day'
      fill_guide <- 'Week Day'
      group_col <- 'Date'
      scale_x <- scale_x_continuous(expand = c(0.02, 0))
    } else if (input$time_type == 'Week_Day') {
      fill_col <- 'as.integer(Time)'
      fill_guide <- 'Time of Day'
      group_col <- 'as.integer(Time)'
      scale_x <- NULL
    } else {
      fill_col <- 'Week_Day'
      fill_guide <- 'Week Day'
      group_col <- NULL
      scale_x <- NULL
    } 
    
    # establish bar fill and facet (y/n)
    if (input$time_type == 'Week_Day') {
      bar <- geom_bar()
      scale <- scale_fill_viridis("Time of Day",
                                 option = "magma",
                                 breaks = c(0, 21600, 43200, 64800, 86340),
                                 labels = c('0', '6', '12', '18', '24'),
                                 direction = -1)
      facet <- NULL
    } else {
      bar <- geom_bar()
      scale <- scale_fill_viridis(fill_guide, 
                                  discrete=TRUE, 
                                  direction = -1,
                                  guide = guide_legend(#nrow = 1, 
                                                       title.position = "top", 
                                                       label.position = "right"))
      facet <- facet_grid(Year ~ .) 
    }
    
    # Convert input to symbol for dplyr
    g_by <- input$time_type
    g_by <- sym(g_by)
    
    # establish input_field
    if (input$time_type == 'Date') {
      input_field <- 'yday(date_time)'
      bar <- geom_bar(color="grey65")
      # Plot Data for bplot
      plot_data <- songs_selected() %>%
        group_by(x = yday(date_time), Year) %>%
        summarise(played_count = n_distinct(date_time))
    } else {
      input_field <- input$time_type
      # Plot Data for bplot
      plot_data <- songs_selected() %>%
        group_by(x = !! g_by, Year) %>%
        summarise(played_count = n_distinct(date_time))
    }
    
    tplot <- ggplot(songs_selected(), aes_string(input_field, group = group_col, fill = fill_col)) + 
      bar +
      geom_hline(yintercept = 0, color = "grey40", size = .7) + 
      scale + 
      scale_x + 
      theme_minimal() +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(15, 0, 10, 0, "pt")), 
            axis.ticks = element_blank(),
            axis.title.y = element_blank(), 
            axis.title.x = element_blank(), 
            axis.text = element_text(size=10), 
            panel.grid.major.x = element_blank(), 
            panel.grid.major.y = element_line(color = "grey80", linetype = '56'),
            panel.grid.minor.x = element_blank(), 
            panel.grid.minor.y = element_blank(),
            strip.text.y = element_text(size = 12)) + 
      labs(title = paste('Number of Songs Played Each', xvar_name), 
           x = xvar_name) + 
      facet
    
    # Established which plots are displayed
    if (input$time_type == 'Week_Day') { 
      tplot # Only top plot (tplot) is displayed
    } else {
      # Both top plot (tplot) and bottom plot (bplot) are displayed using plot_grid
      bplot <- ggplot(plot_data, aes(x, y=played_count, color=as.factor(Year))) + 
        geom_hline(yintercept = 0, color = "grey40", size = .7) +
        geom_line(aes(group = Year), alpha = .7, size = 1) + 
        geom_point(size = 1.5, alpha = .8) +
        scale_color_viridis('Year', option = "cividis", discrete=TRUE) + 
        scale_x +
        theme_minimal() +
        theme(axis.ticks = element_blank(), 
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=10), 
          axis.text.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "grey80", linetype = '56'),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(), 
          legend.margin = margin(l = 22, unit = "pt"))
      plot_grid(tplot, bplot, ncol=1, axis = "lr", align="v", rel_heights = c(4,1)) 
      }
  })
  
  pan3_tribble <- reactive({ 
    req(input$y_axis)
    req(input$time_type)
    
    stime <- sym(input$time_type)
    art_alb <- sym(input$y_axis)
    
    songs_selected() %>%
      group_by(!! art_alb) %>% 
      mutate(sum_played = n_distinct(date_time)) %>%
      group_by(!! art_alb, !! stime, sum_played) %>% 
      summarise(played_count = n_distinct(date_time)) %>%
      spread(!! stime, played_count) %>%
      arrange(desc(sum_played))
    
  })
  
  #########################
  # Data load and cleanup #
  #########################
  
  # Output Data and plots
  
  # Bump Chart Tab Panel Outputs
  output$rank_plot <- renderPlot({ rank_plot_options() })
  output$bump_rank_table <- DT::renderDataTable({ DT::datatable(bump_rank() %>% 
                                                                  select(-rank) %>%
                                                                  spread(Year, played_count) %>%
                                                                  arrange(desc(sum_played)), 
                                                                options = list(pageLength = 5)) })
  
  # Top Songs Tab Panel Outputs
  output$top_plot <- renderPlot({ 
    ggplot(top_songs(), aes(reorder(art_song, n), n)) +
      geom_segment(aes(xend = reorder(art_song, n), yend = 0), color = 'grey40') +
      geom_point(size = 3) + 
      coord_flip() +
      labs(y = "Play Count", x= "", title = "Top 10 Songs in Timeframe") +
      scale_y_continuous(expand = c(0,0), limits = c(0, max(top_songs()$n) + 1)) +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(15, 0, 10, 0, "pt")),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(color = 'grey80', linetype = '56'),
            axis.ticks = element_blank(),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 14), 
            panel.border = element_blank(),
            panel.background = element_blank()) 
  })
  
  # Data frame for top songs
  output$table <- DT::renderDataTable({ DT::datatable(songs_selected()[2:6], options = list(pageLength = 5)) })

  # Frequency plot Tab Panel outputs
  output$freq_plot <- renderPlot({ freq_plot_options() })

  # add dataframes that show top artist and album by time grouping
  output$pan3_table <- DT::renderDataTable({ DT::datatable(pan3_tribble(), options = list(pageLength = 5, scrollX = TRUE)) })
  
  # Tab Panel 4 outputs
  # No options, text loaded from markdown document
  
  # Side Panel Download Outputs
  # Raw dataset
  output$download_raw <- downloadHandler(
    filename = function() {
      paste("songs_raw_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(sample_n(raw_data, 30), file, row.names = FALSE)
    })
  
  # Cleaned and filtered dataset
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste("songs_filtered_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(sample_n(songs_selected(), 30), file, row.names = FALSE)
    })
  
  # Close the server definition
})

##################
# User Interface #
##################

# Generic line initiating the UI
ui <- shinyUI(fluidPage(
  # Add a title
  titlePanel("Alexa Listening History"),
  
  # This creates a layout with a left sidebar and main section
  sidebarLayout(
    
    # beginning of sidebar section
    sidebarPanel(
      dateRangeInput("date_range", "Date Range:", 
                     start = min_date,
                     end = max_date,
                     min = min_date,
                     max = max_date),
      
      radioButtons("time_type", "Time Grouping:",
                   time_group_pairs),
      
      radioButtons("y_axis", "Rank by:",
                   c("Artist" = "main_artist",
                     "Album" = "album")),
      
      hr(),
      
      fluidRow(#cellWidths = c("50%", "50%"), #cellArgs = list(style="padding: 6px"), #splitLayout(
        div(style="display:inline-block",
            column(width = 6, downloadButton("download_raw", "Raw Data Sample"))),
        div(style="display:inline-block",
            column(width = 6, downloadButton("download_filtered", "Filtered Data Sample")))), #, style="float:right"
      width = 3
    ),
    
    # Main section
    mainPanel(tabsetPanel(
      tabPanel("Frequency Plot:", 
               plotOutput(outputId = 'freq_plot', height = "500px"),
               hr(),
               DT::dataTableOutput(outputId = 'pan3_table')),
      tabPanel("Year to Year Popularity:", 
               plotOutput(outputId = 'rank_plot', height = "500px"),
               hr(),
               DT::dataTableOutput(outputId ='bump_rank_table')),
      tabPanel("Top Songs PLayed:", 
               plotOutput(outputId = 'top_plot', height = "500px"),
               hr(),
               DT::dataTableOutput(outputId ='table')),
      tabPanel("About:", includeMarkdown("about.md"))
      )
      
      )
  )
  
  # Close the UI definition
))

##############
# Launch App #
##############

# Launch the app
shinyApp(ui = ui, server = server)
