# import libraries
library(shiny)
library("googlesheets")
library(dplyr)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(tidyr)
library(rlang)
library(here)


#Create here path for relative directory
path <- here()

#Load my sheets (temp commented out, using csv until testing done)
#my_sheets <- gs_ls()

#songs_played <- gs_title("Songs played")
#sp_sheets <- gs_ws_ls(songs_played)

#Import data
#cols <- c("date_time", "artist", "song", "album")

#alexa_songs <- songs_played %>%
#  gs_read(ws = sp_sheets[1], col_names = cols)

#Temp read csv for testing
alexa_songs <- read.csv(paste(path, "alexa_songs_temp.csv", sep = "/"), stringsAsFactors = FALSE, encoding = "UTF-8")
alexa_songs$X <- NULL

#Clean data
alexa_songs$date_time <- strptime(alexa_songs$date_time, format = "%B %e, %Y at %I:%M%p")
#Force Eastern Time, may not be necessary
alexa_songs$date_time <- as.POSIXct(alexa_songs$date_time)
alexa_songs$date_time <- force_tz(alexa_songs$date_time, tzone = "America/New_York")
alexa_songs <- separate(data = alexa_songs, col = artist, into = c("main_artist", "featuring"), sep = " \\[feat. ")

#Clean artist field and create featuring field (Does not split duets)
alexa_songs$featuring <- gsub('\\]', '', alexa_songs$featuring)

#Clean album field (special consideration for lynyrd skynyrd album)
#TO DO: force first letter caps, account for " - clean/explicit/..." cases
alexa_songs$album <- gsub("\\(Pronounced \\'Leh\\-\\'Nérd \\'Skin\\-\\'Nérd\\)", 
                          "Pronounced \\'Leh\\-\\'Nérd \\'Skin\\-\\'Nérd", 
                          alexa_songs$album)
alexa_songs$album <- trimws(sub("\\[.*|\\(.*", "", alexa_songs$album))

#create min and max date values for filter selection
min_date <- min(as_date(alexa_songs$date_time))
max_date <- max(as_date(alexa_songs$date_time))

#add additional time fields so calculations only need to be made once
alexa_songs <- mutate(alexa_songs, 
                      Date = date(date_time),
                      Week_Day = wday(date_time, label = TRUE),
                      Week = week(date_time),
                      Month = month(date_time, label = TRUE))

# radio selection buttons in ui for time grouping values
time_group_pairs <- c("Date" = "Date",
                      "Week Day" = "Week_Day",
                      "Week" = "Week",
                      "Month" = "Month")

##########
# SERVER #
##########

#generic line initiating the SERVER 

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
      unite(art_song, main_artist, song, sep = " - ", remove = F)
    
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
      group_by(!! art_alb, Month, sum_played) %>%
      summarise(played_count = n_distinct(date_time)) %>% 
      group_by(Month) %>%
      mutate(rank = rank(-played_count, ties.method="min")) ##%>%
      ##subset(played_count > 1 & rank <= 5)
    
  })
  
  main_plot_options <- reactive({
    req(input$y_axis)
    
    # Convert input to symbol for dplyr
    g_by <- input$y_axis
    g_by <- sym(g_by)
    
    # Create tribble of rankings by number of songs played each month
    songs_selected_rank <- songs_selected() %>%
      group_by(!! g_by, Month) %>%
      summarise(played_count = n_distinct(date_time)) %>%
      group_by(Month) %>%
      mutate(rank = rank(-played_count, ties.method="min")) 
    
    # Select only songs played more than once and have a ranking of 5 or better
    songs_selected_rank <- subset(songs_selected_rank, played_count > 1 & rank <= 5)
    
    if (input$y_axis == "main_artist") {
      ssq <- songs_selected_rank$main_artist
      title_text <- 'Top Five Artists Each Month'
    } else {
      ssq <- songs_selected_rank$album
      title_text <- 'Top Five Albums Each Month'
    }
    
    # Label song only first time it appears and final month
    song_first_appearance <- songs_selected_rank[!duplicated(ssq),]
    song_first_appearance <- song_first_appearance[which(song_first_appearance$Month < max(songs_selected_rank$Month)),]
    song_final_month <- songs_selected_rank[which(songs_selected_rank$Month == max(songs_selected_rank$Month)),]
    label_vect <- unique(songs_selected_rank$Month)
    
    # Build bump chart
    rank_plot <- ggplot(songs_selected_rank, aes(x=as.integer(Month), y=rank)) +
      geom_line(aes_(colour = g_by), 
                size = 1.5, 
                alpha = .7) +
      geom_point(shape = 21, 
                 stroke=2,
                 size=5, 
                 colour="white",
                 aes_(fill=g_by)) + 
      geom_label_repel(data = song_first_appearance, 
                       aes_(label=g_by), 
                       size=2.5, 
                       fontface = "bold", 
                       color='#2f2f2f', 
                       nudge_x = -.075) + 
      geom_label(data = song_final_month, 
                 aes_(x = quote(as.integer(Month)), 
                      y = quote(rank), 
                      label = g_by),
                 size = 2.5, 
                 color= '#2f2f2f', 
                 fontface = "bold", 
                 hjust=-.05) + 
      scale_y_reverse(lim=c(5,1), 
                      breaks = scales::pretty_breaks(n = 5)) +
      scale_x_continuous(limits = c(min(as.integer(songs_selected_rank$Month)), max(as.integer(songs_selected_rank$Month)) + .2),
                         expand = c(.05, .05), 
                         breaks = as.integer(min(label_vect)):as.integer(max(label_vect)), 
                         labels = as.character(label_vect)) +
      ggtitle(title_text) +
      xlab("Month") +
      ylab("Rank") +
      theme_minimal() +
      theme_bw() +
      theme(panel.background = element_rect(fill = '#ffffff'),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(15,0,10,0,"pt")),
            legend.title=element_blank(),
            axis.text = element_text(size=10), 
            axis.title=element_text(size=11), 
            panel.border = element_blank(), 
            legend.position='none', 
            panel.grid.minor.x = element_blank(), 
            panel.grid.minor.y = element_blank(),
            axis.ticks.x=element_blank(), 
            axis.ticks.y=element_blank(), 
            panel.grid.major.y = element_blank(), 
            panel.grid.major.x = element_line(linetype = 'dashed'))
    
    rank_plot
    
  })
  
  plot2_options <- reactive({ 
    req(input$time_type)
    xvar_name <- names(time_group_pairs)[time_group_pairs == input$time_type]
    
    if (input$time_type %in% c('Date', 'Week_Day', 'Week')) {
      fill_col = 'Month'
      fill_guide = 'Month'
    } else {
      fill_col = 'Week_Day'
      fill_guide = 'Week Day'
    } 
    ggplot(songs_selected(), aes_string(input$time_type, fill = fill_col)) + 
      geom_bar() +
      scale_fill_discrete(name=fill_guide) +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(15,0,10,0,"pt")),
            axis.title.y = element_blank(), 
            axis.title.x = element_text(size = 14, margin = margin(15,0,-7,0,"pt")),
            legend.position = "bottom") + 
      labs(title = paste('Number of Songs Played Each', xvar_name), 
           x = xvar_name)
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
  
  #Output Data and plots
  
  #Tab Panel 1 outputs
  #create output main plot here
  output$top_plot <- renderPlot({ 
    ggplot(top_songs(), aes(reorder(art_song, n), n), color = "Black") +
      geom_segment(aes(xend = reorder(art_song, n), yend = 0), color = 'grey40') +
      geom_point(size = 3) + #aes(colour = viridis(20, option = "D")), 
      coord_flip() +
      labs(y = "Play Count", x="", title = "Top 10 Songs in Timeframe") +
      scale_y_continuous(expand=c(0,0), limits = c(0,max(top_songs()$n)+1)) +
      #scale_color_viridis(discrete = T, name = "Artist") +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(15,0,10,0,"pt")),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(color = 'grey80', linetype = '56'),
            axis.ticks = element_blank(),
            axis.text = element_text(size=10),
            axis.title=element_text(size=11), 
            panel.border = element_blank(),
            panel.background = element_blank()) 
    })
  #output$top_songs_table <- DT::renderDataTable({ DT::datatable(top_songs()) })
  output$table <- DT::renderDataTable({ DT::datatable(songs_selected()[2:6], options = list(pageLength = 10)) })
  
  #Tab Panel 2 outputs
  output$main_plot <- renderPlot({ main_plot_options() })
  output$bump_rank_table <- DT::renderDataTable({ DT::datatable(bump_rank() %>% 
                                                                   select(-rank) %>%
                                                                   spread(Month, played_count) %>%
                                                                   arrange(desc(sum_played))) })

  #Tab Panel 3 outputs
  output$second_plot <- renderPlot({ plot2_options() })
  #add dataframes that show top artist and album by time grouping
  output$pan3_table <- DT::renderDataTable({ DT::datatable(pan3_tribble(), options = list(pageLength = 10)) })
  
  #Tab Panel 4 outputs
  
  
  #Close the server definition
})

##################
# User Interface #
##################

#generic line initiating the UI
ui <- shinyUI(fluidPage(
  
  #Add a title
  titlePanel("Alexa Songs Played (2018)"),
  
  #This creates a layout with a left sidebar and main section
  sidebarLayout(
    
    #beginning of sidebar section
    #usually includes inputs
    sidebarPanel(
      dateRangeInput("date_range", "Date Range:", 
                     start = min_date,
                     end = max_date,
                     min = min_date,
                     max = max_date),
      radioButtons("y_axis", "Rank by:",
                   c("Artist" = "main_artist",
                     "Album" = "album")),
      radioButtons("time_type", "Time Grouping:",
                   time_group_pairs)
    ),
    
    #beginning of main section
    mainPanel(tabsetPanel(
      tabPanel("Top Songs PLayed:", 
               plotOutput(outputId = 'top_plot'),
               #DT::dataTableOutput(outputId ='top_songs_table'),
               DT::dataTableOutput(outputId ='table')),
      tabPanel("Month by Month Popularity:", 
               plotOutput(outputId = 'main_plot'),
               DT::dataTableOutput(outputId ='bump_rank_table')),
      tabPanel("Frequency Plot:", 
               plotOutput(outputId = 'second_plot'),
               DT::dataTableOutput(outputId = 'pan3_table')),
      tabPanel("About:", 
               "Inspirations: http://www.jayblanco.com/blog/2016/7/9/using-lastfm-and-r-to-understand-my-music-listening-habits
               \n
               http://data-slinky.com/2016/07/31/bump_charts.html"))
      )
  )
  
  #Close the UI definition
))

##############
# Launch App #
##############

#generic line that launches the app
shinyApp(ui = ui, server = server)
