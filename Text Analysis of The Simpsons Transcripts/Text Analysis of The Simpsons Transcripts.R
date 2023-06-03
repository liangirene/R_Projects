# ===============================================
# Fill in the following fields
# ===============================================
# Title: Text Analysis of The Simpsons Transcripts
# Description: Shiny app with visualizations of the results from text analysis performed on The Simpsons Transcripts.
# Author: Irene Liang
# Date: 12 November 2022


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(shiny)
library(rsconnect)


# ===============================================
# Import data
# ===============================================
simpson_text <- read.table("simpsons-transcripts.txt", 
                           header = TRUE,
                           sep ="^", 
                           dec =".")


# ===============================================
# Define "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("HW 6 - Text Analysis of The Simpsons Transcripts"),
  fluidRow(
    # replace with your widgets
    column(3,
           sliderInput(inputId = "top", 
                       label = h4("Top-# Most Frequent Words"), 
                       min = 5, 
                       max = 50, 
                       value = 20, 
                       step = 1)
    ),
    column(3,
           selectInput(inputId = "season", 
                       label = h4("Season"), 
                       choices = c("All",
                                   seq(from = 1, to = 33, by = 1)),
                       selected = "all")
    ),
    
    # replace with your widgets
    column(3,
           radioButtons(inputId = "stopwords", 
                        label = h4("Include/Remove Stopwords"), 
                        choices = c("Include" = "include",
                                    "Remove" = "remove"),
                        selected = "include")
    ),
    
    # replace with your widgets
    column(3,
           radioButtons(inputId = "facet", 
                        label = h4("Facet by Season"), 
                        choices = c("On",
                                    "Off"),
                        selected = "Off")
    ),
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Analysis1",
                       h3("2.A) Word Frequency Analysis"),
                       plotOutput("barplot1", height = 1200),
                       plotOutput("wordcloud1", height = "500px"),
                       hr(),
                       h4("Top Word Count", style="text-align:center"),
                       dataTableOutput('table1')),
              tabPanel("Analysis2", 
                       h3("2.B) Sentiment Analysis"),
                       plotOutput("barplot2", height = 900),
                       hr(),
                       plotOutput("barplot3"),
                       hr(),
                       h4("Most Common Words that Contribute to Positive/Negative Sentiments", style="text-align:center"),
                       dataTableOutput('table2'),
                       h4("Summary Statistics of Sentiment Score", style="text-align:center"),
                       verbatimTextOutput('summary_text1'),
                       verbatimTextOutput('summary_text2'),
                       dataTableOutput('summary_stats'))
              ),
  )


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. data frame to be used in plot1)
  
  # Encapsulate the vector into a data frame
  simpson_dat <- data.frame(year = simpson_text$year, 
                            season = simpson_text$season,
                            episode = simpson_text$episode,
                            title = simpson_text$title,
                            text = simpson_text$text)
  
  # ---------- 2.A) Word Frequency Analysis   ---------- #
  # For all seasons
  simpson_tokens <- unnest_tokens(tbl = simpson_dat, output = word, input = text) # Tokenization
  simpson_freq <- count(simpson_tokens, word) # Word Frequencies
  
  # For 1 selected season
  season_group <- data.frame(group_by(simpson_dat, season))
  season_tokens <- unnest_tokens(tbl = season_group, output = word, input = text) # Tokenization
  season_freq <- count(season_tokens, season, word) # Word Frequencies
  
  # For facet
  simpson_freq_facet <- group_by(count(simpson_tokens, season, word), season)
  season_freq_facet <- group_by(count(season_tokens, season, word), season)
  
  # Remove stopwords 
  t_simpson_freq <- count(anti_join(simpson_tokens, stop_words, by = "word"), word)
  t_season_freq <- count(anti_join(season_tokens, stop_words, by = "word"), season, word)
  t_simpson_freq_facet <- group_by(count(anti_join(simpson_tokens, stop_words, by = "word"), 
                                       season, word), season)
  t_season_freq_facet <- group_by(count(anti_join(season_tokens, stop_words, by = "word"), 
                                     season, word), season)
  top_words <- reactive({
    if (input$stopwords == "include") {
      if (input$season == "All" & input$facet == "Off") {
        top_words <- slice_head(arrange(simpson_freq, desc(n)), n = input$top)
      }
      else if (input$season == "All" & input$facet == "On") {
        top_words <- slice_head(arrange(simpson_freq_facet, desc(n)), n = input$top)
      }
      else if (input$season != "All" & input$facet == "Off") {
        top_words <- slice_head(arrange(season_freq[season_freq$season == input$season, ], 
                                        desc(n)), 
                                n = input$top)
      }
      else if (input$season != "All" & input$facet == "On") {
        top_words <- slice_head(arrange(season_freq_facet[season_freq_facet$season == input$season, ], 
                                        desc(n)), n = input$top)
      }
      top_words %>% rename(count = n)
      }
    
    else if (input$stopwords == "remove") {
      if (input$season == "All" & input$facet == "Off") {
        top_words <- slice_head(arrange(t_simpson_freq, desc(n)), n = input$top)
      }
      else if (input$season == "All" & input$facet == "On") {
        top_words <- slice_head(arrange(t_simpson_freq_facet, desc(n)), n = input$top)
      }
      else if (input$season != "All" & input$facet == "Off") {
        top_words <- slice_head(arrange(t_season_freq[t_season_freq$season == input$season, ], 
                                        desc(n)), n = input$top)
      } 
      else if (input$season != "All" & input$facet == "On") {
        top_words <- slice_head(arrange(t_season_freq_facet[t_season_freq_facet$season == input$season, ], 
                                        desc(n)), n = input$top)
      }
      top_words %>% rename(count = n)
      }
    })
  
  # ---------- 2.B) Sentiment Analysis ---------- #
  # Most Common Positive and Negative Words
  simpson_freq_sent <- group_by(inner_join(simpson_freq, sentiments, by = "word"), sentiment)
  simpson_freq_sent_tidy <- simpson_freq %>% 
    anti_join(stop_words, by = "word") %>%
    inner_join(sentiments, by = "word") %>%
    group_by(sentiment)
  season_freq_sent <- group_by(inner_join(season_freq, sentiments, by = "word"), sentiment)
  season_freq_sent_tidy <- season_freq %>% 
    anti_join(stop_words, by = "word") %>%
    inner_join(sentiments, by = "word") %>%
    group_by(sentiment)
    
  sentiment_words <- reactive({
    if (input$season == "All" && input$stopwords == "include") {
      sentiment_words <- mutate(slice_head(arrange(simpson_freq_sent, desc(n)), 
                                      n = input$top), word = reorder(word,n))
    }
    else if(input$season == "All" && input$stopwords == "remove") {
      sentiment_words <- mutate(slice_head(arrange(simpson_freq_sent_tidy, desc(n)), 
                                           n = input$top), word = reorder(word,n))
    }
    else if (input$season != "All"&& input$stopwords == "include") {
      sentiment_words <- mutate(slice_head(arrange(season_freq_sent[season_freq_sent$season == input$season, ], 
                                              desc(n)), n = input$top), word = reorder(word,n))
    }
    else if (input$season != "All"&& input$stopwords == "remove") {
      sentiment_words <- mutate(slice_head(arrange(season_freq_sent_tidy[season_freq_sent_tidy$season == input$season, ], 
                                                   desc(n)), n = input$top), word = reorder(word,n))
    }
  })

  # Change of sentiments throughout each season
  sentiment_score <- reactive({
    if (input$season == "All") {
      episode_tokens <- unnest_tokens(tbl = simpson_dat, output = word, input = text) # Tokenization
      episode_freq <- episode_tokens %>% group_by(episode)
      
      sentiment_score <- episode_freq %>%
        inner_join(sentiments, by = "word") %>%
        count(episode, sentiment, sort = TRUE) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)
    }
    else if (input$season != "All") {
      ep_group_sel_season <- simpson_dat %>%
        filter(season == input$season) %>%
        group_by(episode)
      episode_tokens <- unnest_tokens(tbl = ep_group_sel_season, output = word, input = text) # Tokenization
      
      sentiment_score <- episode_tokens %>%
        inner_join(sentiments, by = "word") %>%
        count(episode, sentiment, sort = TRUE) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)
    }
    
  })
  
  # code for summary statistics for sentiment_score()
  summary_stats_df <- reactive({
    summary_stats_df <- rbind(
      min = sentiment_score()[min(sentiment_score()$sentiment) == sentiment_score()$sentiment, ],
      max = sentiment_score()[max(sentiment_score()$sentiment) == sentiment_score()$sentiment, ]
    )
    summary_stats_df <- cbind("summary statistics" = c("min", "max"), summary_stats_df)
  })
  
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  # code for plot1
  output$barplot1 <- renderPlot({
    if (input$facet == "Off") {
      ggplot(data = top_words(), aes(x = reorder(word, count), y = count)) + 
        geom_bar(fill = "#f87862", stat = "identity") + 
        labs(title = paste(paste("Top", input$top), "Most Frequent Words")) +
        xlab("Top Words") + ylab("Count") + 
        theme(text = element_text(size = 12),
              plot.title = element_text(hjust = 0.5, size = 20)) +
        coord_flip()
    }
    else if (input$facet == "On") {
      ggplot(data = top_words(), aes(x = reorder_within(word, count, season), y = count)) + # reordering bars within each facet
        scale_x_reordered() +
        facet_wrap(~season, scales = "free", nrow = 3, ncol = 11) +
        geom_bar(fill = "tomato", stat = "identity") +
        labs(title = paste(paste("Top", input$top), "Most Frequent Words")) +
        xlab("Top Words") + ylab("Count") + 
        theme(text = element_text(size = 12),
              plot.title = element_text(hjust = 0.5, size = 20)) +
        coord_flip()
    } 
    
  })
  
  output$wordcloud1 <- renderPlot({
      wordcloud(
        words = top_words()$word, 
        freq = top_words()$count, 
        max.words = 50,
        random.order = FALSE,
        colors = brewer.pal(8, "Dark2"))
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    # replace the code below with your code!!!
    top_words()
  })
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  
  # code for plot2
  output$barplot2 <- renderPlot({
    ggplot(data = sentiment_words(), aes(x = word, y = n, fill = sentiment)) + 
      geom_bar(stat = "identity") + 
      facet_wrap(~sentiment, scales = "free_y") +
      coord_flip() + 
      labs(y = "Contribution to sentiment",
           x = NULL,
           title = paste("Most Common Words that Contribute to Positive/Negative Sentiments in Season", input$season)) +
      theme(text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 20))
  })
  
  output$barplot3 <- renderPlot({
    mean = mean(sentiment_score()$sentiment)
    ggplot(data = sentiment_score(), aes(x = episode, y = sentiment)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      geom_text(aes(label = sentiment)) +
      geom_hline(yintercept = mean(sentiment_score()$sentiment), linetype = "dashed") +
      annotate("label", label = "Average Score", x = tail(sentiment_score()$episode, 1), 
               y = mean(sentiment_score()$sentiment), size = 5) +
      scale_x_discrete() +
      labs(y = "sentiment",
           x = "episode",
           title = paste("Sentiment Trajectery of Episodes in Season", input$season)) +
      theme(text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 20))
    })
  
  
  # code for statistics
  output$table2 <- renderDataTable({
    sentiment_words()
  })
  
  output$summary_text1 <- renderText({
    min_score = sentiment_score()[min(sentiment_score()$sentiment) == sentiment_score()$sentiment, ]$sentiment
    min_episode = sentiment_score()[min(sentiment_score()$sentiment) == sentiment_score()$sentiment, ]$episode
    paste(paste(paste("The minimum sentiment score is", min_score), "for episode"), min_episode)
  })
  output$summary_text2 <- renderText({
    max_score = sentiment_score()[max(sentiment_score()$sentiment) == sentiment_score()$sentiment, ]$sentiment
    max_episode = sentiment_score()[max(sentiment_score()$sentiment) == sentiment_score()$sentiment, ]$episode
    paste(paste(paste("The maximum sentiment score is", max_score), "for episode"), max_episode)
  })
  
  output$summary_stats <- renderDataTable({
    summary_stats_df()
  })
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

