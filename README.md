# R Project Compilation

## Project: IBTrACS Trend Analysis
- This project entails the study of attributes amd volume of tropical storm occurances observed in the North Atlantic Storm Basin. The data comes from the International Best Track Archive for Climate Stewardship (IBTrACS) website.
- The data types for the 16 imported columns are as follows:
  - `SEASON`, `NUMBER`, `WMO_WIND`, `WMO_PRES`, `DIST2LAND`, and `LANDFALL` must be of type "integer"
  - `LAT` and `LON` must be of type "double" or "real"
  - The rest of the columns must be of type "character"
              
## Project: Data Visualization (Screen Time & Cal Women's Basketball)
The purpose of this project is to demonstrate proficiency in data visualization, and has 2 distinct components: 
- Replicate iPhone screen time's barchart and improve its visualization 
- In our analysis of the Cal Women's Basketball 2021-2022 Game-by-Game Scores data, we aim to identify the crucial factors that contribute to victory in basketball games. By closely examining Cal's performance throughout the season, we can uncover the correlation between scoring points and successful shooting. To visualize this relationship and its impact on game outcomes, we have created a scatterplot. This graphic effectively represents the points scored by the Cal Women's Basketball team on the x-axis, while the y-axis represents Cal's field goal percentage. Furthermore, we have color-coded the data points based on the game's outcome: green for a win and red for a loss for Cal.

## Project: Investment Simulator
View the published interactive shiny app (hosted by shiny.io): [Investment Simulator](https://ireneliang.shinyapps.io/InvestmentSimulator/)

This project consists of two main components:
- In the first section, I focused on defining important financial math formulas such as future value and ordinary annuity. In addition, I conducted 50 simulations of a 10-year investment period in a Total Stock Market index fund. To enhance understanding and analysis, I created a comprehensive timeline visualization for these simulations.
- In the section section, I leveraged the previously defined formulas to develop an investment simulator through the use of a **Shiny** app. This interactive application enhances the user experience and facilitates practical exploration of investment scenarios.

There are 4 input widgets:
- Money Input
- Target Parameters
- Portfolio Composition
- Simulation Parameters

There are 4 outputs:
- A timeline of yearly balances where each grey line represents one simulation and the red lines represent the 25th, 50th, and 75th percentile of the simulations
- A bargraph for the empirical proportion of reaching the target for each year
- Two tables of summary statistics


## Project: Text Analysis of The Simpsons Transcripts 
View the published interactive shiny app (hosted by shiny.io): [The Simpsons Transcripts](https://ireneliang.shinyapps.io/TheSimpsonsTranscripts/)

This project analyzes The Simpsons' Transcript dataset, comprising characters, locations, episode details, and script lines from around 600 episodes of the show dating back to 1989. I conducted both the word frequency and sentiment analysis on the dataset.

By leveraging the rich dataset of the Simpsons Transcript, this project aims to delve into the linguistic characteristics, prevalent themes, and emotional nuances present in one of the most iconic animated series in television history. The Word Frequency and Sentiment Analysis serve as powerful tools for unraveling the language usage, narrative patterns, and comedic elements that have contributed to The Simpsons' enduring popularity and cultural impact.

There are 4 input widgets
- Top # Most Frequent Words: A slider for the desired number of top words
- Season: A selection bar of the specific season of The Simpsons
- Include/Remove Stopwords: A radio button for whether to include stopwords (common words with little value)
- Facet by Season: A radio button for whether to facet by season

#### Part 1: Word Frequency Analysis
By examining the frequency of words used in the script lines, I extracted valuable insights into the prevailing language patterns and recurring themes throughout the series. This analysis allows us to uncover the most commonly used terms, phrases, and concepts that have shaped the show's distinctive style and comedic elements.

Outputs: 
- A barchart for the top selected number of most frequent words
- A wordcloud where the more frequent a word is, the bigger its size is
- A tabulated numerical summary of frequencies, sorted by descending word count

#### Part 2: Sentiment Analysis
I applied sentiment analysis techniques to evaluate the emotional tone conveyed in the script lines. To achieve this, I first tokenized the transcript and then utilized the AFINN sentiment lexicon, which assigns numeric positivity scores to individual words. The sentiment analysis results were visualized using a bar plot.

By leveraging the power of natural language processing and text mining, I determined whether the expressed sentiment in the script lines is positive or negative. This analysis provides valuable insights into the emotional dynamics at play within episodes and reveals how the show's creators skillfully incorporate various emotional undertones into their humor and storytelling.

Outputs: 
- A flipped barchart showing the most common words that contribute to positive or negative sentiments in the selected season
- A barchart that shows the sentiment trajectery of episodes in the selected season with the average score in dashed line
- Two tables: 
  - The most common words that contribute to positive and negative sentiments
  - The minimum and maximum sentiment score for the selected season with an output text showing which episode the min and max score occurs at

