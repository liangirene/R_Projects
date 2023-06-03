# ===============================================
# Fill in the following fields
# ===============================================
# Title: Investment Simulator Shiny App
# Description: This is a shiny app on total market index fund investment simulator
# Author: Irene Liang
# Date: 10/29/2022


# ===============================================
# Required packages (you can use other packages if you want)
# ===============================================
library(shiny)
library(tidyverse)
library(tidyr)
library(rsconnect)


# Define UI that has four columns
ui <- fluidPage(
    
    # Application title
    titlePanel("HW5: Investment Simulator for Total Market Index Fund "),
    fluidRow(
        # Inputs for initial amount, and periodic contributions 
        column(width = 3,
               h4("Money Input"),
               numericInput(inputId = "initial_amount", 
                            label = "Initial Amount", 
                            value = 1000),
               numericInput(inputId = "periodic_contributions", 
                            label = "Periodic Contributions", 
                            value = 360)
        ),
        
        # Inputs for target amount, and number of years 
        column(width = 3,
               h4("Target Parameters"),
               numericInput(inputId = "target_amount", 
                            label = "Target Amount", 
                            value = 5000),
               sliderInput(inputId = "num_years", 
                           label = "Number of Years", 
                           value = 10, 
                           min = 1, 
                           max = 50)
        ),
        
        # Inputs for mean and standard deviation of annual inflation rates
        column(width = 3,
               h4("Portfolio Composition"),
               br(),
               sliderInput(inputId = "stock_prop", 
                           label = "Proportion of Stocks (%)", 
                           value = 60,
                           min = 0,
                           max = 100,
                           step = 5),
        ),
        
        # Inputs for number of simulations, and random seed
        column(width = 3,
               h4("Simulation Parameters"),
               sliderInput(inputId = "num_simulations", 
                           label = "Number of Simulations", 
                           value = 50, 
                           min = 10, 
                           max = 100),
               numericInput(inputId = "seed", 
                            label = "Random Seed", 
                            value = 123)
        )
    ),
    
    hr(),
    h4('Plot 1: Timeline of Yearly Balances'),
    plotOutput('timeline_balance'),
    
    hr(),
    h4('Plot 2: Probability of Reaching Target'),
    plotOutput('prob_target'),
    
    hr(),
    fluidRow(
        column(width = 6,
               h4('Summary Table 1'),
               p('Summary Statistics of Simulations by Year'),
               tableOutput('summary_stats'),
        ),
        column(width = 6,
               h4("Summary Table 2"),
               p('Simulations > Initial Amount'),
               tableOutput('simulation_compare')
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # ------------------------------------------
    # Portfolio combinations of stocks and bonds
    # ------------------------------------------
    portfolio_tb <- data.frame(
        stocks = seq(from = 0, to = 100, by = 5),
        bonds = seq(from = 100, to = 0, by = -5),
        average = c(5.09, 5.40, 5.70, 6.00, 6.29, 6.57, 6.84, 7.11, 7.37, 7.62, 7.87, 8.11, 8.34, 8.56, 8.78, 8.99, 9.19, 9.38, 9.57, 9.74, 9.91),
        SD = c(4.03, 3.99, 4.11, 4.37, 4.74, 5.21, 5.74, 6.32, 6.94, 7.59, 8.25, 8.94, 9.64, 10.35, 11.07, 11.80, 12.54, 13.28, 14.03, 14.79, 15.55)
    )
    
    # ------------------------
    # Matrix of simulated data
    # ------------------------
    balance_mat = reactive({
        balance = matrix(0, nrow = input$num_years + 1, ncol = input$num_simulations)
        balance[1, ] = input$initial_amount
        set.seed(input$seed)
        
        for (sim in 1:input$num_simulations) {
            return_rates = rnorm(input$num_years, 
                                 mean = portfolio_tb[portfolio_tb$stocks == input$stock_prop, "average"]/100, 
                                 sd = portfolio_tb[portfolio_tb$stocks == input$stock_prop, "SD"]/100)
            for (year in 1:input$num_years) {
                balance[year+1,sim] = (balance[year,sim] * (1 + return_rates[year])) + input$periodic_contributions # compound previous balance for 1 year
            }
        }
        
        colnames(balance) = paste0("sim", 1:input$num_simulations)
        balance
    })
    
    # -------------------------------------------------------
    # Reshape table into long format (for ggplot convenience)
    # -------------------------------------------------------
    dat_sim = reactive({
        tbl = as.data.frame(balance_mat())
        tbl <- tbl %>% rowwise() %>% mutate(percentile25 = quantile(c_across(sim1:paste0("sim", input$num_simulations)), probs = 0.25),
                                            percentile50 = quantile(c_across(sim1:paste0("sim", input$num_simulations)), probs = 0.50),
                                            percentile75 = quantile(c_across(sim1:paste0("sim", input$num_simulations)), probs = 0.75))
        tbl$year = 0:input$num_years
        
        # reshape table into "tall" or "long" format
        dat = pivot_longer(
            data = tbl, 
            cols = starts_with(c("sim", "percentile")), 
            names_to = "simulation_percentile",
            values_to = "amount_stat")
        
        # indicator for whether the row is a simulation
        dat$indicator <- ifelse(dat$simulation_percentile == "percentile25", "percentile25", 
                                ifelse(dat$simulation_percentile == "percentile50", "percentile50", 
                                ifelse(dat$simulation_percentile == "percentile75", "percentile75",
                                'sim')))
        
        dat
    })
    
    # -----------------------------------------
    # Output: Timelines of simulations (Plot 1)
    # -----------------------------------------
    
    # Calculate the average number of years to reach the target amount
    average_yr_target_reac = reactive({
        yr_reach_target_vec <- c()
        for (sim in 1:input$num_simulations) {
            yr_reach_target_vec = append(yr_reach_target_vec, suppressWarnings(min(which(balance_mat()[, sim] >= input$target_amount)) - 1))
        }
        # rm inf from simulations that do not reach target at the end of specified years
        yr_reach_target_vec = yr_reach_target_vec[is.finite(yr_reach_target_vec)]
        average_yr_target = mean(yr_reach_target_vec)
        average_yr_target
    })
    
    output$timeline_balance <- renderPlot({
        df = dat_sim()
        ggplot(data = df %>% filter(indicator == "sim"), aes(x = year, y = amount_stat)) +
            geom_line(aes(group = simulation_percentile), color = "black", alpha = 0.2) +
            geom_line(data = df %>% filter(indicator == "percentile25"), aes(x = year, y = amount_stat), color = "darkblue", size = 1.3) + 
            geom_line(data = df %>% filter(indicator == "percentile50"), aes(x = year, y = amount_stat), color = "red", size = 1.3) +
            geom_line(data = df %>% filter(indicator == "percentile75"), aes(x = year, y = amount_stat), color = "forestgreen", size = 1.3) +
            geom_hline(yintercept = input$target_amount, linetype="dashed") +
            geom_vline(xintercept = average_yr_target_reac(), linetype="dashed") +
            annotate(geom = "text", label = paste(paste("average time to reach target:", round(average_yr_target_reac(), 2), "years")), 
                     x = average_yr_target_reac(), y = 0, size = 5) +
            annotate(geom = "text", label = "Target Amount", x=input$num_years - 0.35, y=input$target_amount - 415, size = 5) +
            
            annotate(geom="text", label="25th Percentile", x=input$num_years + input$num_years/30, 
                     y=df[(df$indicator == 'percentile25') & (df$year == input$num_years),]$amount_stat, color = "darkblue", size = 4) +
            annotate(geom="text", label="50th Percentile", x=input$num_years + input$num_years/30, 
                     y=df[(df$indicator == 'percentile50') & (df$year == input$num_years),]$amount_stat, color = "red", size = 4) +
            annotate(geom="text", label="75th Percentile", x=input$num_years + input$num_years/30, 
                     y=df[(df$indicator == 'percentile75') & (df$year == input$num_years),]$amount_stat, color = "forestgreen", size = 4) +
            
            scale_alpha_manual(values = c('TRUE' = 0.1, 'FALSE' = 1)) +
            guides(alpha = 'none') +
            labs(y = "balance ($)", x = "years") +
            ggtitle(paste(paste("Timeline for", input$num_simulations), paste("Simulations of a", input$num_years),"year Investment Period"), 
                    subtitle = "(25th, 50th, 75th Percentile of Simulations Highlighted)") +
            theme(plot.title=element_text(hjust=0.5, size=20), 
                  plot.subtitle=element_text(hjust=0.5, size = 15))
    })
    
    
    # --------------------------------------------------------
    # Output: Probability of Reaching Target Barplot (Plot 2)
    # --------------------------------------------------------
    
    # Calculate the probability of reaching target for each year
    prob_target_mat = reactive({
        prob_target_vec <- c()
        for (year in 1:input$num_years) {
            prob_target_vec = append(prob_target_vec, (sum(balance_mat()[year,] >= input$target_amount)/input$num_simulations))
        }
        prob_target_df = data.frame(year = 1:input$num_years,
                                    prob_target = prob_target_vec)
        prob_target_df
    })
    
    output$prob_target <- renderPlot({
        ggplot(data = prob_target_mat(), aes(x = year, y = prob_target)) +
            geom_bar(stat="identity", color="darkblue", fill="lightblue") +
            geom_line(size = 0.5, color="red") +
            geom_text(aes(label = prob_target), vjust = -0.2, size = 5) +
            ylim(0,1) +
            labs(x = "Year", y = "Probability") +
            ggtitle(paste(paste("Empirical Proportion of Reaching Target $",input$target_amount),"For Each Year")) +
            theme_minimal() +
            theme(plot.title=element_text(size=20, hjust=0.5))
    })
    
    # --------------------------
    # Output: Summary Statistics
    # --------------------------
    output$summary_stats <- renderTable({
        dat_sim() %>% 
            filter(indicator == "sim") %>%
            filter(year %in% seq(0, input$num_years, by = 1)) %>%
            group_by(year) %>%
            summarise(
                min = min(amount_stat),
                mean = mean(amount_stat),
                percentile_25 = quantile(amount_stat, probs = 0.25),
                percentile_50 = quantile(amount_stat, probs = 0.5),
                percentile_75 = quantile(amount_stat, probs = 0.75),
                max = max(amount_stat)
            )
    })
    
    # ----------------------------------------------------------------------
    # Output: Number & Proportion of Simulations Greater Than initial_amount
    # ----------------------------------------------------------------------
    output$simulation_compare <- renderTable({
        dat_sim() %>% 
            filter(indicator == "sim") %>%
            group_by(year) %>%
            summarise(
                count = sum(amount_stat > input$initial_amount),
                proportion = count / input$num_simulations
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
