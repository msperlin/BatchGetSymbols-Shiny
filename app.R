#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(BatchGetSymbols)
library(stringr)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(purrr)
library(scales)
library(PerformanceAnalytics)

years_back <- 3
available_indices <- c('SP500', 'FTSE', 
                       'Ibovespa')
cache_folder_indices <- 'cache-indices'

# load data
df_ftse <- GetFTSE100Stocks(cache.folder = cache_folder_indices)
df_ibov <- GetIbovStocks(cache.folder = cache_folder_indices)
df_sp500 <- GetSP500Stocks(cache.folder = cache_folder_indices)

stock_indices <- readr::read_csv('data/stock_indices.csv',
                                 col_types = readr::cols())

n_available_stocks <- n_distinct(c(df_ftse$tickers,
                                   df_ibov$tickers,
                                   df_sp500$Tickers))

# define global options
options(digits = 4,
        accuracy = 0.01)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "BatchGetSymbols"),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Single Stocks", tabName = "single_stocks", icon = icon("dashboard")),
            menuItem("Multiple Stocks", tabName = "multiple_stocks", icon = icon("th")),
            menuItem("Indices", tabName = "indices", icon = icon("chart-line"))
        )
    ),
    dashboardBody(
        tabItems(
            # Single stocks tab ----
            tabItem(tabName = "single_stocks",
                    fluidRow(
                        infoBox('Market Indices', 
                                color =  'light-blue',
                                value = n_distinct(available_indices),
                                icon = icon('landmark'), 
                                width = 3),
                        uiOutput('infobox_nstocks'),
                        uiOutput('infobox_nprices')
                        
                    ),
                    fluidRow(
                        box(width = 3,
                            selectInput('index', 'Choose your Index', 
                                        choices = available_indices, 
                                        selected = sample(available_indices, 1))),
                        box(width = 3,
                            uiOutput('ticker_ui')
                        ),
                        box(width = 3,
                            dateRangeInput('date_range', 
                                           label = 'Time period',
                                           start = Sys.Date() - years_back*365,
                                           end = Sys.Date())
                        )
                    ),
                    fluidRow(
                        tabBox(width = 9,
                               tabPanel(title = 'Price Plot',
                                   htmlOutput('company_name'),
                                   tags$hr(),
                                   plotOutput("price_plot")
                               ),
                               tabPanel(title = 'Performance',
                                   tableOutput('perf_table')
                               ),
                               tabPanel(title = 'Download Data',
                                        downloadBttn(
                                            outputId = "dl_single_csv",
                                            label = 'Download CSV File',
                                            style = "simple",
                                            color = "primary"
                                        ),
                                        downloadBttn(outputId = 'dl_single_xlsx',
                                                     label =  'Download Excel File',
                                                     style = "simple",
                                                     color = "primary")
                               )
                        )
                    ),
                    
            ),
            
            # Multiple stocks tab ----
            tabItem(tabName = 'multiple_stocks',
                    fluidRow(
                        box(width = 6, 
                            uiOutput('multiple_tickers_ui')
                        ),
                    ),
                    fluidRow(
                        box(width = 6, 
                            plotOutput('multiple_price_plot'))
                    ),
                    fluidRow(
                        box(h3('Performance Table'),
                            tableOutput('perf_multiple_stocks'))
                    )
            ),
            # Indices stocks tab ----
            tabItem(tabName = 'indices',
                    tabPanel('Introduction',
                             p('23123')
                    )
            )
            
        )
    )
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$dl_single_csv <- downloadHandler(
        filename = function() {
            paste('bgs-data_', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            
            df_prices <- get_single_price()
            
            readr::write_csv(df_prices, con)
        }
    )
    
    output$dl_single_xlsx <- downloadHandler(
        filename = function() {
            paste('bgs-data_', Sys.Date(), '.xlsx', sep='')
        },
        content = function(con) {
            
            df_prices <- get_single_price()
            
            writexl::write_xlsx(df_prices, con)
        }
    )
    
    get_company_info <- reactive({
        
        req(input$ticker)
        
        ticker <- input$ticker
        index <- input$index
        if (index == 'SP500') {
            
            idx <- df_sp500$Tickers == ticker
            company_name <- df_sp500$Company[idx]
            company_sector <- df_sp500$GICS.Sector[idx]
            
        } else if (index == 'FTSE') {
            
            idx <- df_ftse$tickers == ticker
            company_name <- df_ftse$company[idx]
            company_sector <- df_ftse$ICB.sector[idx]
            
        } else if (index == 'Ibovespa') {
            
            idx <- df_ibov$tickers == str_remove(ticker, '.SA')
            company_name <- df_ibov$ticker.desc[idx]
            company_sector <- 'Sector not available'
            
        }
        
        l_out <- list(company_name = company_name,
                      company_sector = company_sector)
        
        return(l_out)
        
    })
    
    
    get_single_price <- reactive({
        tib_prices <- tibble()
        
        if (is.null(input$date_range)) {
            first_date <- Sys.Date() - 5*365
            last_date <- Sys.Date()
        } else {
            first_date = as.Date(input$date_range[1])
            last_date = as.Date(input$date_range[2])
        }
        
        try({
            tib_prices <- BatchGetSymbols(
                tickers = input$ticker,
                first.date = first_date,
                last.date = last_date,
                thresh.bad.data = 0.1)[[2]]
        })
        
        return(tib_prices)
    }) 
    
    get_available_tickers <- reactive({
        if (input$index == 'SP500') {
            available_tickers <- df_sp500$Tickers
        } else if (input$index == 'FTSE') {
            available_tickers <- df_ftse$tickers
        } else if (input$index == 'Ibovespa') {
            available_tickers <- paste0(df_ibov$tickers, '.SA')
        }
        return(available_tickers)
    })
    
    output$infobox_nstocks <- renderUI({
        
        available_tickers <- get_available_tickers()
        
        infoBox(str_glue('Available Stocks for {input$index}'), 
                value = n_distinct(available_tickers),
                color =  'light-blue',
                icon = icon('balance-scale'),
                width = 3)
    })
    
    output$infobox_nprices <- renderUI({
        
        available_tickers <- get_available_tickers()
        
        df_prices <- get_single_price()
        
        infoBox(str_glue('Number of observations for {input$ticker}'), 
                subtitle = str_glue(
                    '{input$date_range[1]} to {input$date_range[2]}'
                ),
                value = nrow(df_prices),
                color =  'light-blue',
                icon = icon('chart-line'),
                width = 3)
    })
    
    output$multiple_tickers_ui <- renderUI({
        
        available_tickers <- get_available_tickers()
        
        selectInput('multiple_tickers', 
                    label = 'Select Tickers', 
                    choices = available_tickers,
                    selected = sample(available_tickers, 2),
                    multiple = TRUE)
    })
    
    output$ticker_ui <- renderUI({
        
        available_tickers <- get_available_tickers()
        
        selectInput('ticker', 
                    label = 'Select your ticker',
                    choices = sort(available_tickers),
                    selected = sample(available_tickers, 1))
    })
    
    output$company_name <- renderText({
        l_out <- get_company_info()
        
        str_glue('<h3>{l_out$company_name}</h3> <p>{l_out$company_sector}</p>')
    })
    
    
    output$price_plot <- renderPlot({
        
        shiny::validate(
            need(input$ticker, 'Waiting for ticker..')
        )
        
        df_prices <- get_single_price()
        
        if ( (nrow(df_prices) < 1)||is.null(df_prices$ticker)) {
            sendSweetAlert(
                session = session,
                title = "Error!",
                text = "No Data found in Yahoo Finance (try other ticker?)",
                type = "warning"
            )
            
            output$text <- renderText('Not enough data points in plot. Please select another ticker..')
            return(print(ggplot()))
        } else {
            output$text <- renderText(str_glue('Data Ok, got {nrow(df_prices)} rows'))
            p <- ggplot(df_prices, aes(x = ref.date, y = price.adjusted)) + 
                geom_line() + 
                labs(title = str_glue('Plot for {isolate(input$ticker)}')) + 
                theme_minimal()
            
            print(p)
        }
        
        
    }) 
    
    output$perf_table <- renderTable({
        df_prices <- get_single_price()
        
        n_years <- (max(df_prices$ref.date) -
            min(df_prices$ref.date))[[1]]/365
        total_return <- last(df_prices$price.adjusted)/
            first(df_prices$price.adjusted) - 1
        ret_per_year <- (1 + total_return)^(1/n_years) - 1
        my_drawdown <- maxDrawdown(df_prices$ret.adjusted.prices)
        
        tab <- tibble(
            'Ticker' = df_prices$ticker[1],
            'Number of Years' = n_years,
            'Total Return' = percent(total_return),
            'Return per year' = percent(ret_per_year),
            'Max Drawdown' = percent(my_drawdown)
            )
        
        tab
    })
    
    get_multiple_prices <- reactive({
        tib_prices <- tibble()
        
        try({
            tib_prices <- BatchGetSymbols(
                tickers = input$multiple_tickers,
                first.date = Sys.Date()-year_back*365,
                thresh.bad.data = 0.1)[[2]]
        })
        
        return(tib_prices)
    })
    
    output$multiple_price_plot <- renderPlot({
        
        shiny::validate(
            need(input$'multiple_tickers', 'Waiting for ticker..')
        )
        
        df_prices <- get_multiple_prices()
        
        if ( (nrow(df_prices) < 1)||is.null(df_prices$ticker)) {
            sendSweetAlert(
                session = session,
                title = "Error!",
                text = "No Data found in Yahoo Finance (try other tickers?)",
                type = "warning"
            )
            
            output$text <- renderText('Not enough data points in plot. Please select another ticker..')
            return(print(ggplot()))
        } else {
            
            # process price data
            
            calc_cumret <- function(df_prices, ticker_in) {
                df_prices_temp <- df_prices %>%
                    select(ref.date, ticker, ret.adjusted.prices) %>%
                    filter(ticker == ticker_in) %>%
                    na.omit()
                
                df_prices_temp$ret.adjusted.prices[1] <- 0 
                df_prices_temp$cum_ret <- cumprod(1+df_prices_temp$ret.adjusted.prices)
                
                return(df_prices_temp)
            }
            
            df_prices_2 <- bind_rows(
                map(unique(df_prices$ticker), 
                    calc_cumret, 
                    df_prices = df_prices)
            )
            
            p <- ggplot(df_prices_2, aes(x = ref.date, y = cum_ret,
                                         color = ticker)) + 
                geom_line() + 
                labs(title = str_glue(
                    'Prices of {n_distinct(df_prices_2$ticker)} stocks'),
                    x = '',
                    y = 'Cumulative Return') + 
                theme_minimal() +
                scale_y_continuous(labels = scales::percent)
            
            print(p)
        }
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
