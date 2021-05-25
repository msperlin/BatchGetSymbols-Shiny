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
library(shinycssloaders)

# load data
#source('global.R', local = TRUE)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = 'blue',
    dashboardHeader(title = "BatchGetSymbols"),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Single Stock/Index", tabName = "single_stocks", icon = icon("dashboard")),
            menuItem("Multiple Stocks", tabName = "multiple_stocks", icon = icon("th")),
            #menuItem("Indices", tabName = "indices", icon = icon("chart-line")),
            menuItem("About", tabName = "about", icon = icon("address-card"))
        )
    ),
    dashboardBody(
        tabItems(
            # Single stocks tab ----
            ui_single_stocks(),
            
            # Multiple stocks tab ----
            ui_multiple_stocks(),
            
            # Indices stocks tab ----
            #ui_index_tab(),
            
            # about pannel ----
            ui_about()
            
            
        )
    )
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$ret_plot <- renderPlot({
        df_in <- get_single_price()
        
        p <- do_perf_plot(df_in)
        p
        
    })
    
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
    
    output$dl_multiple_csv <- downloadHandler(
        filename = function() {
            paste('bgs-data-multiple_', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            
            df_prices <- get_multiple_prices()
            
            readr::write_csv(df_prices, con)
        }
    )
    
    output$dl_multiple_xlsx <- downloadHandler(
        filename = function() {
            paste('bgs-data-multiple_', Sys.Date(), '.xlsx', sep='')
        },
        content = function(con) {
            
            df_prices <- get_multiple_prices()
            
            writexl::write_xlsx(df_prices, con)
        }
    )
    
    output$dl_index_csv <- downloadHandler(
        filename = function() {
            paste('bgs-data-index_', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            
            df_prices <- get_index_price()
            
            readr::write_csv(df_prices, con)
        }
    )
    
    output$dl_index_xlsx <- downloadHandler(
        filename = function() {
            paste('bgs-data-index_', Sys.Date(), '.xlsx', sep='')
        },
        content = function(con) {
            
            df_prices <- get_index_price()
            
            writexl::write_xlsx(df_prices, con)
        }
    )
    
    get_company_info <- reactive({
        
        req(input$ticker)
        
        ticker <- input$ticker
        collection <- input$collection_single
        
        description <- df_collections %>%
            filter(ticker == input$ticker,
                   collection == input$collection_single) %>%
            pull(description)
        
        return(description)
        
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
                cache.folder = cache_folder_stocks,
                thresh.bad.data = 0.1)[[2]]
        })
        
        return(tib_prices)
    })
    
    get_index_price <- reactive({
        tib_prices <- tibble()
        #browser()
        
        if (is.null(input$date_range)) {
            first_date <- Sys.Date() - 5*365
            last_date <- Sys.Date()
        } else {
            first_date = as.Date(input$date_range_index[1])
            last_date = as.Date(input$date_range_index[2])
        }
        
        try({
            tib_prices <- BatchGetSymbols(
                tickers = input$index_to_pick,
                first.date = first_date,
                last.date = last_date,
                thresh.bad.data = 0.1, 
                cache.folder = cache_folder_indices)[[2]]
        })
        
        return(tib_prices)
    })
    
    get_available_tickers <- reactive({

        available_tickers <- df_collections %>%
            filter(collection == input$collection_single) %>%
            pull(ticker)
        
        return(available_tickers)
    })
    
    get_available_tickers_multiple <- reactive({
        
        available_tickers <- df_collections %>%
            filter(collection == input$collection_multiple) %>%
            pull(ticker)
        
        return(available_tickers)
    })
    
    output$infobox_nstocks <- renderUI({
        
        available_tickers <- get_available_tickers()
        
        infoBox(str_glue('Available Stocks for {input$collection_single}'), 
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
    
    output$multiple_ticker_ui <- renderUI({
        
        available_tickers <- get_available_tickers_multiple()
        
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
        description <- get_company_info()
        
        str_glue('<h3>{description}</h3>')
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
                labs(title = str_glue('Plot for {isolate(input$ticker)}'),
                     x = '',
                     y = 'Ajusted Price') + 
                theme_minimal()
            
            print(p)
        }
        
        
    }) 
    
    output$perf_table1 <- renderTable({
        df_prices <- get_single_price()
        
        tab <- calc_performance(df_prices)
        
        tab
    })
    
    output$perf_table2 <- renderTable({
        df_prices <-get_multiple_prices()
        
        tab <- calc_performance(df_prices)
        
        tab
    })
    
    output$perf_table3 <- renderTable({
        df_prices <-get_index_price()
        
        tab <- calc_performance(df_prices)
        
        idx <- match(tab$Ticker, stock_indices$Symbol)
        tab$Index <- stock_indices$Name[idx]
        
        tab
    })
    
    get_multiple_prices <- reactive({
        tib_prices <- tibble()
        
        try({
            tib_prices <- BatchGetSymbols(
                tickers = input$multiple_tickers,
                first.date = input$date_range_multiple[1],
                last.date = input$date_range_multiple[2],
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
    
    # index plot ----
    output$index_price_plot <- renderPlot({
        
        shiny::validate(
            need(input$index_to_pick, 'Waiting for index..')
        )
        
        df_prices <- get_index_price()
        
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
            df_prices_2 <- bind_rows(
                map(unique(df_prices$ticker), 
                    calc_cumret, 
                    df_prices = df_prices)
            )
            
            p <- ggplot(df_prices_2, aes(x = ref.date, 
                                         y = cum_ret,
                                         color = ticker)) + 
                geom_line() + 
                labs(title = str_glue(
                    'Prices of {n_distinct(df_prices_2$ticker)} Indices'),
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
