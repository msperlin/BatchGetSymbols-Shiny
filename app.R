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

year_back <- 5
available_indices <- c('SP500', 'FTSE', 
                       'Ibovespa')
cache_folder_indices <- 'cache-indices'
df_ftse <- GetFTSE100Stocks(cache.folder = cache_folder_indices)
df_ibov <- GetIbovStocks(cache.folder = cache_folder_indices)
df_sp500 <- GetSP500Stocks(cache.folder = cache_folder_indices)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "BatchGetSymbols Dashboard"),
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
                        column(3,
                               infoBox('my info box', value = 13, subtitle = 'subtitle')),
                        column(3,
                               infoBox('my info box', value = 13, subtitle = 'subtitle'))
                        ),
                    fluidRow(
                        box(selectInput('index', 'Choose your Index', 
                                        choices = available_indices, 
                                        selected = sample(available_indices, 1)),
                            tags$hr(),
                            uiOutput('ticker_ui'))),
                    fluidRow(
                        box(
                            htmlOutput('company_name'),
                            tags$hr(),
                            #h4('Ajusted Prices'),
                            plotOutput("price_plot"),
                            verbatimTextOutput('text')
                        )
                        
                    )
            ),
            
            # Multiple stocks tab ----
            tabItem(tabName = 'multiple_stocks',
                    h3('Multiple stocks')
            ),
            # Indices stocks tab ----
            tabItem(tabName = 'indices',
                    h3('Indices')
            )
        )
    )
    
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
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
    
    
    get_prices <- reactive({
        BatchGetSymbols(
            tickers = input$ticker,
            first.date = Sys.Date()-year_back*365,
            thresh.bad.data = 0.1)[[2]]
    }) 
    
    output$ticker_ui <- renderUI({
        
        if (input$index == 'SP500') {
            available_tickers <- df_sp500$Tickers
        } else if (input$index == 'FTSE') {
            available_tickers <- df_ftse$tickers
        } else if (input$index == 'Ibovespa') {
            available_tickers <- paste0(df_ibov$tickers, '.SA')
        }
        
        selectInput('ticker', label = 'Select your ticker',
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
        
        df_prices <- get_prices()
        
        if ( (nrow(df_prices) < 1)||is.null(df_prices$ticker)) {
            sendSweetAlert(
                session = session,
                title = "Error...",
                text = "No Data found in YFinance (try other ticker?)",
                type = "error"
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
    
}

# Run the application 
shinyApp(ui = ui, server = server)
