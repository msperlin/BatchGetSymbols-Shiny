ui_index_tab <- function(){
  tabItem(tabName = 'indices',
          fluidRow(
            box(width = 4,
                selectInput('index_to_pick', 'Choose your Indices', 
                            choices = stock_indices$Symbol, 
                            multiple = TRUE,
                            selected = sample(stock_indices$Symbol, 2))
            ),
            box(width = 4,
                dateRangeInput('date_range_index', 
                               label = 'Select Time Period',
                               start = Sys.Date() - (years_back+5)*365,
                               end = Sys.Date())
            )
          ),
          
          fluidRow(
            tabBox(width = 8,
                   tabPanel(title = 'Price Plot',
                            tags$hr(),
                            plotOutput("index_price_plot")
                   ),
                   tabPanel(title = 'Performance',
                            tableOutput('perf_table3')
                   ),
                   tabPanel(title = 'Download Data',
                            downloadBttn(
                              outputId = "dl_index_csv",
                              label = 'Download CSV File',
                              style = "simple",
                              color = "primary"),
                            downloadBttn(
                              outputId = 'dl_index_xlsx',
                              label =  'Download Excel File',
                              style = "simple",
                              color = "primary")
                   )
            )
          )
  )
}
  