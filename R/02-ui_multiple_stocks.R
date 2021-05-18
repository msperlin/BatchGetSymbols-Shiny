ui_multiple_stocks <- function(){
  tabItem(tabName = 'multiple_stocks',
          fluidRow(
            box(width = 3,
                selectInput('index_multiple', 'Choose your Collection', 
                            choices = available_collections, 
                            selected = sample(available_collections, 1))),
            box(width = 3,
                uiOutput('multiple_ticker_ui')
            ),
            box(width = 3,
                dateRangeInput('date_range_multiple', 
                               label = 'Select Time Period',
                               start = Sys.Date() - years_back*365,
                               end = Sys.Date())
            )
          ),
          
          fluidRow(
            tabBox(width = 9,
                   tabPanel(title = 'Price Plot',
                            tags$hr(),
                            plotOutput("multiple_price_plot")
                   ),
                   tabPanel(title = 'Performance',
                            tableOutput('perf_table2')
                   ),
                   tabPanel(title = 'Download Data',
                            downloadBttn(
                              outputId = "dl_multiple_csv",
                              label = 'Download CSV File',
                              style = "simple",
                              color = "primary"),
                            downloadBttn(
                              outputId = 'dl_multiple_xlsx',
                              label =  'Download Excel File',
                              style = "simple",
                              color = "primary")
                   )
            )
          )
  )
}
  