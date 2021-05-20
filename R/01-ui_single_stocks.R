ui_single_stocks <- function(){
  tabItem(tabName = "single_stocks",
          fluidRow(
            infoBox('Number of Collections', 
                    color =  'light-blue',
                    value = n_distinct(available_collections),
                    icon = icon('landmark'), 
                    width = 3),
            uiOutput('infobox_nstocks'),
            uiOutput('infobox_nprices')
            
          ),
          fluidRow(
            box(width = 3,
                selectInput('collection_single', 'Choose your Collection', 
                            choices = available_collections, 
                            selected = sample(available_collections, 1))),
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
                            tableOutput('perf_table1')
                   ),
                   tabPanel(title = 'Download Data',
                            downloadBttn(
                              outputId = "dl_single_csv",
                              label = 'Download CSV File',
                              style = "simple",
                              color = "primary"),
                            downloadBttn(
                              outputId = 'dl_single_xlsx',
                              label =  'Download Excel File',
                              style = "simple",
                              color = "primary")
                   )
            )
          ),
          
  )
}
  