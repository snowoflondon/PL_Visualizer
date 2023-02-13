library(shinythemes)
library(shinycssloaders)

squads <- c('Arsenal', 'Manchester City', 'Newcastle Utd',
            'Tottenham', 'Manchester Utd', 'Liverpool',
            'Brighton', 'Chelsea', 'Fulham',
            'Brentford', 'Crystal Palace', 'Aston Villa',
            'Leicester City', 'Bournemouth', 'Leeds United',
            'West Ham', 'Everton', 'Nott\'ham Forest',
            'Southampton', 'Wolves')
vis <- c(
  'GF vs. xG',
  'npxG vs. xGA',
  'GA vs. SoTA',
  'GA vs. PSxG',
  'npxG/Sh vs. Sh/90'
)

fluidPage(
  theme = shinytheme('flatly'),
  titlePanel('PL Visualizer'),
  tabsetPanel(
    tabPanel('Team Stats',
      fluidRow(
        column(width = 4,
               verticalLayout(
                 selectInput(inputId = 'teamSelect',
                             label = 'Team Selection',
                             choices = as.list(squads),
                             multiple = FALSE),
                 selectInput(inputId = 'plotSelect',
                             label = 'Visualization',
                             choices = as.list(vis),
                             multiple = FALSE),
                 checkboxInput(inputId = 'hideLabels',
                               label = 'Hide non-selected squad labels',
                               value = FALSE,
                               width = '500px')
               )),
        column(width = 8,
               DT::dataTableOutput(outputId = 'table')) %>%
                  withSpinner(color="#0dc5c1"))
      ),
      fluidRow(
        column(width = 12,
               plotOutput(outputId = 'plot',
                          height = '600px')) %>%
                  withSpinner(color="#0dc5c1"))
      ),
      fluidRow(
        column(width = 12,
               verticalLayout(
                textOutput(outputId = 'text'),
                a(href = 'https://fbref.com/en/comps/9/Premier-League-Stats',
                  'Source FBRef page'))
      ))
    ),
    tabPanel('Outfield Player Stats',
      fluidRow(
        column(width = 4,
               verticalLayout(
                 textInput(inputId = 'playerSelect',
                           label = 'Player Search',
                           value = 'Harry Kane'),
                 actionButton(inputId = 'searchSelect',
                              label = 'Submit'),
                 p('* Goalkeepers currently not supported!'),
                 a(href = 'https://fbref.com/en/comps/9/stats/Premier-League-Stats',
                   'All searchable players names here')
               )),
        column(width = 8,
               plotOutput(outputId = 'playerPlot',
                          height = '1200px') %>% withSpinner(color="#0dc5c1"))
      )),
     tabPanel('Goalkeeper Stats',
      fluidRow(
        column(width = 4,
               verticalLayout(
                 selectInput(inputId = 'gkSelect',
                           label = 'Goalkeeper Search',
                           choices = as.list(gk_stats),
                           multiple = FALSE),
                 a(href = 'https://fbref.com/en/comps/9/keepers/Premier-League-Stats',
                   'All searchable player names here')
               )),
        column(width = 8,
               plotOutput(outputId = 'gkPlot',
                          height = '1200px') %>% withSpinner(color="#0dc5c1"))
      ))
  )
)
