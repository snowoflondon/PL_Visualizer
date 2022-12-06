library(shinythemes)

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
                         multiple = FALSE)
           )),
    column(width = 8,
           DT::dataTableOutput(outputId = 'table'))
  ),
  fluidRow(
    column(width = 12,
           plotOutput(outputId = 'plot',
                      height = '600px'))
  ),
  fluidRow(
    column(width = 12,
           verticalLayout(
            textOutput(outputId = 'text'),
            a(href = 'https://fbref.com/en/comps/9/Premier-League-Stats',
              'Source FBRef page'))
  ))
)
