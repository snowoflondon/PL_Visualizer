library(bslib)
library(shinycssloaders)
library(magrittr)

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

gk_stats <- c('PSxG - GA vs. SoTA vs. Save%') # add more later

theme <- bslib::bs_theme(
  bg = "#ffffff", fg = "#2b2d42",
  primary = "#8d99ae", secondary = "#8d99ae",
  base_font = font_google("Roboto Serif", local = TRUE),
  code_font = c("Courier", "monospace"),
  heading_font = font_google("Roboto Serif", local = TRUE),
  "input-border-color" = "#363537"
)

fluidPage(
  theme = theme,
  titlePanel('PL Visualizer by snowoflondon',
             windowTitle = 'PL Visualizer by snowoflondon - Brian Jungmin Park'),
  tabsetPanel(
    tabPanel(div('Team Stats', style = "color: #2f4f4f"),
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
               DT::dataTableOutput(outputId = 'table') %>% 
                 withSpinner(color="#8d99ae", type = 6))
      ),
      fluidRow(
        column(width = 12,
               plotOutput(outputId = 'plot',
                          height = '600px') %>% 
                 withSpinner(color="#8d99ae", type = 6))
      ),
      fluidRow(
        column(width = 12,
               verticalLayout(
                textOutput(outputId = 'text'),
                a(href = 'https://fbref.com/en/comps/9/Premier-League-Stats',
                  'Source FBRef page'))
      )),
      fluidRow(
        column(width = 12,
               a(href = 'https://github.com/snowoflondon/PL_Visualizer',
                 'Source code'))
      )
    ),
    tabPanel(div('Outfield Player Stats', style = "color: #2f4f4f"),
      fluidRow(
        column(width = 4,
               verticalLayout(
                 textInput(inputId = 'playerSelect',
                           label = 'Player Search',
                           value = 'Harry Kane'),
                 checkboxInput(inputId = 'showLabelPlayers',
                               label = 'Show percentile labels',
                               value = FALSE),
                 actionButton(inputId = 'searchSelect',
                              label = 'Submit'),
                 p('* Goalkeepers currently not supported!'),
                 a(href = 'https://fbref.com/en/comps/9/stats/Premier-League-Stats',
                   'All searchable player names here')
               )),
        column(width = 8,
               plotOutput(outputId = 'playerPlot',
                          height = '1200px') %>% withSpinner(color='#8d99ae', type = 6))
      )),
    tabPanel(div('Goalkeeper Stats', style = "color: #2f4f4f"),
      fluidRow(
        column(width = 4,
               verticalLayout(
                 selectInput(inputId = 'gkSelect',
                           label = 'Visualization',
                           choices = as.list(gk_stats),
                           multiple = FALSE),
                 a(href = 'https://fbref.com/en/comps/9/keepers/Premier-League-Stats',
                   'All searchable player names here')
               )),
        column(width = 8,
               plotOutput(outputId = 'gkPlot',
                          height = '1200px') %>% withSpinner(color = '#8d99ae', type = 6))
      ))
  )
)
