library(tidyverse)
library(rvest)
library(ggrepel)
library(DT)

# fetch data from fbref
url <- 'https://fbref.com/en/comps/9/Premier-League-Stats'
html <- read_html(url)
df_long <- html %>% html_elements('table') %>% html_table() # make a list of data tables
df_long <- df_long[seq(1, length(df_long), 2)] # remove duplicate tables
clean_names <- function(x){
  names(x) <- x[1,]
  x <- x[-1,]
  return(x)
}
df_long[-1] <- df_long[-1] %>% map(~ clean_names(.))
df_long <- df_long %>% lapply(function(x) x[,!duplicated(colnames(x))]) # remove per 90 stats
df_long[[2]] <- df_long[[2]] %>% left_join(
  df_long[[1]] %>% select(Squad, xGA), by = 'Squad'
)
df_long[[1]] <- df_long[[1]] %>% select(-c(Rk, `Last 5`, 
                                           Attendance, `Top Team Scorer`,
                                           Goalkeeper, Notes))

df_long <- df_long %>% map(~ mutate(., across(2:ncol(.), as.numeric))) # identify numeric variables

headings <- html %>% html_elements('h2') %>% as.character() # extract data table categories
headings <- headings[!duplicated(headings)]
headings <- headings[1:length(df_long)]

headings <- headings %>% map(~ strsplit(., split = '>|<') %>%
                               unlist() %>% .[3] %>% str_trim()) %>% unlist() # format headings

#all_squads <- df_long[[1]] %>% pull(Squad)
all_vis <- c(
  'GF vs. xG',
  'npxG vs. xGA',
  'GA vs. SoTA',
  'GA vs. PSxG',
  'npxG/Sh vs. Sh/90'
) # key for plot selection
                              
column_keys <- list(
  c('Squad', 'MP', 'W', 'D', 'L', 'GF', 'GA', 'Pts', 'xG', 'xGA'),
  c('Squad', 'MP', 'Gls', 'xG', 'npxG', 'xGA', 'xAG', 'npxG+xAG'),
  c('Squad', 'MP', 'GA', 'GA90', 'SoTA', 'Save%', 'CS'),
  c('Squad', 'GA', 'PKA', 'PSxG', 'PSxG/SoT', 'PSxG+/-'),
  c('Squad', 'Gls', 'Sh', 'SoT', 'SoT%', 'Sh/90', 'SoT/90', 'G/Sh',
    'xG', 'npxG', 'npxG/Sh', 'G-xG')
) # columns to select for visualization
                              
plot_descs <- list(
  'The GF (goals scored) vs. xG (expected goals) plot shows the \n 
  over- or under-performance of goal scoring relative to expectation. \n
  Diagonal line indicates the boundary where the two metrics are equal, \n
  as in the team has perfectly matched the xG in their goal output.',
  'The npxG (non-penalty xG) vs. xGA (expected goals allowed) plot shows \n
  the expected goal differential. Dominant teams are reflected in high \n
  npxG and low xGA. Dotted lines indicate league medians.',
  'The GA (goals allowed) vs. SoTA (shots on target against) plot shows \n
  how permissive the team defense is - measured by shots conceded - \n
  and goalkeeping performance. Dotted lines indicate league medians',
  'The GA (goals allowed) vs. PSxG (post-shot expected goals) plot shows \n
  goalkeeping performance - positive difference between PSxG and GA indicates \n
  that the keeper has prevented more goals than expected. Diagonal line \n
  indicates the boundary where the two metrics are equal.',
  'The npxG/Sh (non-penalty xG per shot) vs. shots per 90 plot shows \n
  the shot volume relative to quality of shots taken. High npxG/Sh and high \n
  Sh/90 indicates teams taking a high volumne of high-chance shots. \n
  Dotted lines indicate league medians'
) # brief descriptions 

`%nin%` <- Negate(`%in%`) # helper function for negation of %in% 

# core server function
function(input, output){

  react_data <- reactive({
    df_long[[grep(as.character(input$plotSelect), all_vis)]]
  }) # reactive object to select single data table based on user selection
  
  output$table <- DT::renderDataTable({
      df_long[[grep(input$plotSelect, all_vis)]] %>% 
      select(any_of(column_keys[[
          grep(as.character(input$plotSelect), all_vis)
        ]]))
  }) # output live updating table
  
  output$plot <- renderPlot({
    var_y <- strsplit(as.character(input$plotSelect), ' vs. ') %>%
      unlist() %>% head(1) # define y variable for ggplot
    var_x <- strsplit(as.character(input$plotSelect), ' vs. ') %>%
      unlist() %>% tail(1) # define x variable for ggplot
    if (input$plotSelect %nin% c('GF vs. xG', 'GA vs. PSxG')){
      p <- ggplot(react_data(), aes(x = UQ(sym(var_x)),
                                    y = UQ(sym(var_y)))) + 
        geom_point(size = 6) +
        geom_point(data = react_data() %>% filter(Squad == input$teamSelect),
                   aes(color = Squad), size = 6) +
        geom_label_repel(aes(label = Squad)) +
        theme_bw() +
        theme(text = element_text(size = 14), legend.position = 'none') + 
        geom_vline(
          xintercept =  react_data() %>% 
            select(UQ(sym(var_x))) %>% pull() %>% median(),
          linetype = 'dashed'
        ) + 
        geom_hline(
          yintercept =  react_data() %>% 
            select(UQ(sym(var_y))) %>% pull() %>% median(),
          linetype = 'dashed'
        ) + 
        labs(title = input$plotSelect, 
             subtitle = paste0(input$teamSelect, ' highlighted'))
    }
    if (input$plotSelect == 'GF vs. xG'){
      p <- ggplot(react_data(), aes(x = GF, y = xG)) + 
        geom_point(size = 6) +
        geom_point(data = react_data() %>% filter(Squad == input$teamSelect),
                   aes(color = Squad), size = 6) +
        geom_label_repel(aes(label = Squad)) + 
        theme_bw() +
        theme(text = element_text(size = 14), legend.position = 'none') +
        geom_abline() + 
        labs(title = input$plotSelect, 
             subtitle = paste0(input$teamSelect, ' highlighted'))
    }
    if (input$plotSelect == 'GA vs. PSxG'){
      p <- ggplot(react_data(), aes(x = GA, y = PSxG)) + 
        geom_point(size = 6) +
        geom_point(data = react_data() %>% filter(Squad == input$teamSelect),
                   aes(color = Squad), size = 6) +
        geom_label_repel(aes(label = Squad)) + 
        theme_bw() +
        theme(text = element_text(size = 14), legend.position = 'none') +
        geom_abline() + 
        labs(title = input$plotSelect, 
             subtitle = paste0(input$teamSelect, ' highlighted'))
    }
    print(p)
  })
  
  output$text <- renderText({
    plot_descs[[grep(input$plotSelect, all_vis)]] # description below plot
  })
}
