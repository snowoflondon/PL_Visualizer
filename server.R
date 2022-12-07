library(tidyverse)
library(rvest)
library(ggrepel)
library(DT)
library(xml2)
library(RColorBrewer)

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

 url_players <- list(
  'https://fbref.com/en/comps/9/shooting/Premier-League-Stats',
  'https://fbref.com/en/comps/9/passing/Premier-League-Stats',
  'https://fbref.com/en/comps/9/gca/Premier-League-Stats',
  'https://fbref.com/en/comps/9/defense/Premier-League-Stats'
)

# helper function to scrape dynamic tables
pull_xml_table <- function(x){
  df <- read_html(x) %>% xml_find_all('//comment()') %>% xml_text() %>% 
    paste0(collapse = "") %>% read_html %>% xml_find_all("//table") %>% html_table
  df <- df[[1]]
  df <- clean_names(df)
}

df_players <- lapply(url_players, pull_xml_table)
df_players <- df_players %>% lapply(function(x) x[,!duplicated(colnames(x))])
key_stats <- c('Player', 'Pos', 'Gls', 'Sh', 'npxG', 
               'Ast', 'xA', 'PrgDist', 'Att', 'Cmp%',
               'SCA', 'TklW', 'Tkl%', 'Int', 'Clr')
df_players <- df_players %>% lapply(function(x) x %>% select(any_of(key_stats)))
df_players[-(1:2)] <- df_players[-(1:2)] %>% lapply(function(x) x %>%
                                              select(-any_of(c('Sh', 'Att'))))
df_players <- df_players %>% lapply(function(x) x %>% 
                                      filter(Player != 'Player'))
df_new <- df_players[[1]]
for (i in 2:length(df_players)){
  df_new <- df_new %>% inner_join(df_players[[i]])
}
df_new <- df_new[!duplicated(df_new$Player),]
df_new <- df_new %>% 
  mutate(Pos = strsplit(Pos, split = ',') %>% map(~ .[1]) %>% unlist())
df_new <- df_new %>% filter(Pos != 'GK')
df_new <- df_new %>% mutate(across(3:15, as.numeric))
df_medians <- df_new %>% group_by(Pos) %>% 
  summarise(across(3:14, function(x) median(x, na.rm = TRUE))) %>%
  mutate(Player = 'League Median') %>% relocate(Player)                             
                              
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
      unlist() %>% head(1)
    var_x <- strsplit(as.character(input$plotSelect), ' vs. ') %>%
      unlist() %>% tail(1)
    if (input$plotSelect %nin% c('GF vs. xG', 'GA vs. PSxG')){
      p <- ggplot(react_data(), aes(x = UQ(sym(var_x)),
                                    y = UQ(sym(var_y)))) + 
        geom_point(size = 6) +
        geom_point(data = react_data() %>% filter(Squad == input$teamSelect),
                   aes(color = Squad), size = 6) +
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
        theme_bw() +
        theme(text = element_text(size = 14), legend.position = 'none') +
        geom_abline() + 
        labs(title = input$plotSelect, 
             subtitle = paste0(input$teamSelect, ' highlighted'))
    }
    if (input$hideLabels == TRUE){ # checkbox selection 
      p <- p + geom_label_repel(data = react_data() %>%
                                  filter(Squad == input$teamSelect),
                                aes(label = Squad))
    } else {
      p <- p + geom_label_repel(aes(label = Squad))
    }
    print(p)
  })
  
  output$text <- renderText({
    plot_descs[[grep(input$plotSelect, all_vis)]] # description below plot
  })
  
    # server functions for player stats tab
  react_data2 <- eventReactive(input$searchSelect, {
    df_new %>% filter(str_detect(Player, input$playerSelect)) %>% 
      bind_rows(df_medians) %>% select(-Gls) %>%
      group_by(Pos) %>% filter(n() != 1) %>% ungroup()
  })
  
    output$playerPlot <- renderPlot({
    validate( # in case player search is invalid, display error message
      need(expr = nrow(react_data2()) == 2,
           message = 'Search error! \nCheck for unique, valid name search 
           \nFor all searchable names, visit the link on the sidebar'))
    q <-  react_data2() %>% select(-Pos) %>%
        pivot_longer(-Player, names_to = 'Metric', values_to = 'Values') %>%
        mutate(Metric = case_when(Metric == 'Sh' ~ 'Shots',
                                  Metric == 'npxG' ~ 'Non-penalty xG',
                                  Metric == 'Ast' ~ 'Assists',
                                  Metric == 'xA' ~ 'xA',
                                  Metric == 'PrgDist' ~ 'Prg. Distance',
                                  Metric == 'Att' ~ 'Passes Attempted',
                                  Metric == 'Cmp%' ~ 'Pass Completion %',
                                  Metric == 'SCA' ~ 'Shot Creating Actions',
                                  Metric == 'TklW' ~ 'Tackles Won',
                                  Metric == 'Tkl%' ~ 'Tackles Won %',
                                  Metric == 'Int' ~ 'Interceptions',
                                  Metric == 'Clr' ~ 'Clearances')) %>%
        mutate(Metric = factor(Metric,
                               levels = c('Shots', 'Non-penalty xG', 'Assists', 
                                          'xA', 'Prg. Distance', 
                                          'Passes Attempted',
                                          'Pass Completion %',
                                          'Shot Creating Actions',
                                          'Tackles Won', 'Tackles Won %',
                                          'Interceptions', 'Clearances'))) %>%
        ggplot(aes(x = Player, y = Values)) + 
        geom_bar(stat = 'identity', aes(fill = Player), alpha = .6) +
        facet_wrap(~Metric, scales = 'free') + theme_bw() + 
        theme(legend.position = 'bottom', text = element_text(size = 14),
              axis.text.x = element_blank()) +
        xlab('') + ylab('') +
        labs(title = paste0(react_data2()$Player, 
                            ' vs. League Median amongst ',
                            react_data2()$Pos),
             subtitle = 'Premier League') + 
      scale_fill_brewer(palette = 'Set1')
    print(q)
  })
}
