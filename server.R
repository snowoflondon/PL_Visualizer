library(tidyverse)
library(rvest)
library(ggrepel)
library(DT)
library(xml2)
library(RColorBrewer)

url <- 'https://fbref.com/en/comps/9/Premier-League-Stats'
html <- read_html(url)
df_long <- html %>% html_elements('table') %>% html_table()
df_long <- df_long[seq(1, length(df_long), 2)]
clean_names <- function(x){
  names(x) <- x[1,]
  x <- x[-1,]
  return(x)
}
df_long[-1] <- df_long[-1] %>% map(~ clean_names(.))
df_long <- df_long %>% lapply(function(x) x[,!duplicated(colnames(x))])
df_long[[2]] <- df_long[[2]] %>% left_join(
  df_long[[1]] %>% select(Squad, xGA), by = 'Squad'
)
#df_long[[1]] <- df_long[[1]] %>% select(-c(Rk, `Last 5`, 
                                           #Attendance, `Top Team Scorer`,
                                           #Goalkeeper, Notes))

df_long[[1]] <- df_long[[1]] %>% select(-c(Rk,
                                           Attendance, `Top Team Scorer`,
                                           Goalkeeper, Notes))

df_long <- df_long %>% map(~ mutate(., across(2:ncol(.), as.numeric)))

headings <- html %>% html_elements('h2') %>% as.character()
headings <- headings[!duplicated(headings)]
headings <- headings[1:length(df_long)]

headings <- headings %>% map(~ strsplit(., split = '>|<') %>%
                               unlist() %>% .[3] %>% str_trim()) %>% unlist()

#all_squads <- df_long[[1]] %>% pull(Squad)
all_vis <- c(
  'GF vs. xG',
  'npxG vs. xGA',
  'GA vs. SoTA',
  'GA vs. PSxG',
  'npxG/Sh vs. Sh/90'
)
column_keys <- list(
  c('Squad', 'MP', 'W', 'D', 'L', 'GF', 'GA', 'Pts', 'xG', 'xGA'),
  c('Squad', 'MP', 'Gls', 'xG', 'npxG', 'xGA', 'xAG', 'npxG+xAG'),
  c('Squad', 'MP', 'GA', 'GA90', 'SoTA', 'Save%', 'CS'),
  c('Squad', 'GA', 'PKA', 'PSxG', 'PSxG/SoT', 'PSxG+/-'),
  c('Squad', 'Gls', 'Sh', 'SoT', 'SoT%', 'Sh/90', 'SoT/90', 'G/Sh',
    'xG', 'npxG', 'npxG/Sh', 'G-xG')
)
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
)

`%nin%` <- Negate(`%in%`)

url_players <- list(
  'https://fbref.com/en/comps/9/shooting/Premier-League-Stats',
  'https://fbref.com/en/comps/9/passing/Premier-League-Stats',
  'https://fbref.com/en/comps/9/gca/Premier-League-Stats',
  'https://fbref.com/en/comps/9/defense/Premier-League-Stats'
)

url_gk <- list('https://fbref.com/en/comps/9/keepers/Premier-League-Stats',
               'https://fbref.com/en/comps/9/keepersadv/Premier-League-Stats')

pull_xml_table <- function(x){
  df <- read_html(x) %>% xml_find_all('//comment()') %>% xml_text() %>% 
    paste0(collapse = "") %>% read_html %>% xml_find_all("//table") %>% html_table
  df <- df[[1]]
  df <- clean_names(df)
}

function(input, output){

  react_data <- reactive({
    df_long[[grep(as.character(input$plotSelect), all_vis)]]
  })
  
  output$table <- DT::renderDataTable({
      df_long[[grep(input$plotSelect, all_vis)]] %>% 
      select(any_of(column_keys[[
          grep(as.character(input$plotSelect), all_vis)
        ]]))
  })
  
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
        theme_classic() +
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
        theme_classic() +
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
        theme_classic() +
        theme(text = element_text(size = 14), legend.position = 'none') +
        geom_abline() + 
        labs(title = input$plotSelect, 
             subtitle = paste0(input$teamSelect, ' highlighted'))
    }
    if (input$hideLabels == TRUE){
      p <- p + geom_label_repel(data = react_data() %>%
                                  filter(Squad == input$teamSelect),
                                aes(label = Squad),
                                size = 6)
    } else {
      p <- p + geom_label_repel(aes(label = Squad), size = 6)
    }
    print(p)
  })
  
  output$text <- renderText({
    plot_descs[[grep(input$plotSelect, all_vis)]]
  })
  
  react_data2 <- eventReactive(input$searchSelect, {
    df_players <- lapply(url_players, pull_xml_table)
    df_players <- df_players %>% lapply(function(x) x[,!duplicated(colnames(x))])
    key_stats <- c('Player', 'Pos', 'Gls', 'Sh', 'npxG', 
                   'Ast', 'xA', 'PrgDist','TotDist', 
                   'Att', 'Cmp%', 'PrgP', 'PPA',
                   'SCA90', 'GCA90', 'TklW', 'Tkl%', 'Int', 'Clr')
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
    df_new <- df_new %>% mutate(across(3:19, as.numeric))
    validate(need(grep(input$playerSelect, df_new$Player, ignore.case = TRUE) %>%
                    length() == 1,
                  message = 'Search error! \nCheck for unique, valid name search 
           \nFor all searchable names, check the link on the sidebar'))
    pos_s <- df_new %>% filter(str_detect(Player, regex(input$playerSelect, ignore_case = TRUE))) %>% 
      pull(Pos)
    df_new <- df_new %>% filter(Pos == pos_s) %>%
      mutate(across(where(is.numeric), ~ ntile(.x, 100))) %>%
      filter(str_detect(Player, regex(input$playerSelect, ignore_case = TRUE)))
    player_s <- df_new %>% pull(Player)
    df_new <- df_new %>% select(where(is.numeric)) %>%
      pivot_longer(everything(), names_to = 'Metric', values_to = 'Percentile') %>%
      mutate(Category = c(rep('Goal Scoring', 3), rep('Passing', 8),
                     rep('Goal Creating Actions', 2), rep('Defense', 4))) %>%
      mutate(Player = player_s) %>% mutate(Position = pos_s)
    return(df_new)
  })
  
  output$playerPlot <- renderPlot({
    validate(
      need(expr = nrow(react_data2()) == 17,
           message = 'Search error! \nCheck for unique, valid name search 
           \nFor all searchable names, check the link on the sidebar'))
    q <-  react_data2() %>%
        mutate(Metric = case_when(Metric == 'Sh' ~ 'Shots',
                                  Metric == 'Gls' ~ 'Goals',
                                  Metric == 'npxG' ~ 'Non-penalty xG',
                                  Metric == 'Ast' ~ 'Assists',
                                  Metric == 'xA' ~ 'xA',
                                  Metric == 'PrgDist' ~ 'Prg. Distance',
                                  Metric == 'TotDist' ~ 'Total Distance',
                                  Metric == 'Att' ~ 'Passes Attempted',
                                  Metric == 'Cmp%' ~ 'Pass Completion %',
                                  Metric == 'PrgP' ~ 'Progressive Passes',
                                  Metric == 'PPA' ~ 'Passes into \nPenalty Area',
                                  Metric == 'SCA90' ~ 'Shot Creating \nActions/90',
                                  Metric == 'GCA90' ~ 'Goal Creating \nActions/90',
                                  Metric == 'TklW' ~ 'Tackles Won',
                                  Metric == 'Tkl%' ~ 'Tackles Won %',
                                  Metric == 'Int' ~ 'Interceptions',
                                  Metric == 'Clr' ~ 'Clearances')) %>%
        ggplot(aes(x = fct_inorder(Metric), y = Percentile)) + 
        geom_bar(stat = 'identity', aes(fill = Category), alpha = .7) +
        coord_polar() + theme_minimal() +
        theme(text = element_text(size = 18), legend.position = 'bottom',
              axis.text.y = element_blank()) +
        xlab('') + ylab('') +
        scale_fill_brewer(palette = 'Set1') +
        labs(title = react_data2() %>% pull(Player) %>% unique(),
             subtitle = paste0('Percentile among other ',
                               react_data2() %>% pull(Position) %>% unique(),
                               ' - Premier League (2023-2024)'))
    if (input$showLabelPlayers == TRUE){
      q <- q + geom_text(aes(label = Percentile, y = Percentile + 3), 
                         size = 5)
    }
    print(q)
  })
  
  react_data3 <- eventReactive(input$gkSelect, {
    df_gk <- lapply(url_gk, pull_xml_table)
    df_gk <- df_gk %>% lapply(function(x) x[,!duplicated(colnames(x))])
    gk_key <- c('Player', 'GA', 'Save%', 'PSxG', 'PSxG/SoT') # add more later
    df_gk <- df_gk %>% lapply(function(x) x %>% select(any_of(gk_key)) %>%
                                filter(Player != 'Player'))
    df_gk2 <- df_gk[[1]] %>% inner_join(df_gk[[2]]) %>%
      mutate(across(-1, as.numeric)) %>%
      mutate(`PSxG - GA` = PSxG-GA)
    return(df_gk2)
  })
  
  output$gkPlot <- renderPlot({
    if (input$gkSelect == 'PSxG - GA vs. SoTA vs. Save%'){
      q <- react_data3() %>% ggplot(aes(x = `PSxG - GA`, y = `Save%`)) +
        geom_point(size = 6) +
        geom_vline(xintercept = median(react_data3() %>% pull(`PSxG - GA`)),
                   linetype = 'dashed') + 
        geom_hline(yintercept = median(react_data3() %>% pull(`Save%`)), 
                   linetype = 'dashed') + 
        geom_label_repel(aes(label = Player), size = 6) + 
        theme_classic() +
        xlab('Post shot expected goals minus goals conceded') + 
        ylab('Save percentage') +
        theme(text = element_text(size = 16)) +
        labs(title = 'PSxG-GA vs. Save%',
             subtitle = 'Premier League Goalkeepers')
    }
    if (input$gkSelect == 'PSxG/SoT vs. Save%'){
      q <- react_data3() %>% ggplot(aes(x = `PSxG/SoT`, y = `Save%`)) +
        geom_point(size = 6) +
        geom_vline(xintercept = median(react_data3() %>% pull(`PSxG/SoT`)),
                   linetype = 'dashed') + 
        geom_hline(yintercept = median(react_data3() %>% pull(`Save%`)), 
                   linetype = 'dashed') + 
        geom_label_repel(aes(label = Player), size = 6) + 
        theme_classic() +
        xlab('Post shot expected goals per shots on target') + 
        ylab('Save percentage') +
        theme(text = element_text(size = 16)) +
        labs(title = 'PSxG/SoT vs. Save%',
             subtitle = 'Premier League Goalkeepers')
    }
    print(q)
  })
}