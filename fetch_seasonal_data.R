
source("utils.R")
library_list <- c("rvest","dplyr","stringr")
lapply(library_list, require, character.only=TRUE)

base_url <- "http://www.espncricinfo.com/ci/engine/series/index.html?season=2017;view=season"
initial_url <- "http://www.espncricinfo.com"

get_all_matches_cp <- ".series-summary-block"
all_matches <- base_url %>% 
  read_html() %>% 
  html_nodes(css = get_all_matches_cp) %>% 
  html_node("a") %>% html_attr("href")

# Including Test, ODI, T20Internationals and T20leagues
all_matches <- all_matches[1:49]
all_matches <- data.frame("match_category" = c(rep("Test",6),rep("ODI",14),rep("T20I",7),rep("T20L","4")),
                          "series_link" = all_matches[c(1:27,46:49)], stringsAsFactors = F)


master_table <- c()
for(series_num in 1:nrow(all_matches)){
url_series <- initial_url %+% all_matches$series_link[series_num]

match_text <- url_series %>% 
  read_html() %>% 
  html_nodes(".content_data") %>% html_text()

match_text <- unlist(stringr::str_split(match_text,"\n\n\n\t\t\t"))
match_text <- match_text[seq(2,length(match_text),2)]

for(i in 1:length(match_text)){
  
  if(grepl("D/L method)",match_text[i])){
    match_text_1 <- stringr::str_trim(
      stringr::str_sub(match_text[i],1,
                       data.frame(stringr::str_locate_all(match_text[i],"\\("))[4,1] - 1))  
  } else {
  if(grepl("with [0-9]* ball",match_text[i])){
  match_text_1 <- stringr::str_trim(
    stringr::str_sub(match_text[i],1,
                          data.frame(stringr::str_locate_all(match_text[i],"\\("))[3,1] - 1))
  } else{
    if(nrow(data.frame(stringr::str_locate_all(match_text[i],"\\(")))==1){
    match_text_1 <- stringr::str_trim(
      stringr::str_sub(match_text[i],1,
                       data.frame(stringr::str_locate(match_text[i],"\\(") - 1)[,1]))
    } else {
      match_text_1 <- stringr::str_trim(
        stringr::str_sub(match_text[i],1,
                         data.frame(stringr::str_locate_all(match_text[i],"\\("))[2,1] - 1))
    } 
    }}
 team_won <- stringr::str_trim(stringr::str_sub(match_text_1,1,stringr::str_locate(match_text_1," ")[[1]]))
 first_team <- stringr::str_trim(stringr::str_sub(match_text_1,
                                                  stringr::str_locate(match_text_1,"\n\n")[[1]],
                                                  stringr::str_locate(match_text_1,";")[[1]] -1))
 second_team <- stringr::str_trim(stringr::str_sub(match_text_1,
                                                   stringr::str_locate(match_text_1,";")[[1]] + 1,
                                                   stringr::str_length(match_text_1)))
 match_stats <- data.frame("team_won"=team_won,"first_team"=first_team,"second_team"=second_team,"category"=all_matches$match_category[series_num])
 master_table <- dplyr::bind_rows(master_table,match_stats)
 
}
print("Completed Series # " %+% as.character(series_num) %+% " which is a " %+% all_matches$match_category[series_num] %+% " series.")
}

master_table <- master_table[!is.na(master_table$first_team), ]
write.csv(master_table, "cricket_season_2017.csv", row.names = FALSE)

master_table <- read.csv("cricket_season_2017.csv",stringsAsFactors = F)
# Extracting data from table ----------------------------------------------

# Extracting team names
master_table <- return_team_names(master_table, "first_team", "first_team_name")
master_table <- return_team_names(master_table, "second_team", "second_team_name")


# Extracting scores -------------------------------------------------------
master_table <- return_team_score(master_table, "first_team", "first_team_")
master_table <- return_team_score(master_table, "second_team", "second_team_")


# Fixing team names -------------------------------------------------------

master_table <- dplyr::left_join(master_table, team_won_database, by=NULL)


# Calculating team wise stats for all categories-------------------------------------------------------

# Total matches played
all_teams <- data.frame('team_name' = c(master_table$first_team_name,master_table$second_team_name),
                        'category' = c(master_table$category,master_table$category))
total_matches_played <- data.frame(table(all_teams$team_name, all_teams$category))
names(total_matches_played) <- c('team_name', 'category' ,'total_matches_played')

# Total wins/draws/losses
total_wins <- data.frame(table(master_table$winning_team_name, master_table$category))
names(total_wins)[] <- c('team_name', 'category', 'total_wins')

# Total runs scored # Total runs conceeded 
master_table[is.na(master_table)] <- 0
master_table$total_first_team_runs <- as.numeric(master_table$first_team_ft_runs_1) + as.numeric(master_table$first_team_ft_runs_2)
master_table$total_second_team_runs <- as.numeric(master_table$second_team_ft_runs_1) + as.numeric(master_table$second_team_ft_runs_2)

team_run_stats <- data.frame('team_name' = c(master_table$first_team_name,master_table$second_team_name),
                                'runs_scored' = as.numeric(c(master_table$total_first_team_runs, master_table$total_second_team_runs)),
                                'category' = c(master_table$category,master_table$category),
                                'runs_conceeded' = as.numeric(c(master_table$total_second_team_runs,master_table$total_first_team_runs)),
                                stringsAsFactors = FALSE)

team_run_stats <- team_run_stats %>% group_by(team_name, category) %>% summarise('total_runs_scored' = sum(runs_scored,na.rm = TRUE),
                                                                                         'total_runs_conceeded' = sum(runs_conceeded,na.rm = TRUE))


# Total wickets taken # Total wickets lost
team_wkt_stats <- data.frame("team_name" = c(master_table$first_team_name,master_table$second_team_name),
                             "category" = c(master_table$category, master_table$category),
                             "wickets_given" = as.numeric(c(master_table$first_team_total_wickets,master_table$second_team_total_wickets)),
                             "wickets_taken" = as.numeric(c(master_table$second_team_total_wickets,master_table$first_team_total_wickets)), stringsAsFactors = FALSE)

team_wkt_stats <- team_wkt_stats %>% group_by(team_name, category) %>% summarise('total_wickets_given' = sum(wickets_given)
                                                                                 ,'total_wickets_taken'= sum(wickets_taken))


team_table <- left_join(total_matches_played, total_wins, by=NULL)
team_table <- left_join(team_table, team_run_stats, by=NULL)
team_table <- left_join(team_table, team_wkt_stats, by=NULL)
team_table <- team_table[team_table$total_matches_played != 0,]

write.csv(team_table, "aggregated_results_s2017.csv", row.names = F)
