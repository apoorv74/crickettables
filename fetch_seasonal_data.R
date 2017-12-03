
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






