library(rvest)
base_url <- "http://www.espncricinfo.com/ci/engine/series/index.html?season=2017;view=season"
get_all_matches_cp <- ".series-summary-block"

all_matches <- base_url %>% 
  read_html() %>% 
  html_nodes(css = get_all_matches_cp) %>% 
  html_node("a") %>% html_attr("href")

initial_url <- "http://www.espncricinfo.com"
master_table <- c()
for(series_num in 1:6){
url_series <- paste0(initial_url,all_matches[series_num])

match_text <- url_series %>% 
  read_html() %>% 
  html_nodes(".content_data") %>% html_text()

match_text <- unlist(stringr::str_split(match_text,"\n\n\n\t\t\t"))
match_text <- match_text[seq(2,length(match_text),2)]

for(i in 1:length(match_text)){
  match_text_1 <- stringr::str_trim(
    stringr::str_sub(match_text[i],1,
                          data.frame(stringr::str_locate(match_text[i],"\\(") - 1)[,1]))
 team_won <- stringr::str_trim(stringr::str_sub(match_text_1,1,stringr::str_locate(match_text_1," ")[[1]]))
 first_team <- stringr::str_trim(stringr::str_sub(match_text_1,
                                                  stringr::str_locate(match_text_1,"\n\n")[[1]],
                                                  stringr::str_locate(match_text_1,";")[[1]] -1))
 second_team <- stringr::str_trim(stringr::str_sub(match_text_1,
                                                   stringr::str_locate(match_text_1,";")[[1]] + 1,
                                                   stringr::str_length(match_text_1)))
 match_stats <- data.frame("team_won"=team_won,"first_team"=first_team,"second_team"=second_team)
 master_table <- dplyr::bind_rows(master_table,match_stats)
}
}
