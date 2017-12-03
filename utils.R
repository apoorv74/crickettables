check_string <- function(x) {
  stopifnot(is.character(x), length(x) == 1)
}

`%+%` <- function(lhs, rhs) {
  check_string(lhs)
  check_string(rhs)
  paste0(lhs, rhs)
}

return_team_names <- function(dataframe_obj, column_name, new_col_name){
  temp <- data.frame(stringr::str_locate(dataframe_obj[,column_name],"[0-9]"))[,1]
  temp <- data.frame(stringr::str_sub(dataframe_obj[,column_name],1,temp))
  names(temp) <- new_col_name
  temp[,new_col_name] <- stringr::str_trim(stringr::str_replace_all(temp[,new_col_name],"[0-9]",""))
  dataframe_obj <- cbind(dataframe_obj,temp)
  return(dataframe_obj)
}

dataframe_obj <- master_table
score_col <- "second_team"
name_prefix <- "second_team_"

return_team_score <- function(dataframe_obj, score_col, name_prefix){
temp <- dataframe_obj[, score_col]
score_table <- c()
for(i in 1:length(temp)){
  if(dataframe_obj[i,"category"] == "Test"){
  number_pos <- data.frame(stringr::str_locate_all(temp[i], "[\\d]+"))
  temp_1 <- data.frame(stringr::str_sub(temp[i],number_pos$start[1],number_pos$end[1]),
                       stringr::str_sub(temp[i],number_pos$start[2],number_pos$end[2]))
  
  names(temp_1)[] <- c("ft_runs_1","ft_runs_2")
  if(nrow(number_pos) >= 3){
    temp[i] <- str_sub(temp[i],1,unlist(str_locate(temp[i],"\\(")[1]))
    temp_1 <- cbind(temp_1,data.frame(stringr::str_sub(temp[i],number_pos$start[3],number_pos$end[3])))
    names(temp_1)[ncol(temp_1)] <- "ft_wickets"
  }
  } else {
    if(!is.na(unlist(str_locate(temp[i],"\\(")[1]))){
    temp[i] <- str_sub(temp[i],1,unlist(str_locate(temp[i],"\\(")[1]))
    }
    number_pos <- data.frame(stringr::str_locate_all(temp[i], "[\\d]+"))
    temp_1 <- data.frame(stringr::str_sub(temp[i],number_pos$start[1],number_pos$end[1]))
    names(temp_1)[] <- "ft_runs_1"
    if(nrow(number_pos) == 2){
      temp_1 <- cbind(temp_1,data.frame(stringr::str_sub(temp[i],number_pos$start[2],number_pos$end[2])))
      names(temp_1)[ncol(temp_1)]<- "ft_wickets"
    }
  }
  score_table <- dplyr::bind_rows(score_table,temp_1)
}

dataframe_obj <- cbind(dataframe_obj, score_table)
dataframe_obj[,"wickets_1"] <- dataframe_obj[,"ft_wickets"]
dataframe_obj[dataframe_obj$category == "Test","wickets_1"] <- 10
dataframe_obj[dataframe_obj$category != "Test","wickets_1"] <- 
  dataframe_obj[dataframe_obj$category != "Test","ft_wickets"]
names(dataframe_obj)[which(names(dataframe_obj)=="ft_wickets")] <- "wickets_2"
dataframe_obj$wickets_2[is.na(dataframe_obj$wickets_2)] <- 0
dataframe_obj$wickets_2[dataframe_obj$wickets_2 == ""] <- 0
dataframe_obj$wickets_2[dataframe_obj$wickets_2 == 0 & dataframe_obj$category=="Test"]<- 10
dataframe_obj$wickets_2[dataframe_obj$category!="Test"]<- 0
dataframe_obj$ft_runs_2[dataframe_obj$category!="Test"]<- 0
dataframe_obj$wickets_1[is.na(dataframe_obj$wickets_1) & dataframe_obj$category!="Test"]<- 10
col_index <- which(names(dataframe_obj) %in% c('ft_runs_1', 'ft_runs_2', 'wickets_1', 'wickets_2'))
names(dataframe_obj)[col_index] <- paste0(name_prefix,names(dataframe_obj)[col_index])
return(dataframe_obj)
}
