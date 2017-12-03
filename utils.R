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
score_col <- "first_team"
# name_prefix <- "second_team_"

return_team_score <- function(dataframe_obj, score_col, name_prefix){
temp <- dataframe_obj[, score_col]
score_table <- c()
for(i in 1:length(temp)){
  if(dataframe_obj[i,"category"] == "Test"){
    if(!str_detect(temp[i]," and ")){
      number_pos <- data.frame(stringr::str_locate_all(temp[i], "[\\d]+"))
      temp_1 <- data.frame(stringr::str_sub(temp[i],number_pos$start[1],number_pos$end[1]),
                           stringr::str_sub(temp[i],number_pos$start[2],number_pos$end[2]))
      names(temp_1)[] <- c("ft_runs_1", "ft_wickets")
    } else {
      number_pos <- data.frame(stringr::str_locate_all(temp[i], "[\\d]+"))
      temp_1 <- data.frame(stringr::str_sub(temp[i],number_pos$start[1],number_pos$end[1]),
                       stringr::str_sub(temp[i],number_pos$start[2],number_pos$end[2]))
  
  names(temp_1)[] <- c("ft_runs_1","ft_runs_2")
  if(nrow(number_pos) > 3){
    temp[i] <- str_sub(temp[i],1,unlist(str_locate(temp[i],"\\(")[1])) 
    number_pos <- data.frame(stringr::str_locate_all(temp[i], "[\\d]+"))
  }
  if(nrow(number_pos) == 3){
    temp_1 <- cbind(temp_1,data.frame(stringr::str_sub(temp[i],number_pos$start[3],number_pos$end[3])))
    names(temp_1)[ncol(temp_1)] <- "ft_wickets"
  }
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
  temp_1 <- lapply(temp_1, as.character)
  score_table <- dplyr::bind_rows(score_table,temp_1)
}

dataframe_obj <- cbind(dataframe_obj, score_table)
dataframe_obj$total_runs <- as.numeric(dataframe_obj$ft_runs_1) + as.numeric(dataframe_obj$ft_runs_2)
dataframe_obj$total_wickets[is.na(dataframe_obj$total_runs)] <- dataframe_obj$ft_wickets[is.na(dataframe_obj$total_runs)]
dataframe_obj$total_wickets[!is.na(dataframe_obj$total_runs) & is.na(dataframe_obj$ft_wickets)] <- 20
dataframe_obj$total_wickets[is.na(dataframe_obj$total_runs) & is.na(dataframe_obj$ft_wickets)] <- 10
dataframe_obj$total_wickets[!is.na(dataframe_obj$total_runs) & !is.na(dataframe_obj$ft_wickets)] <- 10 + 
  as.numeric(dataframe_obj$ft_wickets[!is.na(dataframe_obj$total_runs) & !is.na(dataframe_obj$ft_wickets)])
dataframe_obj$ft_wickets <- NULL
dataframe_obj$total_runs <- NULL
# if((!is.na(dataframe_obj[,"ft_runs_1"])) && (!is.na(dataframe_obj[,"ft_runs_2"]))){
#   
#     dataframe_obj[!is.na(dataframe_obj$ft_wickets),"total_wickets"] <- 10 + as.numeric(dataframe_obj[!is.na(dataframe_obj[,"ft_wickets"]),"ft_wickets"])
#     dataframe_obj[is.na(dataframe_obj$ft_wickets),"total_wickets"] <- 10 + 10
#   } else {
#   # if((!is.na(dataframe_obj[,"ft_runs_1"])) || (!is.na(dataframe_obj[,"ft_runs_2"]))) {
#     if(is.na(dataframe_obj[,"ft_wickets"])){
#       dataframe_obj[is.na(dataframe_obj[,"ft_wickets"]),"total_wickets"]  <- 10
#     } else {
#       dataframe_obj[!is.na(dataframe_obj[,"ft_wickets"]),"total_wickets"] <- dataframe_obj[!is.na(dataframe_obj[,"ft_wickets"]),"ft_wickets"]  
#     }
#   }

# dataframe_obj[,"wickets_1"] <- dataframe_obj[,"ft_wickets"]
# dataframe_obj[dataframe_obj$category == "Test" & is.na(dataframe_obj$ft_wickets),"wickets_1"] <- 10
# dataframe_obj[dataframe_obj$category == "Test" & is.na(dataframe_obj$ft_wickets),"ft_wickets"] <- ""
# dataframe_obj[dataframe_obj$category != "Test","wickets_1"] <- 
#   dataframe_obj[dataframe_obj$category != "Test","ft_wickets"]
# names(dataframe_obj)[which(names(dataframe_obj)=="ft_wickets")] <- "wickets_2"
# dataframe_obj$wickets_2[is.na(dataframe_obj$wickets_2)] <- 0
# dataframe_obj$wickets_2[dataframe_obj$wickets_2 == ""] <- 0
# dataframe_obj$wickets_2[dataframe_obj$wickets_2 == 0 & dataframe_obj$category=="Test" & !is.na(dataframe_obj$ft_runs_2)]<- 10
# dataframe_obj$wickets_2[dataframe_obj$category!="Test"]<- 0
# dataframe_obj$ft_runs_2[dataframe_obj$category!="Test"]<- 0
# dataframe_obj$wickets_1[is.na(dataframe_obj$wickets_1) & dataframe_obj$category!="Test"]<- 10
col_index <- which(names(dataframe_obj) %in% c('ft_runs_1', 'ft_runs_2', 'total_wickets'))
names(dataframe_obj)[col_index] <- paste0(name_prefix,names(dataframe_obj)[col_index])
return(dataframe_obj)
}

team_won_database <- data.frame("winning_team_name" = c("Pakistan","West Indies","England","South Africa","Sri Lanka","India",
                                         "Bangladesh","Australia","Namibia","Hong Kong","Netherlands",
                                         "United Arab Emirates","Kenya","Scotland","PapuaNew Guinea","Nepal","New Zealand",
                                         "No","Afghanistan","Zimbabwe","Sunrisers Hyderabad","Rising Pune Super Giants",
                                         "Kolkata Knight Riders","Kings XI Punjab","Royal Challengers Bangalore","Mumbai Indians","Delhi Daredevils",
                                         "Gujarat Lions","Leinster Lightning","North-West Warriors","Northern Knights","Match Drawn",
                                         "Warwickshire","Lancashire","Hampshire","Derbyshire",
                                         "Yorkshire","Surrey","Northamptonshire","Leicestershire",
                                         "Glamorgan","Kent","Gloucestershire","Middlesex",
                                         "Essex","Somerset","Sussex",
                                         "Nottinghamshire","Worcestershire","Durham",
                                         "Trinbago Knight Riders","St Kitts and Nevis Patriots","Barbados Tridents","Jamaica Tallawahs","Guyana Amazon Warriors") , 
                                "team_won" = c("Pakistan","West","England","South","Sri","India",
                                                       "Bangladesh","Australia","Namibia","Hong","Netherlands",
                                                       "United","Kenya","Scotland","Papua","Nepal","New",
                                                       "No","Afghanistan","Zimbabwe","Sunrisers","Rising",
                                                       "Kolkata","Kings","Royal","Mumbai","Delhi",
                                                       "Gujarat","Leinster","North-West","Northern","Match",
                                                       "Warwickshire","Lancashire","Hampshire","Derbyshire",
                                                       "Yorkshire","Surrey","Northamptonshire","Leicestershire",
                                                       "Glamorgan","Kent","Gloucestershire","Middlesex",
                                                       "Essex","Somerset","Sussex",
                                                       "Nottinghamshire","Worcestershire","Durham",
                                                       "Trinbago","St","Barbados","Jamaica","Guyana"))
