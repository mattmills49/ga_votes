#' The Files we want are not the most organized. The XML file has 280 top
#' level nodes. The first five are:
#' 1. Timestamp
#' 2. ElectionName
#' 3. ElectionDate
#' 4. Region
#' 5. ElectionVoterTurnout
#' 
#' And the rests are all the actual "contests". 
#' 
#' Within each "contest" we have at least 3 sublists:
#' 1. The county by county vote totals
#' 2. The candidates, where each has their own sublist
#' 3. Attributes about the votes
#' 
#' The arributes are as follows 
#' key                                     21000
#' text                      Lieutenant Governor
#' isQuestion                              false
#' countiesParticipating                     159
#' countiesReported                          159
#' precinctsParticipating                   2634
#' precinctsReported                        2634
#' precinctsReportingPercent              100.00
#' 
#' Each candidates list has a sublist of the different vote types they
#' can receive. The last list, ".attrs", tells you what kind of votes they were.
#' 
#' So here is the structure for governer:
#' 
#' |-> Governor
#' ..|-> Brian Kemp 
#' ..|-> Stacey Abrams
#'   ..|-> Election Day
#'   ..|-> Absentee by Mail
#'   ..|-> Absentee in Person
#'   ..|-> Provisional
#'
#' Within each group the info looks like this:
#'      name     votes 
#' "Appling"     "630" 
#' 
#' So lets start with each race:
#' - We need the race text
#' - Then within each candidate
#' - We need the candidate names
#' - Then within each county we need the vote totals
library(magrittr)

map_df <- function(...) dplyr::bind_rows(lapply(...))

county_votes = function(vote_list){
  vote_type = vote_list[[".attrs"]]["name"]
  vote_totals = vote_list %>%
    head(-1) %>%
    lapply(function(x) data.frame(t(x), stringsAsFactors = F)) %>%
    dplyr::bind_rows()
  vote_totals$type = vote_type
  names(vote_totals)[1] = "county"
  
  return(vote_totals)
}

candidate_votes <- function(candidate_list){
  candidate_name = candidate_list[[".attrs"]]["text"]
  
  cand_df = candidate_list %>%
    head(-1) %>%
    lapply(county_votes) %>%
    dplyr::bind_rows()
  
  cand_df$name = candidate_name
  
  return(cand_df)
}

race_votes <- function(race_list){
  race_name = race_list[[".attrs"]]["text"]
  
  race_df = race_list %>%
    head(-1) %>%
    tail(-1) %>%
    lapply(candidate_votes) %>%
    dplyr::bind_rows()
  
  race_df$race = race_name
  
  return(race_df)
}

race_votes <- function(vote_list){
  # "Precinct"...
  # name           votes  
  # "Election Day" "26461"
  vote_type = vote_list[[".attrs"]]["name"]
  vote_lgl = names(vote_list) == "Precinct"
  vote_results = vote_list[vote_lgl] %>%
    map_df(function(x) data.frame(t(x), stringsAsFactors = F))
  names(vote_results)[1] = "precinct"
  vote_results$type = vote_type
  return(vote_results)
}

race_choices <- function(choice_list){
  # "VoteType" "VoteType" "VoteType" "VoteType" "VoteType" "VoteType" ".attrs"  
  # key  text                totalVotes
  # "10" "BRIAN KEMP  (REP)" "48923"   
  choice_name = choice_list[[".attrs"]]["text"]
  choice_key = choice_list[[".attrs"]]["text"]
  vote_lgl = names(choice_list) == "VoteType"
  choice_votes_df = map_df(choice_list[vote_lgl], race_votes)
  choice_votes_df$candidate = choice_name
  choice_votes_df$candidate_key = choice_key
  return(choice_votes_df)
}

race_names <- function(contest_list){
  # "VoteType" "VoteType" "Choice"   "Choice"   "Choice"   ".attrs" 
  # key  text       isQuestion precinctsReporting precinctsReported
  # "10" "Governor" "false"    "193"              "193"  
  race_name = contest_list[[".attrs"]]["text"]
  race_key = contest_list[[".attrs"]]["key"]
  choice_lgl = names(contest_list) == "Choice"
  choices_df = map_df(contest_list[choice_lgl], race_choices)
  choices_df$race = race_name
  choices_df$race_key = race_key
  return(choices_df)
}

tidy_precinct_votes <- function(county_list){
  contest_lgl = names(county_list) == "Contest"
  return(map_df(county_list[contest_lgl], race_names))
}




