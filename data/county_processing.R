source("Documents/ga_votes/lib/helpers.R")

raw_xml = XML::xmlParse("Documents/ga_votes/data/county_results/DeKalb_votes.xml")
data_list = XML::xmlToList(raw_xml)

vote_info = tidy_precinct_votes(data_list)

write.csv(vote_info, file="Documents/ga_votes/data/dekalb_results.csv", row.names = F)
