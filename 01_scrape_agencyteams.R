library(tidyverse)
library(httr)  
library(rvest) 
library(janitor)
library(writexl)

#first we'll capture how many tables on on the page

#set url for transition list page
myurl <- "https://buildbackbetter.gov/the-transition/agency-review-teams/"
#perform the GET call
website1 <- GET(myurl) 
#see how many tables this captures
tbls <- html_nodes(content(website1), "table")
num_tables <- length(tbls)
num_tables



### FUNCTION FOR SCRAPING TABLES ####

#set url for transition list page
scrape_table <- function(tablenum) {

  url <- "https://buildbackbetter.gov/the-transition/agency-review-teams/"
  #perform the GET call
  website1 <- GET(url) 
  #grab the titles of all tables
  titles <- html_nodes(content(website1), "h2")
  #capture just one
  title1 <- print(html_text(titles)[[tablenum]])
  #grab the DATA inside the associated table
  table1 <- html_table(tbls[[tablenum]], fill=TRUE)
  #add the name of the table itself as a new column and clean the column names
  table1 <- table1 %>% 
    as_tibble() %>% 
    clean_names() %>% 
    mutate(
      agency = title1
    ) %>% 
    select(agency, everything())
  
  return(table1)

}

#run function once
scrape_table(1)



#### LOOP THROUGH ALL TABLES ####

#we'll use the map function to loop through all the tables at once and combine

#above we measured how many tables were on the page
num_tables
#we'll use this variable to determine how many table numbers we'll want
num_sequence <- seq(1, num_tables)
num_sequence

#now we'll feed the sequence of numbers into the function
transition_data_scraped <- map_df(num_sequence, scrape_table)

transition_data_scraped

#add a unique ID field string to match new records
#also another one just for names, which we'll use for people in multiple roles
transition_data_scraped <- transition_data_scraped %>% 
  mutate(
    idstring = str_trim(paste0(name, most_recent_employment, agency)),
    namestring = str_trim(paste0(name, most_recent_employment))
  )

##determine names with multiple roles and flag them in new column
multiplenames <- transition_data_scraped %>% 
  count(namestring) %>% 
  filter(n > 1) %>% 
  pull(namestring)

transition_data_scraped <- transition_data_scraped %>% 
  mutate(
    on_multiple_teams = if_else(namestring %in% multiplenames, "Y", "")
  )

#some names have title after denoting they are the team lead
#we'll capture that and remove title from name field
transition_data_scraped <- transition_data_scraped %>% 
  mutate(
    team_lead = if_else(str_detect(name, "Team Lead"), "Y", ""),
    name = str_remove(name, ", Team Lead")
  ) %>% 
  select(agency, name, team_lead, on_multiple_teams, everything())




#save results
saveRDS(transition_data_scraped, "processed_data/transition_data_scraped.rds")
write_xlsx(transition_data_scraped, "processed_data/transition_data_scraped.xlsx")


#save archived copy to use for identifying changes later on
filestring <- paste0("archived_data/transition_data_archived_", Sys.time())
filestring <- str_replace_all(filestring, "-", "_")
filestring <- str_replace_all(filestring, ":", "_")
filestring <- str_replace(filestring, " ", "t")
#remove seconds for clarity
filestring <- str_sub(filestring, 1, -4L)
#add file extension
filestring <- paste0(filestring, ".rds")
#run the string through and save the file
saveRDS(transition_data_scraped, filestring)





# 
# ### STEP BY STEP FOR ONCE ####
# 
# #set url for transition list page
# url <- 'https://buildbackbetter.gov/the-transition/agency-review-teams/'
# 
# #perform the GET call
# website1 <- GET(url) 
# 
# #let's see what we have in content
# print(content(website1))
# 
# 
# # Getting information from a website with html_nodes from the rvest package
# # We get the webpage title and tables with html_nodes and labels such as h2
# # which was used for the title of the website and table used for the tables.
# 
# #grab the titles of all tables
# titles <- html_nodes(content(website1), "h2")
# #show them all
# print(html_text(titles))
# #show just one
# title1 <- print(html_text(titles)[[1]])
# title1
# 
# #see how many tables this captures
# tbls <- html_nodes(content(website1), "table")
# print(length(tbls))
# #looks like all of them
# 
# 
# #grab the DATA inside the tables
# tbl1 <- html_table(tbls[[1]], fill=TRUE)
# print(tbl1)
# 
# #add the name of the table itself as a new column and clean the column names
# tbl1_withname <- tbl1 %>% 
#   as_tibble() %>% 
#   clean_names() %>% 
#   mutate(
#     agency = title1
#   ) %>% 
#   select(agency, everything())
# 
# tbl1_withname
# 
# 
# 
# 
