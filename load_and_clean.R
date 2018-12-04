#' 
#' Test run of email cleaning and analysis.
#' 

################
### packages ###
################

require(tm.plugin.mail)
require(tm)
require(tidytext)
require(tidyverse)
require(openxlsx)
require(rvest)

######################
### load mbox data ###
######################

#' this will take an mbox file (gmails format for downloading emails) and separate them
#' into a new folder ('mail' here) with one file per email. tm can create a VCorpus 
#' object. It is then parsed into a dataframe via tidytext functions. 

mail <- paste0(getwd(), "/mail") 
tm.plugin.mail::convert_mbox_eml("data/mbox_archives/politicalemails_2018_11_07.mbox", mail)
vc <- tm::VCorpus(DirSource(mail), readerControl = list(reader = readMail))

emails <- tidytext::tidy(vc)
emails$archive_date <- "2018-11-07" #date I downloaded these emails

emails <- emails %>% 
  mutate(author_email = str_extract(author,pattern = "<(.*)>") %>% str_sub(2,-2)) %>% 
  filter(#as.Date(datetimestamp) > as.Date("2018-09-27"), #date I signed up for most senate emails
    author_email != "noreply@google.com", #google emails to download archives
    author_email != "info@patriotemails.com" #Chris Sununu campaign somehow got my email
  ) 


#####################
### sender counts ###
#####################

#' I need to manually map each email adress to a campaign to identify party and other features.
#' This section saves a csv of each unique email adress and counts to do this mapping.  

sender_counts <- emails %>% group_by(author_email) %>% 
  summarise(emails = n(),
            kav_mentions = sum(str_detect(string = text,"(k|K)avanaugh")),
            trump_mentions = str_detect(text,"Trump") %>% sum()) %>% 
  arrange(desc(emails))

sender_counts

#save lists of emails to map to campaigns, load already mapped emails
email_campaign_crosswalk <- read.xlsx("data/2018 races.xlsx",sheet = 2)
sender_counts <- left_join(sender_counts,email_campaign_crosswalk,by="author_email")

sum(is.na(sender_counts$Campaign_id)) #ensure this is 0

saveRDS(emails,file = "data/clean/emails.rds")
write_csv(sender_counts,path = "output/sender_counts.csv")

###########################
### parse email message ###
###########################

#helper function to parse html code in email text fields 
quick_parse_text <- function(text){
  
  tables <- text %>% read_html() %>% html_nodes("table")
  if(length(tables) > 1){
    table <-  tables[1]
  }
  else{
    table <- tables
  }
  
  parsed_text <- table %>% 
    html_text() %>% str_replace_all("=|>|\\n"," ") %>% 
    str_squish() #deletes all \n, might want a space
  
  if(is_empty(parsed_text)){
    return("parse failure")
  }
  else{
    return(parsed_text)
  }
}

#check result
emails$text[1] %>% quick_parse_text()

#add to database
emails <- emails %>% 
  mutate(parsed_text = map(emails$text,possibly(quick_parse_text,otherwise = "parse failure")) %>% unlist())

#emails$text[4] %>% guess_encoding()

########################
### add campaign ids ###
########################

#manually created dataset
email_campaign_crosswalk <- read.xlsx("data/2018 races.xlsx",sheet = 2)
campaign_info <- read.xlsx("data/2018 races.xlsx",sheet = 1)

emails <- left_join(x = emails,y=email_campaign_crosswalk,by="author_email")
emails <- left_join(x = emails,y=campaign_info,by="Campaign_id")


##################
### clean data ###
##################

#correct Bernie's party
emails_simple <- emails_simple %>% 
  mutate(Party = if_else(Organization == "Bernie Sanders","D",Party),
         datetimestamp = as.Date(datetimestamp))

#not all the variables are useful
emails_simple <- emails %>% 
  select(author,datetimestamp,author_email,Organization,Party,District,parsed_text) %>% 
  mutate(email_id = row_number()) %>% select(email_id,everything())

write_csv(emails_simple,"output/emails_simple.csv")
