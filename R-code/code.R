###############
# Scrapping care homes reviews
#############


library(RJSONIO)
library(xml2)
library(rvest)
library(stringr)
library(dplyr)
library(tidyverse)
library(rio)
library(janitor)
library(tibble)
library(caret)


#############
# Get the codes of the care homes
###############

# ------------
# Get the urls 
# -------------

url = "https://www.carehome.co.uk/care_search_results.cfm/searchcountry/uk/searchchtype/reviews/orderid/-1/startpage/"


urls <-sprintf("https://www.carehome.co.uk/care_search_results.cfm/searchcountry/uk/searchchtype/reviews/orderid/-1/startpage/%s",rep(1:128))

urls_test = head(urls,1)
         

# get links of all reviewed care homes
df <- lapply(urls, function(u){
  
  data <- read_html(u)
  links <- data %>% html_nodes("a") %>% html_attr("href") %>% as.data.frame()
  return(links)
})
 
# transform the list into a data frame
  df = df %>% 
    unlist(recursive = FALSE) %>% 
    enframe() %>% 
    unnest() 
  
# ----------------------------------------------------------------------------
# Extract the codes of the care homes and the link where the review is hosted
# ----------------------------------------------------------------------------
  
  df_codes = df %>% filter(str_detect(value, "https://www.carehome.co.uk/carehome.cfm/searchazref")) %>%
    mutate(home_id = gsub("https://www.carehome.co.uk/carehome.cfm/searchazref/", "", value)) %>%
    group_by(home_id) %>% unique() %>% rename(review_link = value) %>% select(home_id, review_link) %>%
    mutate(link = paste0(review_link, "#reviews"))
  
  
  write.csv(df_codes, "/Users/Personas/Dropbox/side_projects/scrapping/care-homes-reviews/links_reviews.csv", row.names = FALSE)
  
  codes =  import("/Users/Personas/Dropbox/side_projects/scrapping/care-homes-reviews/links_reviews.csv")
  urls = as.character(codes$link)
  
  
 urls
 
 copy = head(urls, 2)
 
 d1 = "https://www.carehome.co.uk/carehome.cfm/searchazref/10002005PENA#reviews"
 
 
 df <- lapply(copy, function(u){
   
   data <- read_html(d1)
   
   links <- data %>% html_nodes(".col-md-12 div p") %>% html_text() %>% as.data.frame()
   links2 <- data %>% html_nodes(".submitted") %>% html_text() %>% as.data.frame()
   links3 <- data %>% html_nodes(".col-sm-12") %>% html_text() %>% as.data.frame()
   
   
   names(links) <- "var"
   names(links2) <- "var"
   names(links3) <- "var"
   
   #links1
   review = links %>% filter(!str_detect(var, "Average Rating|Overall Standard:|Facilities:|Care / Support:|Cleanliness:|Treated with Dignity:|Food & Drink:|Staff:|Activities:|Management:|Safety / Security:|Rooms:|Value for Money:"))
   overall = links %>% filter(str_detect(var, "Overall Standard:"))
   facilities = links %>% filter(str_detect(var, "Facilities:"))
   care = links %>% filter(str_detect(var, "Care / Support:"))
   clean = links %>% filter(str_detect(var, "Cleanliness:"))
   dignity = links %>% filter(str_detect(var, "Treated with Dignity:"))
   food = links %>% filter(str_detect(var, "Food & Drink:"))
   staff = links %>% filter(str_detect(var, "Staff:"))
   activities = links %>% filter(str_detect(var, "Activities:"))
   management = links %>% filter(str_detect(var, "Management:"))
   safety = links %>% filter(str_detect(var, "Safety / Security:"))
   rooms = links %>% filter(str_detect(var, "Rooms:"))
   vfm = links %>% filter(str_detect(var, "Value for Money:"))
   
   #links2
   issuer = links2 %>% separate(var, into = paste0('var', 1:2), sep = '[()]') %>% rename(issued =var2) %>% select(issued)
   date = links2 %>% separate(var, into = paste0('var', 1:2), sep = 'Relates to')  %>% rename(date_rev =var2) %>% select(date_rev)
   
   #link3
   ex = links3 %>% filter(!str_detect(var, "UK Care Homes Search|Website|General"))
   test = ex %>% separate(var, into = paste0('var', 1:2), sep = 'Add to Compare')  %>% rename(date_rev =var2) 
   test = test %>% mutate(postcode = gsub("\t|\r|\n", "", var1)) 
   
   test = test %>% mutate(var_post = str_sub(postcode, -9, -1)) %>% select(var_post)
  
   
   data_reviews = data.frame(review, overall, facilities, care, clean, dignity, food, staff, activities, management, safety, rooms, vfm, issuer, date)
   names(data_reviews) = c("review", "overall", "facilities", "care", "clean", "dignity", "food", "staff", "activities", "management", "safety", "rooms", "vfm", "issued", "date")
   
   data_reviews = cbind(data_reviews, test)
   
 return(data_reviews)
 })
 
 df <- do.call(rbind, df) 
 
 
 
 