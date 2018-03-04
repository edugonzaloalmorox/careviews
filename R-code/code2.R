install.packages("rowr")
library(rowr)


codes =  import("/Users/Personas/Dropbox/side_projects/scrapping/care-homes-reviews/links_reviews.csv")
urls = as.character(codes$link)
copy = urls[6201:6225]



# review 
df <- lapply(copy, function(u){
  
  require(dplyr)
 
  data <- read_html(u)
  rev =  data %>%  html_nodes("[itemprop=reviewBody]")  %>% html_text() %>% as.data.frame()
  date_rev =  data %>%  html_nodes("[itemprop=dateCreated]")  %>% html_text() %>% as.data.frame() 
  names(rev) = "review"
  names(date_rev) = "date_review"
  rev = rev %>% mutate(number_review = row_number())
  id = rep(u, nrow(rev))
  rev = cbind(rev, date_rev, id)
  return(rev)
  
})
 
rev <- do.call(rbind, df) 


# author
author =  lapply(copy, function(u){
  
  require(dplyr)
  
  data <- read_html(u)
  links = data %>% html_nodes(".submitted") %>% html_text() %>% as.data.frame() 
  names(links) <- "var"
  issuer = links %>% separate(var, into = paste0('var', 1:2), sep = '[()]') %>% rename(issued =var2) %>% select(issued)
  issuer = issuer %>% mutate(number_review = row_number())
  id = rep(u, nrow(issuer))
  author = cbind(issuer,id)
  return(author)
})
  
author_issue <- do.call(rbind, author)



# details
details  =  lapply(copy, function(u){
  data <- read_html(u)
  
  name =  data %>%  html_nodes("[itemprop=name]")  %>% html_text() %>% as.data.frame() 
  address =  data %>%  html_nodes("[itemprop=streetAddress]")  %>% html_text() %>% as.data.frame() 
  postalcode =  data %>%  html_nodes("[itemprop=postalCode]")  %>% html_text() %>% as.data.frame()
  
  
  direction =cbind(name, address, postalcode)
  names(direction) <- c("name", "address","postalcode")
  direction = direction[1, ]
  id = id = rep(u, nrow(direction))
  
  details = cbind(direction,id)
  details = details %>% mutate(address = gsub("\r|\n|\t", "", address))
  
  return(details)
  
})

details_df <- do.call(rbind, details)

# Link pieces of information

df_test = left_join(rev, author_issue, by = c("id", "number_review"))

df_test$id = as.factor(df_test$id)



df_test_beta = left_join(df_test, details_df, by = "id")

head(df_test_beta, 20)

write.csv(df_test_beta, "/Users/Personas/Dropbox/side_projects/scrapping/care-homes-reviews/data-6200.csv", row.names = FALSE)





# quality issues
quality =  lapply(copy, function(u){
  
  require(dplyr)
  
    data <- read_html(u)
    links = data %>% html_nodes("[class=col-sm-3]") %>% html_text()  %>% as.data.frame()
    links = data %>% html_nodes("#Overall Standard") %>% html_text()  %>% as.data.frame()
    names(links) <-  "var"
    links = links %>%
      mutate(var2 = gsub("\r|\n|\t", "", var)) %>%
    filter(!str_detect("First Name|Last Name|Email|Verify Email|Telephone", var2)) 
    y  = nrow(links)/4
    number_review = rep(1:y, each = 4)
    
    id = rep(u, nrow(links))

    quality_rev = cbind(links,number_review, id)%>% rename(quality_var = var2) %>% select(-var)

    return(quality_rev)
})

quality_df <- do.call(rbind, quality)


df_test = left_join(rev_25, author_issue, by = c("id", "number_review"))







