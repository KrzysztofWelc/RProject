install.packages(RSelenium)
library(RSelenium)
library(rvest)
library(tidyr)

rD <- rsDriver(
  port = 4763L,
  browser = "chrome",
  chromever = "100.0.4896.20",
)

remDr <- rD[['client']]

remDr$open()
remDr$navigate("https://www.imdb.com/title/tt8649186/reviews/?ref_=tt_ql_urv")

load_more_button <- remDr$findElement(using="xpath", "/html/body/div[3]/div/div[2]/div[3]/div[1]/section/div[2]/div[4]/div/button")

x = load_more_button$isElementDisplayed()
while (load_more_button$isElementDisplayed()[[1]]){
  load_more_button$clickElement()
  Sys.sleep(3)
}

#simpsons_recs = remDr$findElements(using="css selector", '.review-container')

src <- remDr$getPageSource()[[1]]

pg = rvest::read_html(src)
remDr$close()
#data = tidyr::tibble(
#  content = pg %>% rvest::html_nodes('.content .text') %>% rvest::html_text(),
#  rating = pg %>% rvest::html_nodes('.rating-other-user-rating span:first-of-type') %>% rvest::html_text(),
#  date = pg %>% rvest::html_nodes('.review-date') %>% rvest::html_text(),
#)

data = tidyr::tibble()
reviews = pg %>% rvest::html_nodes('.review-container') 
for (r in reviews){
  content = r %>% rvest::html_nodes('.content .text') %>% rvest::html_text()
  rating = r %>% rvest::html_nodes('.rating-other-user-rating span:first-of-type') %>% rvest::html_text()
  date = r %>% rvest::html_nodes('.review-date') %>% rvest::html_text()
  if(length(rating) == 0){
    rating = NA
  }
  
  x = tidyr::tibble(content, rating, date)
  
  data = data %>% bind_rows(x)
  
}