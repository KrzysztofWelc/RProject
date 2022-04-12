install.packages(RSelenium)
library(RSelenium)
library(rvest)
library(tidyr)

rD <- rsDriver(
  port = 4768L,
  browser = "chrome",
  chromever = "100.0.4896.20",
)

remDr <- rD[['client']]



scrap = function(client, url){
  client$open()
  client$navigate(url)
  
  load_more_button <- remDr$findElement(using="xpath", "/html/body/div[3]/div/div[2]/div[3]/div[1]/section/div[2]/div[4]/div/button")
  
  while (load_more_button$isElementDisplayed()[[1]]){
    load_more_button$clickElement()
    Sys.sleep(6)
  }
  
  src <- client$getPageSource()[[1]]
  
  pg = rvest::read_html(src)
  client$close()
  
  
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
  data
}

simpsons_data = scrap(remDr, 'https://www.imdb.com/title/tt0096697/reviews/?ref_=tt_ql_urv')
write.csv(simpsons_data, './data/simpsons_data.csv')


guy_data = scrap(remDr, 'https://www.imdb.com/title/tt0182576/reviews/?ref_=tt_ql_urv')
write.csv(guy_data, './data/guy_data.csv')


southpark_data = scrap(remDr, 'https://www.imdb.com/title/tt0121955/reviews/?ref_=tt_ql_urv')
write.csv(southpark_data, './data/southpark_data.csv')


