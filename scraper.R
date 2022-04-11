install.packages(RSelenium)
library(RSelenium)

rD <- rsDriver(
  port = 4567L,
  browser = "chrome",
  chromever = "100.0.4896.20",
)

remDr <- rD[['client']]

remDr$open()
remDr$navigate("https://www.imdb.com/title/tt0096697/reviews/?ref_=tt_ql_urv")
load_more_button$clickElement

load_more_button <- remDr$findElement(using="xpath", "/html/body/div[3]/div/div[2]/div[3]/div[1]/section/div[2]/div[4]/div/button")


while (isTRUE(load_more_button$isElementDisplayed())){
  load_more_button$clickElement()
  Sys.sleep(1000)
  load_more_button <- remDr$findElement(using="xpath", "/html/body/div[3]/div/div[2]/div[3]/div[1]/section/div[2]/div[4]/div/button")
}
