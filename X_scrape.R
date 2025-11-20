# Prereq:
# jdk-21 + add JAVA_HOME to environment variables
# download chrome testing app + chrome driver
# download selenium .jar file
# place chrome stuff + .jar file in the same location

options(scipen = 999)
list.of.packages <- c("RSelenium", "tidyverse", "rvest")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = T)

require(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

require(RSelenium)

# Run Selenium server in separate terminal
# java -Dwebdriver.chrome.driver="chromedriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4450

remDr <- remoteDriver(remoteServerAddr = "localhost" ,
                      port = 4450L,
                      browserName = "chrome")

remDr$open()
remDr$navigate("https://www.x.com/")

#remDr$goBack()
#remDr$goForward()
#remDr$refresh()
#remDr$getCurrentUrl()
#remDr$getTitle()

# closing 
remDr$closeall()
rm(remDr)
gc()
