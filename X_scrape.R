# Prereq:
# jdk-21 + add JAVA_HOME to environment variables
# download chrome testing app + chrome driver
# download selenium .jar file
# place chrome stuff + .jar file in the same location

options(scipen = 999)
list.of.packages <- c("RSelenium", "tm", "wordcloud", "RColorBrewer", "tidyverse", "rvest", "vader")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = T)

require(tidyverse)
require(RSelenium)
require(quanteda, warn.conflicts = FALSE, quietly = TRUE)
require(stringr)
require(readtext)
require(RCurl)
library(plyr)
library(tm)
library(tidyverse)
library(vader)
library(dplyr)
library(ggplot2)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


###############FETCHING DATA - SCRAPING#########################################
# Run Selenium server in separate CMD window (Windows)
# java -Dwebdriver.chrome.driver="chromedriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4450

remDr <- remoteDriver(remoteServerAddr = "localhost" ,
                      port = 4450L,
                      browserName = "chrome")

remDr$open()
remDr$navigate("https://www.nitter.net/")

input <- "Brexit lang:en"

tweets_before <- tibble() 
tweets_after <- tibble()


for (y in 2013:2025) {
  for (m in 1:12) {
    for (d in 1:2) {
      
      Sys.sleep(2)
      remDr$navigate("https://nitter.net/search?f=tweets&q=")
      Sys.sleep(2)
      
      input_box <- remDr$findElement("xpath", "//input[@name='q']")
      input_box$sendKeysToElement(list(input))
      #Sys.sleep()
      
      filter <- remDr$findElements(using = "xpath", value = "//span")
      filter[[2]]$highlightElement()
      filter[[2]]$clickElement()
      Sys.sleep(2)
      
      # Locate "from" and "to" date inputs
      date_from <- remDr$findElement("xpath", "//input[@name='since']")
      date_to   <- remDr$findElement("xpath", "//input[@name='until']")

      yyyy <- sprintf("%04d", y)
      mmdd <- sprintf("%02d%02d", m, d)      # "0108"
      
      yyyy_to <- sprintf("%04d", y)
      mmdd_to <- sprintf("%02d%02d", m, d+1)
      
      # Clear input
      date_from$clearElement()
      date_to$clearElement()
      Sys.sleep(0.3)
      
      # Enter FROM date
      date_from$sendKeysToElement(list(
        yyyy,             # type year
        key = "tab",      # tab to next sub-field
        mmdd              # month+day typed together
      ))
      
      # Enter TO date
      date_to$sendKeysToElement(list(
        yyyy_to,
        key = "tab",
        mmdd_to
      ))
      
      search <- remDr$findElements(using = "xpath", value = "//span")
      search[[1]]$clickElement()
      Sys.sleep(3)
      
      # Scroll to load more tweets
      scroll_times <- 3
      for (i in 1:scroll_times){
        remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")
        Sys.sleep(1)
      }
      
      # Extract tweets
      tweet_divs <- remDr$findElements(
        "xpath",
        "//div[contains(@class,'timeline-item')]"
      )
      tweet_texts <- tweet_divs |>
        lapply(\(x) tryCatch(x$getElementText()[[1]], error = \(e) NA)) |>
        unlist() |>
        as.character() |>
        discard(\(v) is.na(v) || v == "" || identical(v, "function"))
      
      Sys.sleep(2)
      # Convert to structured df
      tweets_df <- tibble(raw = tweet_texts) %>%
        mutate(
          lines = str_split(raw, "\n"),
          date = sapply(lines, function(x) if(length(x) >= 3) x[3] else NA),
          text = sapply(lines, function(x) paste(x[4:length(x)], collapse = " "))
        ) %>%
        select(date, text)
      
      Sys.sleep(1)
      # Append to df
      current_date <- as.Date(paste0(yyyy, "-", m, "-", d))
      
      if (current_date <= as.Date("2020-01-31")) {
        tweets_before <- bind_rows(tweets_before, tweets_df)
      } else {
        tweets_after <- bind_rows(tweets_after, tweets_df)
      }
      Sys.sleep(1)
    }
  }
}

tail(tweets_before,1)
tail(tweets_after,1)

                     
# closing 
remDr$closeall()
rm(remDr)
gc()

################FETCHING DATA - SAVED ON MACHINE######################
write.csv(tweets_before, "tweets_before.csv", row.names = FALSE)
write.csv(tweets_after,  "tweets_after.csv",  row.names = FALSE)

tweets_before <- read.csv("tweets_before.csv")
tweets_after  <- read.csv("tweets_after.csv")

write.csv(LSD_tweets_before, "LSD_tweets_before", row.names = FALSE)
write.csv(LSD_tweets_after,  "LSD_tweets_after",  row.names = FALSE)

write.csv(vader_tweets_before, "vader_tweets_before", row.names = FALSE)
write.csv(vader_tweets_after,  "vader_tweets_after",  row.names = FALSE)

LSD_tweets_before <- read.csv("LSD_tweets_before")
LSD_tweets_after  <- read.csv("LSD_tweets_after")

vader_tweets_before <- read.csv("vader_tweets_before")
vader_tweets_after  <- read.csv("vader_tweets_after")

LSD_hansard_before <- read.csv("data_before")
LSD_hansard_after  <- read.csv("data_after")

#################FORMATTING#########################
# Add 2025 to this years date, rows with date on format "day month", for 2025 entries only
tweets_after$date <- ifelse(
  grepl("^[A-Za-z]{3} \\d+$", tweets_after$date),
  format(as.Date(paste0(tweets_after$date, " 2025"), "%b %d %Y"), "%d %b %Y"),
  tweets_after$date
)

# Where scraping has not picked up date in date column but in text, for 2025 entries only
tweets_after <- tweets_after %>% 
  mutate(
    date = ifelse(
      str_detect(text, "^[A-Za-z]{3} \\d+"),
      text %>% 
        str_extract("^[A-Za-z]{3} \\d+") %>% 
        str_replace("^([A-Za-z]{3}) (\\d+)$", "\\2 \\1 2025"),
      date
    )
  )

# Set all time stamps to date stamps, for 2025 entries only
tweets_after <- tweets_after %>%
  mutate(date = if_else(!is.na(date) & str_starts(date, "@"), "8 Dec 2025", date))
tweets_after$date <- ifelse(grepl("^[0-9]+s$", tweets_after$date), "8 Dec 2025", tweets_after$date)


# Format date 
tweets_after$date <- format(as.Date(tweets_after$date, "%d %b %Y"), "%d %b %Y")
tweets_before$date <- format(as.Date(tweets_before$date, "%d %b %Y"), "%d %b %Y")


#####################CLEANING###############################
cleaning_function <- function(data_frame, column_name){
  
  cleaned_text <- data_frame[[column_name]] %>%
    str_remove_all("http\\S+") %>%        # URLs
    str_remove_all("@\\w+") %>%            # mentions
    str_remove_all("\\bRT\\b|\\bvia\\b") %>% # retweets
    str_replace_all("#", "") %>%           # keep hashtag words
    str_remove_all("<.*?>")
  
  cleaned_text %>%
    tokens(remove_punct = TRUE, remove_symbols = TRUE) %>%
    tokens_remove(stopwords("english")) %>%
    tokens_remove(pattern = "^.{1,2}$", valuetype = "regex") %>% # very short words
    tokens_wordstem() %>%
    tokens_lookup(dictionary = data_dictionary_LSD2015)
}


cleaned_before <- cleaning_function(tweets_before, "text")
cleaned_after <- cleaning_function(tweets_after, "text")

###################SENTINENT ANALYSIS#######################################

#LSD
LSD_sentiment_count <- function(data){
  dfm <- dfm(data)

  dict_output <- convert(dfm, to = "data.frame")

  ## convert and estimate sentiment
  dict_output$Value <- log(
    (dict_output[,3] + dict_output[,5] + 0.5) /
    (dict_output[,2] + dict_output[,4] + 0.5)
  )
  
  dict_output
}

LSD_before_sentiment <- LSD_sentiment_count(cleaned_before)
LSD_after_sentiment <- LSD_sentiment_count(cleaned_after)


# Add LSD sentiment value to df
LSD_tweets_before <- tweets_before
LSD_tweets_before$doc_id <- seq_len(nrow(LSD_tweets_before))
LSD_before_sentiment$doc_id <- seq_len(nrow(LSD_before_sentiment))
LSD_tweets_before <- merge(
  LSD_tweets_before[, c("date", "text", "doc_id")],
  LSD_before_sentiment[, c("doc_id", "Value")],
  by = "doc_id"
)

LSD_tweets_after <- tweets_after
LSD_tweets_after$doc_id <- seq_len(nrow(LSD_tweets_after))
LSD_after_sentiment$doc_id <- seq_len(nrow(LSD_after_sentiment))
LSD_tweets_after <- merge(
  LSD_tweets_after[, c("date", "text", "doc_id")],
  LSD_after_sentiment[, c("doc_id", "Value")],
  by = "doc_id"
)


#Vader
vader_results_before <- vader_df(
  tweets_before$text,
  incl_nt = TRUE,
  neu_set = TRUE,
  rm_qm = TRUE
)

vader_tweets_before <- tweets_before
vader_tweets_before$vader_compound <- vader_results_before$compound
vader_tweets_before$vader_pos      <- vader_results_before$pos
vader_tweets_before$vader_neu      <- vader_results_before$neu
vader_tweets_before$vader_neg      <- vader_results_before$neg
vader_tweets_before$doc_id <- seq_len(nrow(vader_tweets_before))

vader_results_after <- vader_df(
  tweets_after$text,
  incl_nt = TRUE,
  neu_set = TRUE,
  rm_qm = TRUE
)

vader_tweets_after <- tweets_after
vader_tweets_after$vader_compound <- vader_results_after$compound
vader_tweets_after$vader_pos      <- vader_results_after$pos
vader_tweets_after$vader_neu      <- vader_results_after$neu
vader_tweets_after$vader_neg      <- vader_results_after$neg
vader_tweets_after$doc_id <- seq_len(nrow(vader_tweets_after))




##################DENSITY PLOTS###########################
#Density plot LSD
plot_df <- bind_rows(
  LSD_before_sentiment %>% mutate(period = "Before"),
  LSD_after_sentiment  %>% mutate(period = "After")
)

ggplot(plot_df, aes(x = Value, fill = period)) +
  geom_density(alpha = 0.4) +
  labs(x = "Sentiment score", y = "Density")


#Density plot Vader
plot_df <- bind_rows(
  vader_tweets_before %>% mutate(period = "Before"),
  vader_tweets_after  %>% mutate(period = "After")
)

ggplot(plot_df, aes(x = vader_compound, fill = period)) +
  geom_density(alpha = 0.4) +
  labs(x = "Sentiment score", y = "Density")


###############PLOTS LSD######################
#sentiment over time, spread
ggplot(LSD_tweets_before, aes(date, Value)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(x = "Date", y = "Sentiment")

#sentiment over time, mean
LSD_tweets_before %>%
  group_by(date) %>%
  dplyr::summarize(Value = mean(Value, na.rm = T)) %>%
  ggplot(aes(x = date, y = Value)) + 
  geom_bar(stat = "identity")

#sentinent over time, scatter
ggplot(LSD_tweets_before, aes(date, Value)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, span = 0.3) +
  labs(x = "Year", y = "Sentiment")


#sentinent over time, monthwise
LSD_tweets_before$date <- as.Date(as.character(LSD_tweets_before$date))
LSD_tweets_before$month <- format(LSD_tweets_before$date, "%Y-%m")
monthly_LSD <- aggregate(Value ~ month, data = LSD_tweets_before, FUN = mean)
monthly_LSD$month <- as.Date(paste0(monthly_LSD$month, "-01"), "%Y-%m-%d")

ggplot(monthly_LSD, aes(x = month, y = Value)) +
  geom_line() +
  geom_point() +
  labs(x = "Month", y = "Sentiment Value")


#sentinent over time, monthwise
# prepare before dataset
LSD_tweets_before$date <- as.Date(trimws(LSD_tweets_before$date), "%d %b %Y")
#LSD_tweets_before$date <- as.Date(as.character(LSD_tweets_before$date))
LSD_tweets_before$month <- format(LSD_tweets_before$date, "%Y-%m")
monthly_LSD_before <- aggregate(Value ~ month, data = LSD_tweets_before, FUN = mean)
monthly_LSD_before$month <- as.Date(paste0(monthly_LSD_before$month, "-01"), "%Y-%m-%d")
monthly_LSD_before$period <- "Before Brexit"

# prepare after dataset
LSD_tweets_after$date <- as.Date(trimws(LSD_tweets_after$date), "%d %b %Y")
#LSD_tweets_after$date  <- as.Date(as.character(LSD_tweets_after$date))
LSD_tweets_after$month <- format(LSD_tweets_after$date, "%Y-%m")
monthly_LSD_after <- aggregate(Value ~ month, data = LSD_tweets_after, FUN = mean)
monthly_LSD_after$month <- as.Date(paste0(monthly_LSD_after$month, "-01"), "%Y-%m-%d")
monthly_LSD_after$period <- "After Brexit"

# combine
monthly_LSD_all <- rbind(monthly_LSD_before, monthly_LSD_after)

# plot
ggplot(monthly_LSD_all, aes(x = month, y = Value, color = period)) +
  geom_line() +
  geom_point() +
  labs(x = "Month", y = "Sentiment LSD", color = "Period") +
  theme_minimal()

#sentiment over time, quarter wise
monthly_LSD_all$quarter <- format(monthly_LSD_all$month, "%Y-Q%q")  # pseudo-quarter
quarterly_LSD <- aggregate(Value ~ quarter + period, data = monthly_LSD_all, FUN = mean)

ggplot(quarterly_LSD, aes(x = quarter, y = Value, color = period, group = period)) +
  geom_line() +
  geom_point() +
  labs(x = "Quarter", y = "Sentiment LSD", color = "Period") +
  theme_minimal()


#sentiment over time, monthwise, smooth
library(zoo)
monthly_LSD_all$Value_smooth <- rollmean(monthly_LSD_all$Value, k = 3, fill = NA)  # 3-month rolling

ggplot(monthly_LSD_all, aes(x = month, y = Value_smooth, color = period)) +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Sentiment LSD")


###############PLOTS VADER###########################

# prepare before dataset
vader_tweets_before$date <- as.Date(trimws(vader_tweets_before$date), "%d %b %Y")
vader_tweets_before$month <- format(vader_tweets_before$date, "%Y-%m")
monthly_vader_before <- aggregate(vader_compound ~ month, data = vader_tweets_before, FUN = mean)
monthly_vader_before$month <- as.Date(paste0(monthly_vader_before$month, "-01"), "%Y-%m-%d")
monthly_vader_before$period <- "Before Brexit"

# prepare after dataset
vader_tweets_after$date <- as.Date(trimws(vader_tweets_after$date), "%d %b %Y")
vader_tweets_after$month <- format(vader_tweets_after$date, "%Y-%m")
monthly_vader_after <- aggregate(vader_compound ~ month, data = vader_tweets_after, FUN = mean)
monthly_vader_after$month <- as.Date(paste0(monthly_vader_after$month, "-01"), "%Y-%m-%d")
monthly_vader_after$period <- "After Brexit"

# combine
monthly_vader_all <- rbind(monthly_vader_before, monthly_vader_after)

# plot
ggplot(monthly_vader_all, aes(x = month, y = vader_compound, color = period)) +
  geom_line() +
  geom_point() +
  labs(x = "Month", y = "Sentiment Vader", color = "Period") +
  theme_minimal()

#sentiment over time, quarter wise
monthly_vader_all$quarter <- format(monthly_vader_all$month, "%Y-Q%q")  # pseudo-quarter
quarterly_vader <- aggregate(vader_compound ~ quarter + period, data = monthly_vader_all, FUN = mean)

ggplot(quarterly_vader, aes(x = quarter, y = vader_compound, color = period, group = period)) +
  geom_line() +
  geom_point() +
  labs(x = "Quarter", y = "Sentiment Vader", color = "Period") +
  theme_minimal()


#sentiment over time, monthwise, smooth
library(zoo)
monthly_vader_all$vader_compound <- as.numeric(monthly_vader_all$vader_compound)
monthly_vader_all$vader_compound_smooth <- rollmean(monthly_vader_all$vader_compound, k = 3, fill = NA)  # 3-month rolling

ggplot(monthly_vader_all, aes(x = month, y = vader_compound_smooth, color = period)) +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Sentiment Vader")


#####################WEIGHTING####################
#Before
combined_smooth <- bind_rows(
  monthly_vader_all %>%
    transmute(month, period, sentiment = vader_compound_smooth, method = "VADER"),
  monthly_LSD_all %>%
    transmute(month, period, sentiment = Value_smooth, method = "LSD")
)

ggplot(combined_smooth, aes(x = month, y = sentiment, color = method, linetype = period)) +
  geom_line(linewidth = 1.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Sentiment")



combined <- monthly_LSD_all %>%
  inner_join(monthly_vader_all, by = "month")

combined$w50_50 <- 0.5 * combined$Value + 0.5 * combined$vader_compound
combined$w70_30 <- 0.7 * combined$Value + 0.3 * combined$vader_compound
combined$w30_70 <- 0.3 * combined$Value + 0.7 * combined$vader_compound

plot_df <- combined %>%
  pivot_longer(cols = c(w50_50, w70_30, w30_70),
               names_to = "weighting",
               values_to = "sentiment")

ggplot(plot_df, aes(x = month, y = sentiment, color = weighting)) +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Weighted Sentiment LSD_vader")




################HANSARD + TWEETS (LSD WEIGHTED VADER)########################################
LSD_hansard_before$date <- as.Date(trimws(LSD_hansard_before$date), "%d %b %Y")
LSD_hansard_after$date <- as.Date(trimws(LSD_hansard_after$date), "%d %b %Y")


LSD_hansard_all <- bind_rows(
  LSD_hansard_before,
  LSD_hansard_after
)

LSD_hansard_all$month <- format(LSD_hansard_all$date, "%Y-%m")

monthly_hansard <- aggregate(Value ~ month, data = LSD_hansard_all, FUN = mean)
monthly_hansard$month <- as.Date(paste0(monthly_hansard$month, "-01"))
monthly_hansard$series <- "Hansard (LSD)"

#####

monthly_weighted <- combined %>%
  dplyr::select(month, w50_50) %>%
  dplyr::rename(Value = w50_50)


monthly_weighted$series <- "Tweets (LSD + VADER)"

#Monthly
plot_df <- bind_rows(monthly_hansard, monthly_weighted)

ggplot(plot_df, aes(x = month, y = Value, color = series)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Month",
    y = "Sentiment",
    color = ""
  ) +
  theme_minimal()


#Events monthly
events <- as.Date(c("2016-06-23", "2020-01-31"))

ggplot(plot_df, aes(month, Value, color = series)) +
  geom_line() +
  geom_vline(xintercept = events, linetype = "dashed") +
  ggplot2::annotate(
    "text",
    x = events,
    y = max(plot_df$Value, na.rm = TRUE),
    label = c("Referendum", "Exit"),
    angle = 90,
    vjust = -0.5
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") 

#Density
plot_df %>%
  mutate(period = ifelse(month < as.Date("2020-02-01"), "Before", "After")) %>%
  ggplot(aes(Value, fill = period)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ series) +
  labs(x = "Sentiment", y = "Density")



#################WORDCLOUD########################## - BAD
library(wordcloud)
library(RColorBrewer)

tweets_wc   <- data.frame(text = c(tweets_before$text, tweets_after$text))
hansard_wc  <- data.frame(text = c(LSD_hansard_before$text, LSD_hansard_after$text))


make_wc <- function(df, title) {
  
  corpus <- Corpus(VectorSource(df$text))
  
  corpus <- corpus |>
    tm_map(content_transformer(tolower)) |>
    tm_map(removePunctuation) |>
    tm_map(removeNumbers) |>
    tm_map(removeWords, stopwords("en")) |>
    tm_map(stripWhitespace)
  
  corpus <- corpus[sapply(corpus, function(x) nchar(as.character(x)) > 0)]
  
  tdm <- TermDocumentMatrix(corpus)
  m   <- as.matrix(tdm)
  freq <- sort(rowSums(m), decreasing = TRUE)
  
  wordcloud(
    words = names(freq),
    freq  = freq,
    max.words = 120,
    random.order = FALSE,
    colors = brewer.pal(8, "Dark2"),
    scale = c(3, 0.6)
  )
  title(title)
}



par(
  mfrow = c(1, 2),
  mar = c(1, 1, 3, 1),  
  oma = c(0, 0, 0, 0)
)

make_wc(tweets_wc,  "Tweets")
make_wc(hansard_wc, "Hansard")






