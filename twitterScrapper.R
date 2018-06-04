# Preamble 
# This scraper is to be run automatically between 23h30 and 00h to get the daily variation
# on the price of 10 crypto-currencies, the existence of a commit on the respective github repositories
# and the number of tweets of the official affiliated account
# The scraper is run from the 1st of December 2017 until the first of January 2018, included

library(twitteR)
library(httr)
library(jsonlite)
library(rvest)

# Twitter initialization
api_key <- 'xxx'
api_secret <- 'xxx'
access_token <- 'xxx-xxx'
access_secret <- 'xxx'

setup_twitter_oauth(api_key, api_secret, access_token, access_secret)

# Relevant URLS
COINMARKET_CAP_URL <- parse_url("https://coinmarketcap.com/")
GITHUB_API <- parse_url("https://api.github.com")

# 

# Define the crypto currencies' names of interest and their respective github repositories 
crypto_currencies <- c('Bitcoin', 'Dash', 'Ethereum', 'Ripple', 
                                  'NEO', 'Ethereum Classic', 'IOTA', 'Litecoin',
                                  'Monero', 'ZCoin')

github_repositories <- c('bitcoin/bitcoin', 'dashpay/dash', 'ethereum/go-ethereum',
                         'ripple/rippled', 'neo-project/neo', 'ethereumproject/go-ethereum',
                         'iotaledger/wallet','litecoin-project/litecoin',
                         'monero-project/monero', 'zcoinofficial/zcoin')

listcryptos <- data.frame(crypto_currencies, github_repositories)

# Download the homepage of coinmarketcap
coinmarket_cap_result <- GET(build_url(COINMARKET_CAP_URL))

# Parse the homepage
html_coinmarket_cap <- read_html(coinmarket_cap_result)

# Prepare output
outputColumnNames <- c("Name", "24hPerVariation", "TweetCount", "FollowersCount", "PublicAppearance", "WasCommitted")
variables <- length(outputColumnNames)
iterations <- length(listcryptos[,1])
output <- matrix(ncol=variables, nrow=iterations)

# For each crypto, make a list of actions
 for (i in 1:length(listcryptos[,1])) {
# for (i in 1:1) {
  # Build xpath to get the link of the details on the crypto 
  xpath <- paste("//*[text()='", listcryptos[i,]$crypto_currencies, "'][contains(@class, 'currency-name-container')]",
                 sep = "")
  
  # Extract the link from the homepage of coinmarketcap
  cryptonode <- html_nodes(html_coinmarket_cap, xpath = xpath)
  cryptolink <- html_attr(cryptonode, "href")
  
  # Build the link on coinmarketcap
  COINMARKET_CAP_URL$path <- cryptolink
  URL <- build_url(COINMARKET_CAP_URL)
  
  # Then visit the link
  currency_page_result <- GET(URL)
  currency_page_html <- read_html(currency_page_result)
    
  # Extract daily variation in percentage from API
  api_link_xpath <- "//*[@id='tools']//*[contains(text(), 'api.coinmarketcap.com')]"
  api_node <- html_nodes(currency_page_html, xpath = api_link_xpath)
  api_url <- html_attr(api_node, 'href')
  brute_json <- jsonlite::fromJSON(api_url)
  percent_change_24 <- brute_json$percent_change_24h
  
  # Extract twitter official account's name
  twitter_name_xpath <- "//*[contains(@class, 'twitter-timeline')]"
  twitter_node <- html_nodes(currency_page_html, xpath = twitter_name_xpath)
  twitter_username <- html_attr(twitter_node, 'data-screen-name')
  
  # Get data of interest from this account
  user <- getUser(twitter_username)
  tweetNumber <- user$statusesCount
  followersCount <- user$followersCount
  public_appearance <- user$listedCount
  
  # Check if a commit has been made today
  GITHUB_API$path <- paste("repos", listcryptos[i,]$github_repositories, "commits", sep = "/")
  URL <- build_url(GITHUB_API)
  commits_json <- jsonlite::fromJSON(URL)
  last_commit_difference <- difftime(as.POSIXct(commits_json$commit$committer$date[1]), as.POSIXct(Sys.Date()), units="days")
  committed_today <- last_commit_difference >= -1
  
  # Put in matrix
  output[i,] <- c(crypto_currencies[i], percent_change_24, tweetNumber, followersCount, public_appearance, committed_today)
}

# Format output
today <- Sys.Date()
output <- data.frame(output)
colnames(output) <- outputColumnNames
output$Date <- today
fileName <- paste(today, "cryptos_activity", ".json", sep = "_")
# fullPath <- file.path("data", fileName)
write(toJSON(output), file=fileName)
