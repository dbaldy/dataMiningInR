# R file used for regressing consecutively the data
path <- "~/data/"

file.names <- dir(path, pattern = ".json")

generalFrame <- data.frame()

# Loop through the files to build a single data frame that contains all the daily data
for (i in file.names) {
  datafile <- fromJSON(i)
  generalFrame <- rbind(generalFrame, datafile)
}

crypto_currencies <- c('Bitcoin', 'Dash', 'Ethereum', 'Ripple', 
                       'NEO', 'Ethereum Classic', 'IOTA', 'Litecoin',
                       'Monero', 'ZCoin')

for (crypto in crypto_currencies) {
  # For each crypto, create a formatted data frame with the correct data
  crypto_frame <- generalFrame[generalFrame$Name==crypto,
                               c('Date', 'Name', 'TweetCount', 'FollowersCount', 'PublicAppearance', 
                                 'WasCommitted', '24hPerVariation')]
  
  daily_columns <- c(3, 4, 5)
  for (col in daily_columns) {
    for (lineNb in length(crypto_frame[,1]):2) {
      crypto_frame[lineNb,col] <- crypto_frame[lineNb,col] - crypto_frame[lineNb - 1,col]
    }
  }
  
  crypto_frame <- crypto_frame[-1,]
  
  # Regress the data
  fit <- lm(`24hPerVariation`~ TweetCount + FollowersCount + PublicAppearance + WasCommitted, 
            data = crypto_frame)
  
  # Display
  print(crypto)
  print(summary(fit))
}

# General regression
crypto_frame <- generalFrame[, c('Date', 'Name', 'TweetCount', 'FollowersCount', 
                                 'PublicAppearance', 'WasCommitted', '24hPerVariation')]
fit <- lm(`24hPerVariation`~ TweetCount + FollowersCount + PublicAppearance + WasCommitted, 
          data = crypto_frame)
print('All cryptos')
print(summary(fit))
