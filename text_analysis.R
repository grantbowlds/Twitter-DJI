library(dplyr)
library(ggplot2)

# trumptweets <- read.csv("~/Documents/2020 - Spring/MATH2820L/Final Project/trumptweets.csv")
DJI <- read_csv(url("https://raw.githubusercontent.com/grantbowlds/Twitter-DJI/master/Data/%5EDJI.csv"))

names(trumptweets)[10] <- "sentiment"
names(trumptweets)[11] <- "market_check"
names(trumptweets)[12] <- "intl_check"

text_results <- trumptweets %>%
  mutate(Date = as.Date(date)) %>%
  group_by(Date) %>%
  summarise(market_mentions = sum(market_check),
            intl_mentions = sum(intl_check),
            sentiment_ratio = 
              length(sentiment[sentiment=="positive"])/ length(sentiment))

DJI_text <- right_join(DJI, text_results)
DJI_text <- na.omit(DJI_text)

DJI_text <- DJI_text %>%
  mutate(net_market = Close - Open) %>% 
  mutate(market_groups = ifelse(market_mentions == 0, 0, 
                                ifelse(market_mentions >= 1 & market_mentions < 5, 1, 2)),
         intl_groups = ifelse(intl_mentions == 0, 0, 
                              ifelse(intl_mentions >= 1 & intl_mentions < 5, 1, 2)))
#----------------------------------------------------------------------------------------------------------------------

# Market Groups --> Net Market
ggplot(DJI_text, aes(y = net_market, x = sentiment_ratio, color = factor(market_groups))) + 
  geom_point(alpha = 0.5) + 
  scale_color_manual(values = c("blue","purple","red")) + 
  facet_wrap( ~ factor(market_groups))
# Market Groups --> Volume
ggplot(DJI_text, aes(y = Volume, x = sentiment_ratio, color = factor(market_groups))) + 
  geom_point(alpha = 0.5) + 
  scale_color_manual(values = c("blue","purple","red")) + 
  facet_wrap( ~ factor(market_groups))

# Intl Groups --> Net Market
ggplot(DJI_text, aes(y = net_market, x = sentiment_ratio, color = factor(intl_groups))) + 
  geom_point(alpha = 0.5) + 
  scale_color_manual(values = c("blue","purple","red")) + 
  facet_wrap( ~ factor(intl_groups))
# Intl Groups --> Volume
ggplot(DJI_text, aes(y = Volume, x = sentiment_ratio, color = factor(intl_groups))) + 
  geom_point(alpha = 0.5) + 
  scale_color_manual(values = c("blue","purple","red")) + 
  facet_wrap( ~ factor(intl_groups))

# These plots all seem to suggest that there is no relationship between tweet mentions
# and market performance or trading volume
#----------------------------------------------------------------------------------------------------------------------

# Regression
# Net Market
model1 <- lm(net_market ~ sentiment_ratio + market_mentions + intl_mentions, data = DJI_text)
summary(model1)
# None are significant at 5% level

# Volume
model2 <- lm(Volume ~ sentiment_ratio + market_mentions + intl_mentions, data = DJI_text)
# market mentions, intl mentions not significant
# sentiment is significant on volume at the 5% level, let's examine this further

# Tweet sentiment is significant on volume at the 5% significance level
ggplot(DJI_text, aes(x = sentiment_ratio, y = Volume)) + 
  geom_point() + geom_smooth(method="lm")

mod2_results <- data.frame(sentiment_ratio = DJI_text$sentiment_ratio,
                            observed = DJI_text$Volume, 
                            predicted = model2$fitted.values,
                            residual = model2$residuals)
# Remove outlier
mod2_results <- mod2_results[-652,] 
model_volume <- lm(observed ~ sentiment_ratio, data = mod2_results)
summary(model_volume)
# Now signficant at the 1% level

# residual plot
ggplot(mod2_results, aes(y = residual, x = predicted)) + 
  geom_point() + 
  geom_hline(yintercept = 0)
# qq plot
ggplot(mod2_results, aes(sample = residual)) + 
  geom_qq()

# model appears to satisfy normality requirements


# Conclusion: We can somewhat use a linear model with tweet sentiment to predict the volume of trading 
# but not market performance. Additionally, it appears that tweets mentioning the market or international 
# issues impact neither market performance or volume. 















