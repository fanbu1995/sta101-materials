# 6/17/2021
# STA101 final exam questions on MLR

library(fivethirtyeight)
library(xtable)
library(ggplot2)
library(gridExtra)
data('candy_rankings')

#glimpse(candy_rankings)

# fit the model
model = lm(winpercent ~ chocolate + bar + sugarpercent + pricepercent,
           data = candy_rankings)

summary(model)

# EDA plots
ggplot(data = candy_rankings, aes(x=sugarpercent, y=winpercent)) +
  geom_point() +
  stat_smooth(method="lm") +
  theme_bw(base_size = 14)

ggplot(data = candy_rankings, aes(x=pricepercent, y=winpercent)) +
  geom_point() +
  stat_smooth(method="lm") +
  theme_bw(base_size = 14)

# diagnostic plots
mod = data.frame(index = 1:nrow(candy_rankings), 
                 fitted = model$fitted.values,
                 residuals = model$residuals)
p1 = 
ggplot(data = mod, aes(x=residuals)) +
  geom_histogram(fill='skyblue', bins=15) +
  labs(x='Residuals', title='Histogram of residuals') +
  theme_bw(base_size = 14)

p2 = 
ggplot(data = mod, aes(sample=residuals)) +
  stat_qq() +
  stat_qq_line()+
  labs(x='theoretical normal quantiles', y='standardized residuals',
       title = 'Q-Q plot of residuals') +
  theme_bw(base_size = 14)

p3 = 
ggplot(data = mod, aes(y=residuals, x=fitted)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x= "Fitted values", y = "Residuals", title='Residuals vs. Fitted values') +
  theme_bw(base_size = 14)

p4 = 
ggplot(data = mod, aes(y=residuals, x=index)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x= "Index of observations", y = "Residuals", 
       title='Residuals vs. Index of observations') +
  theme_bw(base_size = 14)

grid.arrange(p1, p2, p3, p4, nrow=2)

