# June 15 in-class code

# 1. how to do "pairs"
# correlation function for "pairs"
# (copied from the help doc for the "pairs" function)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# do it for the crabs dataset
library(MASS)
data('crabs')
pairs(crabs[,4:8], pch=16, col='darkblue', lower.panel = panel.cor)


# 2. fit multiple linear regression, and do ANOVA on a MLR
data('Boston')
m1 = lm(medv ~ crim + chas, data=Boston)
summary(m1)
anova(m1)

# 3. stepwise selection
## backward
m_full = lm(medv ~ ., data=Boston)
step(m_full, direction = 'backward')

## forward
m_empty = lm(medv ~ 1, data=Boston)
step(m_empty, scope=formula(m_full), direction = 'forward')

# 4. checking conditions
m_best = lm(formula = medv ~ crim + zn + chas + nox + rm + dis + rad + 
              tax + ptratio + black + lstat, data = Boston)
plot(m_best)
