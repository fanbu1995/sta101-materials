
# 05/10/2021: make plots for the mid-term exam of STA 101

setwd('~/Downloads/sta101-materials/exams/')

# crabs plot
library(MASS)
library(ggplot2)
data("crabs")

ggplot(data=crabs, aes(x=RW, y=FL)) +
  geom_point() +
  labs(x='rear width (mm)', y='frontal lobe size (mm)') +
  theme_bw(base_size=14)


# mix and match for data distributions
python_blue = "#1F77B4"

set.seed(42)
pdf('data_dist_plots.pdf', width = 5, height = 5)

## bi-modal
x1 = c(rnorm(200, 5,1),rnorm(200, 1,1))

hist(x1, breaks=30, col = python_blue, main="", yaxt = "n", 
     ylab=NULL, xlab=NULL)

## multi-modal
x2 = c(rnorm(150, 5, 1), rnorm(150, 1,1), rnorm(150,-3,1))

hist(x2, breaks=30, col = python_blue, main="", yaxt = "n", 
     ylab=NULL, xlab=NULL)

## right-skewed
x3 = rchisq(400, 2)

hist(x3, breaks=30, col = python_blue, main="", yaxt = "n", 
     ylab=NULL, xlab=NULL)

## uniform
x4 = runif(500, 0, 5)

hist(x4, breaks=15, col = python_blue, main="", yaxt = "n", 
     ylab=NULL, xlab=NULL)

dev.off()

