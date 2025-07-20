# to understand derivatives https://tutorial.math.lamar.edu/classes/calci/derivativeinterp.aspx
#https://www.econometrics-with-r.org/4.2-estimating-the-coefficients-of-the-linear-regression-model.html
library(AER)
library(KernSmooth)
library(ggplot2)
library(dplyr)

# load the the data set in the workspace
data(CASchools)

# compute STR and append it to CASchools
CASchools$STR <- CASchools$students/CASchools$teachers
summary(CASchools$STR)

# compute TestScore and append it to CASchools
CASchools$score <- (CASchools$read + CASchools$math)/2
summary(CASchools$score)

lm.1 <- lm(score ~ STR, data = CASchools)
summary(lm.1)

# calculate alternate smoother (also NW kernel, but using different defaults)
hCA <- dpill(x = CASchools$STR, y = CASchools$score)
fitCA <- locpoly(x = CASchools$STR, y = CASchools$score, bandwidth = hCA)

# prepare loess smoother for plotting
ord <- order(CASchools$STR)

Data = data.frame(rbind(select(CASchools, STR, score),
                        data.frame(STR = fitCA$x, score = fitCA$y),
                        data.frame(STR = CASchools$STR[ord],
                                    score = lm.1$fitted.values[ord])))

Data$smooth <- c(rep('S', nrow(CASchools)),
                    rep('NW', length(fitCA$x)),
                    rep('LM', nrow(CASchools)))

Data$smooth <- factor(Data$smooth)

my_colors2 = c("NW" = "green", "LM" = "orange")
my_lines = c('NW' = 1, 'LM' = 2)

##First the scatter plot
d <- ggplot(Data) +
    geom_point(aes(STR, score),
                    data = . %>% filter(smooth == 'S')) +
    labs(x = 'Student to Teacher Ratio',
        y = 'Test Score',
        title = 'Scatterplot of Test Score and Student Teacher Ratio')

##Now add in the smoothers
d + geom_line(aes(x = STR, y = score, color = smooth, linetype = smooth),
                data = . %>% filter(smooth != 'S'), linewidth = 0.8) +
    scale_colour_manual(values = my_colors2) +
    scale_linetype_manual(values = my_lines)

# calculate derivative smoother
hCA1 <- dpill(x = CASchools$STR, y = CASchools$score)
fitCA1 <- locpoly(x = CASchools$STR, y = CASchools$score,
                drv = 1 ,bandwidth = hCA1)

fitCA2 <- locpoly(x = CASchools$STR, y = CASchools$score,
                drv = 1 ,bandwidth = 1.5*hCA1)

fitCA3 <- locpoly(x = CASchools$STR, y = CASchools$score,
                drv = 1 ,bandwidth = 10)

myq <- quantile(fitCA$x, c(0.05, 0.95))
ind <- (fitCA1$x >= myq[1]) & (fitCA1$x <= myq[2])

Data2 = data.frame(rbind(
                    data.frame(STR = fitCA1$x[ind], score = fitCA1$y[ind]),
                    data.frame(STR = fitCA2$x[ind], score = fitCA2$y[ind]),
                    data.frame(STR = fitCA3$x[ind], score = fitCA3$y[ind])))

Data2$bandwidth <- c(rep('defaultBW', length(fitCA1$x[ind])),
                    rep('1.5*default', length(fitCA2$x[ind])),
                    rep('10', length(fitCA3$x[ind])))

Data2$bandwidth <- factor(Data2$bandwidth,
                        levels = c('defaultBW', '1.5*default', '10'))

my_colors3 = c("defaultBW" = "green", "1.5*default" = "purple", "10" = "orange")
my_lines2 = c("defaultBW" = 1, "1.5*default" = 2, "10" = 3)

d1 <- ggplot(Data2) +
    labs(x = 'Student to Teacher Ratio',
        y = 'Derivative',
        title = 'Estimated Derivative of Test Score and Student Teacher Ratio') +
    geom_line(aes(x = STR, y = score, color = bandwidth, linetype = bandwidth),
                linewidth = 0.8) +
    scale_colour_manual(values = my_colors3) +
    scale_linetype_manual(values = my_lines2) +
    geom_hline(yintercept = 0)

d1

#the global equivalent derivative
summary(fitCA3$y)

#zero crossing of the purple function (1.5*BW)
#the first crossing
min(which(fitCA2$y <= 0))
fitCA2$x[27:28]
fitCA2$y[27:28]
#the second crossing
max(which(fitCA2$y <= 0))
fitCA2$x[298:299]
fitCA2$y[298:299]
