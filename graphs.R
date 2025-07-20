png('Smooth.png', 850, 850)

##First the scatter plot
d <- ggplot(Data) +
    geom_point(aes(STR, score),
                    data = . %>% filter(smooth == 'S')) +
    labs(x = 'Student to Teacher Ratio',
        y = 'Test Score',
        title = 'Scatterplot of Test Score and Student Teacher Ratio')

##Now add in the smoothers
d + geom_line(aes(x = STR, y = score, color = smooth, linetype = smooth),
                data = . %>% filter(smooth != 'S'), linewidth = 1.2) +
    scale_colour_manual(values = my_colors2) +
    scale_linetype_manual(values = my_lines) +
    theme_grey(base_size = 22)

dev.off()

png('Deriv.png', 850, 850)

d1 <- ggplot(Data2) +
    labs(x = 'Student to Teacher Ratio',
        y = 'Derivative',
        title = 'Estimated Derivative of Test Score vs Student Teacher Ratio') +
    geom_line(aes(x = STR, y = score, color = bandwidth, linetype = bandwidth),
                linewidth = 1.2) +
    scale_colour_manual(values = my_colors3) +
    scale_linetype_manual(values = my_lines2) +
    geom_hline(yintercept = 0)

d1 + theme_grey(base_size = 22)

dev.off()
