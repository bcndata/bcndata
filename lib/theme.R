library(ggplot2)

# Set the default theme
theme_set(theme_bw())
# Modify it with theme()
tt <- theme(axis.text = element_text(size=16, colour=NULL),
            axis.title = element_text(size = 20, colour= NULL),
            plot.title = element_text(size = 30),
            # axis.line = element_line(size = 1, colour = "black"),
            axis.text = element_text(colour = "blue"),
            axis.ticks = element_line(size = 2),
            legend.background = element_rect(fill = "white"),
            #legend.position = c(0.14, 0.80),
            panel.grid.major = element_line(colour = "grey80"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            text = element_text(size = 12, family = 'Times New Roman')
            )

bcn_data_theme <- tt
