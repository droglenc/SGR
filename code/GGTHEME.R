library(ggplot2)
library(patchwork)

theme_SGR <- theme_bw() +
  theme(legend.position="none",
        axis.title.x = element_text(size=14,face="bold",margin=margin(t=10)),
        axis.title.y = element_text(size=14,face="bold",margin=margin(r=10)),
        axis.text.x = element_text(size=12,margin=margin(t=3)),
        axis.text.y = element_text(size=12,margin=margin(r=3)),
        plot.subtitle = element_text(face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )  

