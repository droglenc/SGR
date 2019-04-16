library(ggplot2)
library(patchwork) ## See this to install ... https://github.com/thomasp85/patchwork#installation

theme_SGR <- theme_bw() +
  theme(legend.position="none",
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.subtitle = element_text(face="bold"),
        panel.grid.major = element_line(size=0.3,linetype="dotted",color="gray90"),
        panel.grid.minor = element_line(size=0.3,linetype="dotted",color="gray90")
  )  

