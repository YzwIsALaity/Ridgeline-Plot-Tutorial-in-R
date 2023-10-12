require(gridExtra)
Dt <- read.csv('Simulated TNF-alpha.csv')
Dt$Time <- factor(Dt$Time)
Dt$'Numerical_PTID' <- rep(1:length(unique(Dt$PTID)), 7)
head(Dt)

require(ggplot2)
require(ggthemes)
require(ggridges)

# Basic ridgeline
p1 <- 
  ggplot(Dt, aes(x = Numerical_PTID, 
                 y = Time)) + 
  geom_ridgeline(aes(height = TNF_alpha),
                 scale = 0.001,  
                 fill = "lightblue") + 
  ggtitle('Basic ridgeline') + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + 
  theme_ridges(center_axis_labels = TRUE)

# Ridgeline for density
p2 <- 
  ggplot(Dt, aes(x = TNF_alpha, y = Time)) + 
  geom_density_ridges(scale = 5, 
                      rel_min_height = 0.001, 
                      fill = "lightblue", 
                      quantiles = c(0.25, 0.5, 0.75), 
                      quantile_lines = TRUE) + 
  ggtitle('Density ridgeline') + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + 
  theme_ridges(center_axis_labels = TRUE)

# Ridgeline for density with jittered points
p3 <- 
  ggplot(Dt, aes(x = TNF_alpha, y = Time)) + 
  geom_density_ridges(scale = 5, 
                      rel_min_height = 0.0001, 
                      fill="lightblue", 
                      jittered_points = TRUE, 
                      position = "raincloud") + 
  ggtitle('Density ridgeline with jittered points') + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + 
  theme_ridges(center_axis_labels = TRUE)

# Ridgeline in binline version
p4 <- 
  ggplot(Dt, aes(x = TNF_alpha, y = Time)) + 
  geom_density_ridges(stat = "binline", 
                      scale = 3, 
                      rel_min_height = 0.0001, 
                      fill="lightblue") +
  ggtitle('Ridgeline in bins') + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) + 
  theme_ridges(center_axis_labels = TRUE)

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

# Texts for y axis
yText <- c('At enrollment', paste0(c(30, 90, 180, 360, 540, 720), '-day'))

p5 <- 
  ggplot(data = Dt, aes(x = TNF_alpha,             # x-axis: value of TNF-alpha
                        y = Time,                  # y-axis: time point
                        fill = after_stat(x))) + 
  geom_density_ridges_gradient(scale = 4) + 
  scale_fill_gradient2_tableau() + 
  scale_y_discrete(expand = c(0, 0), 
                   breaks = c(0, 30, 90, 180, 360, 540, 720), 
                   label = yText) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = expression("Mean TNF"*alpha ~ '(pg/mL)'),          # x-axis label
       fill = expression("TNF"*alpha ~ '(pg/mL)'),            # legend title
       title = expression('Density of' ~ "TNF"*alpha ~ '(pg/mL) at Different Time Points')) +     
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_blank(),                       # Remove vertical gridlines
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_blank(),
        axis.text.x = element_text(colour = "black"),               # there two are for texts in axes
        axis.text.y = element_text(colour = "black"),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(colour = "black", vjust = -1),                              
        axis.title.y = element_text(colour = "black"),
        legend.title = element_text(colour = "black"),
        legend.text = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5))
p5

nObs <- 1000
nTime <- 100

Mean <- 100
SD <- 1

Dt_Plot <- data.frame()
for(i in 1:nTime){
  NoiseForMean <- rnorm(1, mean = 5, sd = 2)
  NoiseForSD <- rnorm(1, mean = 2, sd = 0.5)
  MeanForGenerate <- Mean + NoiseForMean
  SDForGenerate <- SD + NoiseForSD
  Value <- rnorm(n = nObs, mean = MeanForGenerate, sd = SDForGenerate)
  Trans <- data.frame('Row' = paste0('X', i), 'Value' = Value)
  Dt_Plot <- rbind(Dt_Plot, Trans)
}

p <-
  ggplot(data = Dt_Plot, aes(x = Value, y = Row)) + 
  geom_density_ridges(scale = 10, fill = 'white') +
  theme_bw() + 
  theme(# panel.background = element_rect(fill = 'black'),
    panel.border = element_blank(),   
    panel.grid = element_blank(),
    axis.text.x = element_blank(),  
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),                              
    axis.ticks.y = element_blank(),                  
    axis.title.x = element_blank(),                              
    axis.title.y = element_blank(),
    legend.title = element_text(color = 'black', face = 'bold'),
    plot.title = element_text(hjust = 0.5, face = 'bold'))
p


