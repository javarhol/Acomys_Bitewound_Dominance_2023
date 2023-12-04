
## libraries
library(tidyverse)
library(cowplot)
library(ggbeeswarm)

## data
inventory <- read.csv("Animalnventory.csv")
data <- read.csv("rawdata.csv")

## add sex to data
data <- data %>% 
  left_join(inventory[,c(1,2,9)], by = "CageID") %>% 
  mutate(Sex = case_when(Sex == "F" ~ "Females",
                         Sex == "M" ~ "Males",
                         Sex == "B" ~ "Breeders"))
  
## number of male and female groups (14 F, 22 M, 10 B)
inventory %>% 
  count(Sex == "F")

inventory %>% 
  count(Sex == "M")

inventory %>% 
  count(Sex == "B")

## Number wounded per sex (F = 7, M = 10, B = 4)

tmp <- data %>% 
  mutate(Wnd = "Wounded")

grpdata <- inventory %>% 
  select(CageID, Sex, GroupSize, Age) %>% 
  left_join(tmp[,c(2,11)], by = "CageID") %>% 
  mutate(Sex = case_when(Sex == "F" ~ "Females",
                         Sex == "M" ~ "Males",
                         Sex == "B" ~ "Breeders")) %>% 
  distinct()

grpdata[is.na(grpdata)] <- "Not Wounded"

grpdata %>%
  filter(Wnd == "Wounded") %>% 
  distinct() %>% 
  group_by(Sex) %>% 
  summarise(n = n()) 

## Plots

grpdata$Sex <- factor(grpdata$Sex, levels = c("Males", "Females", "Breeders"))

### Wounded vs Not Wounded by Sex
p1 <- ggplot(grpdata, aes(Sex, fill=Wnd)) +
  geom_bar(position="fill", color="black") +
  scale_y_continuous(expand = c(0,0), breaks = c(0,.25,.5,.75,1),
                     labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme_cowplot() +
  ylab("Cages \nwith Wounds") + xlab("") +
  scale_fill_manual(values = c("white", "grey")) +
  labs(fill = NULL) +
  theme(legend.position = "none")
p1

age <- ggplot(grpdata, aes(Wnd, (Age/30))) +
  stat_summary(geom = "bar", position = "identity", fun = "mean", color = "black", fill = "grey", alpha = 0.8)+
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.2) +
  geom_beeswarm(cex = 3) +
  scale_y_continuous(limits = c(0,35),
                     expand = expansion(mult = c(0, 0.05)),
                     breaks = seq(0,36, by = 3)) +
  xlab("") + ylab("Age in months") + theme_minimal_vgrid() + coord_flip()
age

### Wound Score by Sex

data$Sex <- factor(data$Sex, levels = c("Males", "Females", "Breeders"))

ggplot(data, aes(Sex, fill=as.integer(Score), group = Score)) +
  geom_bar(position="fill", color = "#100F0F") +
  scale_y_continuous(expand = c(0,0), breaks = c(0,.25,.5,.75,1),
                     labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme_cowplot() +
  ylab("Percent Cages with Wounds") + xlab("") +
  scale_fill_gradient(name = "Score", low = "white", high = "#100F0F")

ggplot(data, aes(Sex, Score)) +
  stat_summary(geom = "bar", position = "identity", fun = "mean") +
  geom_beeswarm(cex = 2, size = 3) +
  scale_y_continuous(expand = c(0,0), limits = c(0,12.5), breaks = seq(1:12)) +
  theme_cowplot() +
  ylab("Total Wound Score") + xlab("")

p2 <- ggplot(data, aes(Sex, Score)) +
  geom_violin(fill = "grey") +
  stat_summary(fun = "mean", geom = "point", size = 2) +
  scale_y_continuous(expand = c(0,0), limits = c(0,12), breaks = seq(0:12)) +
  theme_cowplot() + 
  ylab("Total \nWound Score") + xlab("") + theme(legend.position = "none")
p2

age2 <- ggplot(data, aes(Age/30, Score)) +
  geom_point()+
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0,12), breaks = seq(0:12),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(limits = c(0,35),
                     expand = expansion(mult = c(0, 0.05)),
                     breaks = seq(0,36, by = 3)) +
  theme_minimal_vgrid() +
  ylab("Total Wound Score") + xlab("Age in months")
age2

data_long <- data %>% 
  gather(key = "Wound Metric", value = "value", 
                    Character, Area, Deepness, Region) %>% 
  left_join(inventory[,c(1,3)])

p3 <- ggplot(data_long, aes(Sex, value)) +
   stat_summary(geom = "bar", position = "identity", fun = "mean", color = "black", fill = "grey") +
   stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.2) +
   scale_y_continuous(expand = c(0,0), limits = c(0,3)) +
   ylab("Score") + xlab("") +
   theme_cowplot() + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1), legend.position = "none") +
   facet_grid(cols = vars(`Wound Metric`)) + panel_border()
p3
  
toprow <- plot_grid(p1, p2,
          ncol = 2,
          labels = "AUTO")

plot_grid(toprow, p3, labels = c('', 'C'), ncol = 1)

ggsave("woundplot.png", dpi=300, width=180, height=180, unit="mm")

plot_grid(age, age2, ncol = 1, labels = "AUTO")

ggsave("agewound.png", dpi=300, width=180, height=180, unit="mm")

#stats
##total score
hist(data$Score)
shapiro.test(data$Score)

kruskal.test(Score ~ Sex, data = data)
library(FSA)
dunnTest(Score ~ Sex, data = data, method = "holm")

##woundmetrics
hist(filter(data_long, `Wound Metric` == "Area")$value)
shapiro.test(filter(data_long, `Wound Metric` == "Area")$value)

kruskal.test(value ~ Sex, data = filter(data_long, `Wound Metric` == "Area"))
kruskal.test(value ~ Sex, data = filter(data_long, `Wound Metric` == "Character"))
kruskal.test(value ~ Sex, data = filter(data_long, `Wound Metric` == "Deepness"))
kruskal.test(value ~ Sex, data = filter(data_long, `Wound Metric` == "Region"))

dunnTest(value ~ Sex, data = filter(data_long, `Wound Metric` == "Region"), method = "holm")

kruskal.test(Age ~ Wnd, data = grpdata)
cor.test(data$Score, data$Age, method = "spearman")
