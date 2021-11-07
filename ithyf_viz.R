ithyf <- read.csv("results_ITHYF.csv")

data <- ithyf[,1:4]
library(tidyverse)
data <- data %>%
  mutate(total = select(., negative:positive) %>% rowSums(na.rm = TRUE))
data <- data[order(data$total, decreasing=TRUE),]
data <- data[,1:4]

library(tidyr)
library(HH)

ITHYF1 <- likert(data , horizontal=TRUE, aspect=1.5,
                 auto.key=list(space="right", columns=1,
                               reverse=TRUE, padding.text=2),
                 main="",
                 col=c("#db7f92", "#d1cfcf", "#8ad18c"))
ITHYF1

###
ithyf5 <- read.csv("results_ITHYF5.csv")

data2 <- ithyf5[,1:4]
library(tidyverse)
data2 <- data2 %>%
  mutate(total = select(., negative:positive) %>% rowSums(na.rm = TRUE))
data2 <- data2[order(data2$total, decreasing=TRUE),]
data2 <- data2[,1:4]

library(tidyr)
library(HH)

ITHYF2 <- likert(data2, horizontal=TRUE, aspect=1.5,
                 auto.key=list(space="right", columns=1,
                               reverse=TRUE, padding.text=2),
                 main="",
                 col=c("#db7f92", "#d1cfcf", "#8ad18c"))
ITHYF2

### wishful vs logical hope
ithyf_hope <- colSums(ithyf[,5:6])
ithyf5_hope <- colSums(ithyf5[,5:6])
hope <- as.data.frame(rbind(ithyf_hope, ithyf5_hope))
hope$source <- rownames(hope)
hope <- hope %>%
  pivot_longer(., !source, names_to = 'hope_type', values_to = 'count')
hope$hope_type <- as.factor(hope$hope_type)

ggplot(data=hope, aes(x=source, y=count, fill=hope_type)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="black" ) 
  ) +
  xlab("Letter series") + ylab("Frequency") + labs(fill = "Hope type") +
  scale_x_discrete(labels=c('ITHYF', 'ITHYF5')) +
  scale_fill_manual(values = c("#fcd977", "#bf84d1", "#fcd977", "#bf84d1"))
