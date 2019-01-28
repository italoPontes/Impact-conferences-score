library(ggplot2)
library(ggExtra)
library(plotly)
library(plyr)

qualis_filename <- "../data/Brazil_CAPES_evaluations.csv"
qualis_data <- read.csv2(file=qualis_filename, sep=',')
colnames(qualis_data) <- c('Issn', 'Title', 'field', 'BR_score')
qualis_data$Issn <- gsub('-', '', qualis_data$Issn)
# Convert A1 to 8, and C to 1. A1 is the best one score.
qualis_data$BR_score <- as.integer(qualis_data$BR_score)*(-1)+9
qualis_data[qualis_data$field=="ENGENHARIAS II", ]

# ~~~

international_filename <- "../data/Scimagojr2016.csv"
inter_data <- read.csv2(file=international_filename)
inter_data$SJR <- as.numeric(inter_data$SJR)
inter_data$H.index <- as.numeric(inter_data$H.index)

# ~~~

# Merge two dataframe: qualis data from Brazil and SJR international data
data <- merge(x=qualis_data, y=inter_data, by='Issn') # It was retrieved only 153 journals!
data$BR_score <- as.integer(data$BR_score)
data$Total.Refs. <- as.integer(data$Total.Refs.)

# ~~~

data_to_show <- count(qualis_data$BR_score)
data_to_show$type <- 'Original'
aux <- count(data$BR_score)
aux$type <- 'Post-Filtered'
data_to_show <- rbind(data_to_show, aux)
ggplotly(ggplot(data_to_show, aes(x=as.factor(x), y=freq, fill=type))+
           geom_bar(stat = "identity", position=position_dodge2(), alpha=.9) +
           xlab("Level") + ylab("Quantity") +
           ggtitle("Journals score distribuition") +
           scale_fill_discrete(name = "Category") +
           theme_light())

# ~~~

ggplot(data, aes(x=data$BR_score)) +
  labs(title = "Journals quality histogram - Brazilian evaluation",
       x = "Category", y = "Quantity") + 
  geom_bar() + theme_light()


ggplot(data, aes(x=data$SJR)) +
  labs(title = "Journals quality histogram - SJR evaluation",
       x = "Category", y = "Quantity") + 
  geom_bar() + theme_light()

ggplot(data, aes(x=data$H.index)) +
  labs(title = "Journals quality histogram - SJR evaluation",
       x = "Category", y = "Quantity") + 
  geom_bar() + theme_light()

# ~~~

g <- ggplot(data, aes(BR_score, SJR)) + 
  geom_count() + 
  geom_smooth(method="lm", se=F)

ggMarginal(g, type = "boxplot", fill="transparent")
ggMarginal(g, type = "histogram", fill="transparent")

ggplot(data, aes(x=as.factor(BR_score), y=SJR)) +
  geom_boxplot()+ theme_bw()

# ~~~

cor(data$Total.Refs., data$SJR, use = "pairwise.complete.obs") #0.04
cor(as.numeric(data$Citable.Docs...3years.), data$SJR, use = "pairwise.complete.obs") #0.08
cor(as.numeric(data$Cites...Doc...2years.), data$SJR, use = "pairwise.complete.obs") #0.81
cor(as.numeric(data$Ref....Doc.), data$SJR, use = "pairwise.complete.obs") #0.33

# ~~~





