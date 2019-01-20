library(ggplot2)
library(ggExtra)

qualis_filename <- "../data/classificacoes_publicadas_todas_as_areas_avaliacao1522078273541.csv"
qualis_data <- read.csv2(file=qualis_filename, sep=',')
colnames(qualis_data) <- c('Issn', 'Title', 'field', 'BR_score')
qualis_data$Issn <- gsub('-', '', qualis_data$Issn)
# Convert A1 to 8, and C to 1. A1 is the best one score.
qualis_data$BR_score <- as.integer(qualis_data$BR_score)*(-1)+9

# ~~~

international_filename <- "../data/scimagojr 2016.csv"
inter_data <- read.csv2(file=international_filename)
inter_data$SJR <- as.numeric(inter_data$SJR)
inter_data$H.index <- as.numeric(inter_data$H.index)

# ~~~

# Merge two dataframe: qualis data from Brazil and SJR international data
data <- merge(x=qualis_data, y=inter_data, by='Issn') # It was retrieved only 153 journals!
data$BR_score <- as.integer(data$BR_score)
data <- data[data$SJR > 0 & data$H.index > 0, ]

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

g <- ggplot(data, aes(BR_score, H.index)) + 
  geom_count() + 
  geom_smooth(method="lm", se=F)

ggMarginal(g, type = "boxplot", fill="transparent")
ggMarginal(g, type = "histogram", fill="transparent")


# ~~~

cor(data$BR_score, data$SJR, use = "pairwise.complete.obs")
cor(data$BR_score, data$H.index)

# ~~~

