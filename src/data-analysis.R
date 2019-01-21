library(ggplot2)
library(ggExtra)

qualis_filename <- "../data/Brazil_CAPES_evaluations.csv"
qualis_data <- read.csv2(file=qualis_filename, sep=',')
colnames(qualis_data) <- c('Issn', 'Title', 'field', 'BR_score')
qualis_data$Issn <- gsub('-', '', qualis_data$Issn)
# Convert A1 to 8, and C to 1. A1 is the best one score.
qualis_data$BR_score <- as.integer(qualis_data$BR_score)*(-1)+9

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


# ~~~

cor(data$BR_score, data$SJR, use = "pairwise.complete.obs", method="spearman") #0.61
cor(data$BR_score, data$H.index, method="spearman") #-0.03
cor(data$BR_score, data$Total.Refs., method="spearman") #0.02
cor(data$BR_score, as.numeric(data$Cites...Doc...2years.), method="spearman") #0.58
cor(data$BR_score, as.numeric(data$Citable.Docs...3years.), method="spearman") #0.07

cor(data$Total.Refs., data$SJR, use = "pairwise.complete.obs") #0.04
cor(as.numeric(data$Citable.Docs...3years.), data$SJR, use = "pairwise.complete.obs") #0.08
cor(as.numeric(data$Cites...Doc...2years.), data$SJR, use = "pairwise.complete.obs") #0.81
cor(as.numeric(data$Ref....Doc.), data$SJR, use = "pairwise.complete.obs") #0.33

# ~~~

min(data[data$BR_score == 8, ]$H.index) # 365
max(data[data$BR_score == 7, ]$H.index) # 706
max(data[data$BR_score == 6, ]$H.index) # 706
max(data[data$BR_score == 5, ]$H.index) # 706
max(data[data$BR_score == 4, ]$H.index) # 706
max(data[data$BR_score == 3, ]$H.index) # 706
max(data[data$BR_score == 2, ]$H.index) # 703
max(data[data$BR_score == 1, ]$H.index) # 702


min(data[data$BR_score == 8, ]$SJR) # 1
max(data[data$BR_score == 7, ]$SJR) # 3155
max(data[data$BR_score == 6, ]$SJR) # 3182
max(data[data$BR_score == 5, ]$SJR) # 2986
max(data[data$BR_score == 4, ]$SJR) # 3127
max(data[data$BR_score == 3, ]$SJR) # 3127
max(data[data$BR_score == 2, ]$SJR) # 3127
max(data[data$BR_score == 1, ]$SJR) # 3127


min(as.numeric(data[data$BR_score == 8, ]$Citable.Docs...3years.)) # 
max(as.numeric(data[data$BR_score == 7, ]$Citable.Docs...3years.)) # 
max(as.numeric(data[data$BR_score == 6, ]$Citable.Docs...3years.)) # 
max(as.numeric(data[data$BR_score == 5, ]$Citable.Docs...3years.)) # 
max(as.numeric(data[data$BR_score == 4, ]$Citable.Docs...3years.)) # 
max(as.numeric(data[data$BR_score == 3, ]$Citable.Docs...3years.)) # 
max(as.numeric(data[data$BR_score == 2, ]$Citable.Docs...3years.)) # 
max(as.numeric(data[data$BR_score == 1, ]$Citable.Docs...3years.)) # 


min(as.numeric(data[data$BR_score == 8, ]$Total.Docs...2016.)) # 
max(as.numeric(data[data$BR_score == 7, ]$Total.Docs...2016.)) # 
max(as.numeric(data[data$BR_score == 6, ]$Total.Docs...2016.)) # 
max(as.numeric(data[data$BR_score == 5, ]$Total.Docs...2016.)) # 
max(as.numeric(data[data$BR_score == 4, ]$Total.Docs...2016.)) # 
max(as.numeric(data[data$BR_score == 3, ]$Total.Docs...2016.)) # 
max(as.numeric(data[data$BR_score == 2, ]$Total.Docs...2016.)) # 
max(as.numeric(data[data$BR_score == 1, ]$Total.Docs...2016.)) # 


min(as.numeric(data[data$BR_score == 8, ]$Total.Docs...2years.)) # 
max(as.numeric(data[data$BR_score == 7, ]$Total.Docs...2years.)) # 
max(as.numeric(data[data$BR_score == 6, ]$Total.Docs...2years.)) # 
max(as.numeric(data[data$BR_score == 5, ]$Total.Docs...2years.)) # 
max(as.numeric(data[data$BR_score == 4, ]$Total.Docs...2years.)) # 
max(as.numeric(data[data$BR_score == 3, ]$Total.Docs...2years.)) # 
max(as.numeric(data[data$BR_score == 2, ]$Total.Docs...2years.)) # 
max(as.numeric(data[data$BR_score == 1, ]$Total.Docs...2years.)) # 


min(as.numeric(data[data$BR_score == 8, ]$Total.Refs.)) # 
max(as.numeric(data[data$BR_score == 7, ]$Total.Refs.)) # 
max(as.numeric(data[data$BR_score == 6, ]$Total.Refs.)) # 
max(as.numeric(data[data$BR_score == 5, ]$Total.Refs.)) # 
max(as.numeric(data[data$BR_score == 4, ]$Total.Refs.)) # 
max(as.numeric(data[data$BR_score == 3, ]$Total.Refs.)) # 
max(as.numeric(data[data$BR_score == 2, ]$Total.Refs.)) # 
max(as.numeric(data[data$BR_score == 1, ]$Total.Refs.)) # 


min(as.numeric(data[data$BR_score == 8, ]$Total.Cites..3years.)) # 
max(as.numeric(data[data$BR_score == 7, ]$Total.Cites..3years.)) # 
max(as.numeric(data[data$BR_score == 6, ]$Total.Cites..3years.)) # 
max(as.numeric(data[data$BR_score == 5, ]$Total.Cites..3years.)) # 
max(as.numeric(data[data$BR_score == 4, ]$Total.Cites..3years.)) # 
max(as.numeric(data[data$BR_score == 3, ]$Total.Cites..3years.)) # 
max(as.numeric(data[data$BR_score == 2, ]$Total.Cites..3years.)) # 
max(as.numeric(data[data$BR_score == 1, ]$Total.Cites..3years.)) # 


