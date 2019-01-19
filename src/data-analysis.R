library('ggplot2')

qualis_filename <- "../data/classificacoes_publicadas_todas_as_areas_avaliacao1522078273541.csv"
international_filename <- "../data/SJR_Scientific_Journal_Rankings-SCImago.csv"

qualis_data <- read.csv2(file=qualis_filename, sep=',')
inter_data <- read.csv2(file=international_filename)

qplot(as.factor(qualis_data$Estrato), geom="histogram") 
       