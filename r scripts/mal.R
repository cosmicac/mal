library('plyr')
library('XML')
library('ggplot2')
setwd('C:/Users/weird_000/mal')

extract.vectors <- function(anime.list) {
  scores <- as.numeric(lapply(anime.list, function(x) x$my_score))
  titles <- as.character(lapply(anime.list, function(x) x$series_title))
  type <- as.character(lapply(anime.list, function(x) x$series_type))
  epi.num <- as.numeric(lapply(anime.list, function(x) x$series_episodes))
  status <- as.character(lapply(anime.list, function(x) x$my_status))
  tags <- as.character(lapply(anime.list, function(x) x$my_tags))
  return(list(scores, titles, type, epi.num, status, tags))
}

xmlListToDataFrame <- function(filename) {
  xml <- xmlTreeParse(filename, options = NOCDATA)
  list <- xmlToList(xml)
  anime.list <- list[-1]
  anime.df <- data.frame(extract.vectors(anime.list))
  names(anime.df) <- c('scores', 'titles', 'type', 'epi.num', 'status', 'tags')
  return(anime.df)
}

createScoreGraph <- function(anime.df, color, status) {
  completed.df <- anime.df[anime.df$status == status,]
  names(anime.df) <- c('scores', 'titles', 'type', 'epi.num', 'status', 'tags')
  scores <- ggplot(data = completed.df, aes(x = factor(scores))) +
            geom_bar(fill = 'cyan2')
  return(scores)
}

cosmicac.df <- xmlListToDataFrame('cosmicac_list.xml')
cosmic.scores <- createScoreGraph(cosmicac.df, 'cyan2', 'Completed')
cosmic.scores

danjot.df <- xmlListToDataFrame('danjot_list.xml')
danjot.scores <- createScoreGraph(danjot.df, 'steelblue4', '2')
danjot.scores

andrew.df <- xmlListToDataFrame('andrewdashiz_list.xml')
andrew.scores <- createScoreGraph(danjot.df, 'steelblue4', '2')
andrew.scores