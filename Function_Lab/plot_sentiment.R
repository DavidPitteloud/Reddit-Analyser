library("plotly")
library("RColorBrewer")
library("tidyverse")

df <- Greta_news_comments_week

plot_sentiments_reddit <- function(df) {
  
  contenu_wordcloud <- df %>%
    tibble::as_tibble() %>%
    tidytext::unnest_tokens(word, comment) %>%
    dplyr::filter(is.na(as.numeric(word)))
  
  
  ##SentimentAnalysis:
  
  contenu_sentiments <- contenu_wordcloud %>%
    dplyr::inner_join(tidytext::get_sentiments("nrc"), by = "word") %>%
    dplyr::group_by(sentiment) %>%
    dplyr::count()
  
  plot_sentiment <- ggplot2::ggplot(contenu_sentiments,
                                    ggplot2::aes(x = sentiment, y = n, fill = sentiment)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::labs(x = "", y = "Number of words", fill = "Sentiment")+
    scale_fill_brewer(palette="RdYlGn")
  ggplotly(plot_sentiment)
}

plot_sentiments_reddit(df)
