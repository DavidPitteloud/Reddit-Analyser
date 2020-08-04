#Wordcloud function 

df <- scrapping_function("trump","news","comments","week")

plot_wordcloud_reddit <- function(df) {
  
  
  contenu_wordcloud <- df %>%
    tibble::as_tibble() %>%
    tidytext::unnest_tokens(word, comment) %>%
    dplyr::filter(is.na(as.numeric(word)))
  
  contenu_wordcloud %>%
    dplyr::count(word) %>%
    with(wordcloud::wordcloud(
      word,
      n,
      max.words = 50,
      colors = RColorBrewer::brewer.pal(8, "Spectral")
    ))
}

plot_wordcloud_reddit(df)