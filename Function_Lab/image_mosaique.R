library("jpeg")
library("magick")
library("magrittr")
library("png")
library("projectg1ptds")
library("RCurl")
library("rsvg")
library("rvest")
library("stringr")
library("tibble")
library("xml2")


inkart<-projectg1ptds::scrapping_function("ink","Art","new","week")
######
df<-inkart
#function select image with regex 

intake<-str_match(df$link, ".*jpg")%>%unique()
intake<-intake[-1,]
intake<-as_tibble(intake)

intake$link<-intake$value
read_image_wrapper <- function(url) image_read(url)
images_list <- purrr::map(intake$value, read_image_wrapper)
img<-image_join(images_list)

#intake$link<-paste("read_image('",intake$value,"')", collapse = NULL)


#image_append(image_scale(img, "100"), stack = TRUE)

image_montage(img)



montage <- function(df) {
  
  if(any(grepl(".*jpg$", df[,16]) == FALSE)){
    df <- df[grepl(".*jpg$", df[,16]),]
  }
  
  if(dim(df)[1] == 0){
    stop("No image link in this dataframe")
  }
  
  #Create new dataframe to not touch the original one
  inkart <- df 
  #Select unique links to image.
  intake <- stringr::str_match(inkart$link, ".*jpg") %>% unique()
  
  intake <- intake[-1, ]
  intake <- tibble::as_tibble(intake)
  
  read_image_wrapper <- function(url) image_read(url)
  images_list <- purrr::map(intake$value, read_image_wrapper)
  img <- magick::image_join(images_list)
  
  
  magick::image_montage(img)
}

montage(df)