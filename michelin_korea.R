library(rvest)
library(rJava)
library(KoNLP)
library(doBy)
library(wordcloud)
library(wordcloud2)
library(ggmap)
library(stringr)


# data crawling
name <- c()
cuisine <- c()
url <- c()

for(i in 1:9){
  html <- read_html(paste0("https://guide.michelin.co.kr/ko/restaurant/page/",i))
  
  # name
  name <- c(name,  
            html_nodes(html,".restaurant-list") %>%
            html_nodes(".restaurant-list-title") %>%
            html_text())
  
  # cuisine
  cuisine <- c(cuisine,
               html_nodes(html,".restaurant-list") %>%
               html_nodes(".restaurant-list-category") %>%
               html_text())
  
  # url
  url <- c(url,
           html_nodes(html,".restaurant-list") %>%
           html_nodes(".restaurant-list-title") %>%
           html_nodes("a") %>%
           html_attr("href"))
  
}

address <- c()
mention <- c()
distiction <- c()
quote <- c()

for(i in 1:length(url)){
  html <- read_html(url[i])
  
  # address
  address <- c(address, 
    html_nodes(html,".list-type.type2") %>%
    html_node(".list-item-content") %>%
    html_text())
  
  # mention
  mention <- c(mention,
               html_node(html,".restaurant-system") %>%
               html_node("td") %>%
               html_text())
    
  # distiction
  temp <- as.character(html_node(html,".restaurant-system") %>%
                       html_node("i"))
  
  distiction <- c(distiction,
                  unlist(strsplit(temp,split = "icon-"))[3])
  
  # quatation
  quote <- c(quote,
             html_node(html,".quotation") %>%
               html_text())

}


# michelin data
michelin <- data.frame(name,
                       cuisine ,
                       distiction,
                       address,
                       mention,
                       stringsAsFactors = F)



# which words is mentioned? - wordcloud
## word
useSejongDic()
ext_word <- unlist(extractNoun(quote))
freq_word <- table(ext_word)
df_word <- data.frame(freq_word)
df_word_200 <- head(orderBy(~-Freq,df_word),200)
rownames(df_word_200) <- NULL
delete <- c(1,3,4,7,12,13,15,16,17,18,19,22,25,26,
            28,32,43,48,49,56,62,64,73,75,83,85,90)
wordcloud2(df_word_200[-delete,])

## cuisine
freq_cuisine <- table(michelin$cuisine)
wordcloud2(data.frame(type = names(freq_cuisine),
                      freq = as.numeric(freq_cuisine)))

## distinction
par(mfrow = c(1,2))

freq_star <- tapply(michelin$name, michelin$distiction, length)
wordcloud(words = names(freq_star),
          freq = freq_star,
          scale = c(5,1),
          min.freq = 1,
          random.order = F,
          rot.per = 0.1,
          colors = brewer.pal(8,"Dark2"))

wordcloud(words = names(freq_cuisine),
          freq = freq_cuisine,
          scale = c(5,0.5),
          min.freq = 1,
          random.order = F,
          rot.per = 0.1,
          colors = brewer.pal(8,"Dark2"))




# michelin map in seoul
## map
center <- geocode(enc2utf8("N서울타워"))
map <- get_googlemap(as.numeric(center),
                     zoom = 12,
                     maptype = "roadmap")

## color function
col <- function(...){
  temp <- c(...)
  c <- c()
  s <- c()
  for(i in temp){
    if(str_detect(i,"star")){
      c <- c(c,"red")
      if(str_detect(i,"star3")){
        s <- c(s,0.9)
      }else if(str_detect(i, "star2")){
        s <- c(s,0.6)
      }else{
        s <- c(s,0.4)
      }
    }else if(str_detect(i,"bib")){
      c <- c(c,"blue")
      s <- c(s,0.5)
    }else{
      c <- c(c,"darkgreen")
      s <- c(s,0.5)
    }
  }
  return(list(c,s))
}


## get lon, lat
geo <- geocode(enc2utf8(michelin$address))

ggmap(map)+
  geom_point(data = michelin,
             aes(x = geo$lon, y = geo$lat),
             color = unlist(col(michelin$distiction)[1]),
             size = 4,
             alpha = unlist(col(michelin$distiction)[2])
)


# Gu
gu <- unlist(strsplit(michelin$address,split = " "))
gu[grep("구$",gu)]

table(gu)
