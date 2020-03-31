#지역별 순이동에 따른 워드 클라우드
#install.pakages("wordcloud")
#library(wordcloud)

word <- c("인천광역시","강화군","웅진군","눈물","웃음")
frequency <- c(651,85,61,15,700)

wordcloud(word,frequency,colors="blue")

#단어들의 색 변환

wordcloud(word,
          frequency,
          random.order = F,
          random.color = F,
          colors=rainbow(length(word)))

#다양한 단어 색 출력을 위한 팔레트 패키지의 활용
#install.packages("RColorBrewer")
#library(RColorBrewer)

pal2 <- brewer.pal(8,"Dark2")

word <- c("인천광역시","강화군","웅진군","눈물","웃음")
frequency <- c(651,85,61,15,700)

wordcloud(word,frequency,colors=pal2)

      