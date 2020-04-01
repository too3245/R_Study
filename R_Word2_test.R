# 다운로드 사이트 : http://kostat.go.kr/
#작업의 순서
#1. 데이터 파일 읽기 : 6_101_DT_1826001_A01_M.csv(국내 인구 이동 통계 파일) :read.csv(file.choose(),header=T)
#2.'전국' 지역이 아닌 데이터만 추출:data[data$해정구역.시군구.별!="전국",]
#3.행정구역 중 '구'단위에 해당하는 행 번호 추출:x <- grep("구$",data2$행정구역.시군구.별)
#4.'구' 지역 데이터 제외 : <-data2[-c(x),]
#5.순 이동 인구수가 0보다 큰 지역 추출: data3[data3$순이동.명>0,]
#6.단어(행정 구역)할당: data4$행정구역.시군구.별
#7.행정 구역별 빈도 :data$순이동 명명:data4%순이동.명
#8.워드클라우드 출력:wordcloud()
#파일명을 직접 입력해도 되지만 파일을 선택하는 창을 띄워(file.choose)

#다운로드 사이트: : http://kostat.go.kr/
#6_101_DT_1B26001_A01_M.csv

data <- read.csv(file.choose(),header=T)
head(data)

#데이터 정제: 불필요한 지역 제외
#'전국' 지역 제외
data2 <-data[data$행정구역.시군구.별 !="전국",]
head(data2)

#'구' 단위 지역 통계 제외
x <- grep('구$',data2$행정구역.시군구.별)
data3 <- data2[-c(x),]
head(data3)

#전입자 수가 많은 지역
data4 <- data3[data3$순이동.명>0,]
head(data4)

word <- data4$행정구역.시군구.별
View(word)

frequency <- data4$순이동.명
View(frequency)

pal2 <- brewer.pal(8,"Dark2")

wordcloud(word,frequency,colors=pal2)

#연설문의 단어에 대한 워드 클라우드 만들기
#install.packages("KoNLP")
#install.packages("RColorBrewer")
#install.packages("wordcloud")

#library(KoNLP)
#library(RColorBrewer)
#library(wordcloud)


useSejongDic()
pal2 <- brewer.pal(8,"Dark2")

text <- readLines(file.choose())
text

noun <- sapply(text,extractNoun,USE.NAMES = F)
noun


noun2 <- unlist(noun)
noun2

#p.221

word_count <- table(noun2)
word_count

head(sort(word_count,decreasing = TRUE),10)

wordcloud(names(word_count), #문자들
          freq=word_count, #문자의 개수
          scale=c(12,0.8),# 스케일 0.8애서 12
          min.freq=3, #최소 문자 개수 값
          random.order=F, #이름이 출력되는공간이 랜덤 
          rot.per=.1, #회전 각도값
          colors=pal2)

