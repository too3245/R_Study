#텍스트 마이닝(Text Mining)
#문자로 된 데이터에서 가치 있는 정보를 얻어 내는 분석 기법
#SNS나 웹사이트에 올라온 글을 분석해 사람들이 어떤 이야기를 나누고 있는지 파악할 때 활용
#형태소(Morphology Analysis):문자을 구성하는 어절들이 어떤 품사로 되어 있는지 분석
#분석절차
#-형태소 분석
#-명사,동사 형용사 등 의미를 지닌 품사 단어 추출
#-빈도표 만들기
#-시각화

#10-1. 힙합 가사 텍스트 마이닝
#텍스트 마이닝 준비하기

#### 10-1 ####
#install.packages("rjava")
#install.packages("memoise")
#install.packages("KonNLP")

#패키지 로드
library(KoNLP)
##Checking user defined dictionary!

library(dplyr)

#패키지 로드 에러 발생할 경우 -java 설치 경로 확인 후 경로 설정
#java 폴더 경로 설정
#Sys.setenv(JAVA_HOME="C:/ProgramFiles/Java/jre)

#사전 설정하기

useNIADic()

#Backup was jsust finished!
##9830122 words dictionary was built

#데이터 준비
txt <- readLines("hiphop.txt")

## Warning in readLines("hiphop.txt"):incomplete final line found on
## 'hiphop.txt'

head(txt)

#특수문자 제거

#install.packages("stringr")
#library(stringr)

#특수문자 제거

txt <- str_replace_all(txt," \\W", " ")
class(txt)
View(txt)
dim(txt)


#가장 많이 사용된 단어 알아보기
#명사 추출하기
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")

#가사에서 명사 추출
nouns <- extractNoun(txt)
class(nouns)
#추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표로 생성

wordcount <- table(unlist(nouns))
View(wordcount)
dim(wordcount)
wordcount

#자주 사용되는 단어 빈도표 만들기
#데이터 프레임으로 변환

df_word <- as.data.frame(wordcount,stringsAsFactors = F)

class(df_word)
#변수명 수정

df_word <- rename(df_word,word=Var1,freq = Freq)
#두글자 이상 단어 추출
df_word <- filter(df_word,nchar(word) >= 2)

top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
head(top_20)


##워드클라우드 하기
#install.packages("wordcloud")
#패키지 로드
library(wordcloud)
## Lodingrequred package : RColorBrewer

library(RColorBrewer)

##단어색상 목록 만들기

pal <- brewer.pal(8,"Dark2")#Dark2색상 목록에서 8개의 색상 추출

#워드 클라우드 생성
set.seed(1234)#난수 고정
wordcloud(words = top_20$word, #단어 
          freq = top_20$freq, #빈도
          min.freq = 2, #최소단어 빈도
          max.words = 200, #표현단어 수
          random.order =  F,#고빈도 단어 중앙 배치
          rot.per = .1,# 회전 단어 비율
          scale= c(4,0.3), #단어 크기 범위
          colors = pal) # 색깔 목록


gomin.txt <- readLines("remake.txt")
gomin.txt <- str_replace_all(gomin.txt ," \\W", " ")

gomin <- extractNoun(gomin.txt)
wordcount <- table(unlist(gomin))
wordcount
df_word <- as.data.frame(wordcount,stringsAsFactors = F)
class(df_word)
df_word <- rename(df_word,word=Var1,freq = Freq)
df_word <- filter(df_word,nchar(word) >= 2)

top_40 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(40)
head(top_40)

pal <- brewer.pal(8,"Dark2")#Dark2색상 목록에서 8개의 색상 추출

#워드 클라우드 생성
set.seed(1234)#난수 고정
wordcloud(words = top_40$word, #단어 
          freq = top_40$freq, #빈도
          min.freq = 2, #최소단어 빈도
          max.words = 200, #표현단어 수
          random.order =  F,#고빈도 단어 중앙 배치
          rot.per = .1,# 회전 단어 비율
          scale= c(4,0.3), #단어 크기 범위
          colors = pal) # 색깔 목록
##10-2 구정원 트위터 특세트 마이닝
# - 국정원 계정 트위터 데이터
# - 국정원 계정으로 작성된 3,744

#데이터 준비하기 
#데이터 로드

twitter <- read.csv("twitter.csv",
                    header=T,
                    stringsAsFactors = F,
                    fileEncoding = "UTF-8")
names(twitter)
#변수명 수정
twitter <- rename(twitter,
                  no = 번호,
                  id = 계정이름,
                  date = 작성일,
                  tw = 내용)
#특수문자 제거
twitter$tw <- str_replace_all(twitter$tw,"\\W"," ")

#단어 빈도표 만들기 
#트위터에서 명사 추출
nouns <- extractNoun(twitter$tw)

#추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns))

#데이터 프레임으로 변환
df_word <- as.data.frame(wordcount,stringsAsFactors = F)

df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)
names(df_word)

#단어 빈도 막대 그래프 만들기

df_word <- filter(df_word,nchar(word) >= 3)

top_40 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(40)
head(top_40)

#단어빈도 막대 그래프 만들기

order <- arrange(top_40,freq)$word

ggplot(data = top_40,aes(x=word,y=freq))+
  ylim(0,1000)+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limit = order)+geom_text(aes(label=freq),hjust=0-.3)

#library(devtools)
#library(htmlwidgets)
#library(htmltools)
#library(jsonlite)
#library(yaml)
#library(base64enc)
#library(tm)
#library(wordcloud2)



#3.워드클라우드 그리기(기본)
wordcloud2(top_40)

#3.1 wordcloud2 크기, 색 변경(size,color)
wordcloud2(top_40,size = 0.5, col="random-dark")

#3.2 키워드 회전 정도 조절(rotateRatio)
wordcloud2(top_40,size=0.5, col="random-dark",rotateRatio = 0)

#3.3 배경색 검정(backgroundColor)
wordcloud2(top_40,size=0.5,col = "random-light",backgroundColor = "black")

#특정 개수 이상 추출되는 글자만 색깔을 변경하여 나타나도록....
#https://html-color-codes.info/Korean/
#사이값 지정시 :(weight >800 && weight <1000)
#100개 이상 검색될 시 노랑으로, 아니면 초록으로 표현한다.
In_out_color = "function(word,weight){ return(weight> 100)?'#F3EF12':'#1EC612;}"

#wroldcoloud2 그리기

#library(wordcloud2)
#기존 모형으로 wordcloud2 생성
#모형선택 : shape ='circle', 'cardioid',
#                  'diamond',triangle-forward',
#                  'triangle','pentagon','star'

wordcloud2(df_word,
           shape='diamond',
           size=0.8,
           color = "random-light",
           backgroundColor = 'black'
           )

#https://cran.r-project.org/web/packages/wordcloud2/vgnettes/wordcloud.html
