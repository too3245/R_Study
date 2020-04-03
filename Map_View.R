#지도 시각화
#단계 구분도(Choropleth Map)
#지역별 통계치를 색깔의 차이로 표현한 지도
#인구나 소득 같은 특성이 지역별로 얼마나 다른지 쉽게 이해할 수 있음

# 11-1 미국 주별 강력 범죄율 단계 구분도 만들기

#패키지 준비하기
#install.package("ggiraptExtra")
#library(ggiraphExtra)

str(USArrests)

head(USArrests)

#패키지 준비하기
#library(tibble)

#행 이름을 state 변수로 바꿔 데이터 프레임 생성

crime <- rownames_to_column(USArrests,var ="state")

crime$state <- tolower(crime$state)

crime
names(crime)

View(USArrests)

#tibble(티블)은
#행 이름을 가질 수 있지만 (예: 일반데이터 프레임에서 변활 할 때)
#(연산자로 서브 세팅 할 때 제거됩니다.)
#NULL이 아닌행이름을 티블에 지정하려고 하면 경고가 발생합니다.
#일반적으로 행 이름은 기본적으로 다른 모든 열과
#의미가 다른 문자 열이므로 행 이름을 사용하지 않는 것이 가장 좋습니다.
#이러한 함수를 사용하면
#데이터 프레임에 행 이름(has_rownames())이 있는지 감지하거나,
#제거하거나(remove_rownames())
#명시적 열(rownames_to_column() 및 coloumn_to_rownames())사이에 앞뒤로 변환 할 수 있습니다.
#rowid_to_column()도 포함되어 있습니다.
#이것은 1부터 시작하여 순차적인 행 ID를 오름차순으로하는 데이터 프레임의 시작 부분에 열을 추가합니다.
#기존 행 이름이 제거 됩니다.


#미국 주 지도 데이터 준비하기

#library(ggplot2)
states_map <- map_data("state")
str(states_map)

ggChoropleth(data =crime,          #지도에 표현할 데이터
             aes(fill = Rape,     #색깔로 표현할 변수
                 map_id = state),  #지역 기준 변수
             map =states_map)      #지도 데이터

#인터렉티브 단계구분도 만들기
ggChoropleth(data =crime,          #지도에 표현할 데이터
             aes(fill = Rape,      #색깔로 표현할 변수
                 map_id = state),  #지역 기준 변수
             map =states_map,      #지도 데이터
             interactive =  T)     #인터렉티브      


#11-2. 대한민국 시도별, 결핵 환자 수 단계 구분도 만들기

#대한민국 시도별 인구 단계 구분도 만들기

#패키지 준비하기
#install.packages("stringi")

#install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)

#대한민국 시도별 인구 데이터 준비하기
str(changeCode(korpop1))

#library(dplyr)
korpop1 <- rename(korpop1,
                  pop = 총인구_명,
                  name = 행정구역별_읍면동)
str(changeCode(korpop1))

#단계 구분도 만들기

ggChoropleth(data=korpop1,       #지도에 표현할 데이터
             aes(fill=pop,       #색깔로 표현할 변수
                 map_id=code,    #지역기준 변수
                 tooltip=name),  #지도위에 표시할 지역명
             map=kormap1,        #지도 데이터
             interactive = T)    #인터렉티브

#대한민국 시도별 결핵 환자 수 단계 구분도 만들기
str(changeCode(tbc))

ggChoropleth(data = tbc,
             aes(fill= NewPts,
                 map_id =code,
                 tooltip = name),
             map =kormap1,
             interactive = T)


#12.인터랙티브 그래프
#인터랙티브 그래프 만들기
#패키지 준비하기
install.packages("plotly")
library(plotly)

#ggplot으로 그래프 만들기
library(ggplot2)
p <- ggplot(data = mpg,aes(x=displ,y=hwy,col=drv))+geom_point()
p

ggplotly(p)
#https://plotly.com/
g <- ggplot(data = diamonds,aes(x=cut,fill=clarity))+geom_bar(position="dodge")
ggplotly(g)


##12-2. dygraphs 패키지로 인터랙티브 시계열 그래프 만들기
#인터렉티브 시계열 그래프 만들기

#패키지 준비하기
#install.packages("dygraphs")
library(dygraphs)

#데이터 준비하기
economics <- ggplot2::economics
head(economics)

#시간 순서 속성을 지니는 xts 데이터 타입으로 변경

library(xts)
eco <- xts(economics$unemploy,order.by = economics$date)
head(eco)

#그래프 생성
dygraph(eco)

#날짜 범위 선택가능

dygraph(eco) %>%  dyRangeSelector()

#여러값 표현하기
# 저축률
eco_a <- xts(economics$psavert,order.by = economics$date)
#실업자 수
eco_b <- xts(economics$unemploy/1000,order.by = economics$date)

#합치기

eco2 <- cbind(eco_a,eco_b)
colnames(eco2) <- c("pasavert","unemploy")
head(eco2)

#그래프 만들기

dygraph(eco2) %>%  dyRangeSelector()

#http://dygraphs.com/