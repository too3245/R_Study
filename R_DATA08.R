#ggplot2 레이어 구조 이해하기

#3단계: 설정추가(축 범위, 색, 표시)

#2단계: 그래프 추가(점,막대,선)

#1단계: 배경설정(축)

#산점도 만들기

#산점도(Scater Plot):데이터를 X축과 Y축에 점으로 표현한 그래프
#나이와 소득처럼,연속값으로 된 두 변수의 관계를 표현할 때 사용

#ggplot2 로드

library(ggplot2)

#1.배경 설정하기
#X축 disply, y축 hwy로 지정해 배경 생성
ggplot(data = mpg, aes(x=displ,y=hwy))


#2.그래프 추가하기

#배경에 산점도 추가

ggplot(data = mpg,aes(x=displ,y =hwy)) +geom_point()

#3.축 범위를 조정하는 설정 추가하기
#x축 범위 3~6으로 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6)

#3. 축 범위를 조정하는 설정 추가하기
#x축의 범위 3~6 y축의 범위 10~30 으로 지정

ggplot(data = mpg, aes(x=displ, y=hwy))+
  geom_point()+
  xlim(3,6)+
  ylim(10,30)

#집단별 평균표 만들기

library(dplyr)

df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy))

df_mpg

#2.그래프 생성하기
ggplot(data = df_mpg,aes(x=drv,y=mean_hwy)) +geom_col()

#3. 크기 순으로 정렬하기
ggplot(data=df_mpg,aes(x=reorder(drv,-mean_hwy),y=mean_hwy))+geom_col()

#막대기 그래프2 - 빈도 막대 그래프 
ggplot(data=mpg,aes(x=drv))+geom_bar()

# x 축 연속 변수, y 축 빈도
ggplot(data = mpg, aes(x = hwy)) + geom_bar()
#선 그래프 - 시간에 따라 달라지는 데이터 표현하기
#선 그래프: 데이터를 선으로 표현한 그래프
#선으로 표현한 그래프,환율,자기지수등 경제지표가 시간에 따라 어떻게 변하는지 표현할때 활용 
#시계열 그래프 만들기
ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()

#상자 그림 -집단간 분포 차이 표현하기
# 상자 그림(Box Plot): 데이터의 분포 (퍼져있는 형태)를 직사각형 모형으로 표현한 그래프
#분포를 알 수 있기 떄문에 데이터의 특성을 좀 더 자세히 이해할 수 있음
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()
