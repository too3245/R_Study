#한글로 년 월 일형식으로 입력하는 경우
as.Date("2014년 11월 1일",format="%Y년 %m월 %d일")

#년도가 대문자 Y로 사용
as.Date("01112014",format="%d%m%Y")

#년도가 소문자 y로 사용
as.Date("01112014",format="%d%m%y")

#기준 일자를 주고 및 일 후 찾기
#주어진 날짜 기준으로 10일 후의 날짜
as.Date(10,origin="2014-11-10")

#주어진 날짜 기준으로 10일 이전 날짜
as.Date(-10,origin="2014-11-10")

#날짜 연산하기
#날짜처럼 보이지만 문자입니다.
"2014-11-30" -"2014-11-01"

#문자로 확인.
class("2014-11-30")

as.Date("2014-11-30") - as.Date("2014-11-01")

as.Date("2014-11-01") +5

#install.pakages("plotrix")
#library(plotrix)

x <- c(9,15,20,6)
label <- c("영업 1팀","영업 2팀","영업 3팀","영업 4팀")
pie(x,labels = label,main="부서별 영업 실적적")

View(label)

#초기의 시작 앵글을 바꾸는 옵션
#INIT.ANGLE=90 #90도로 앵글을 바꿔서 출력함

pie(x,init.angle=90,labels = label,main="부서별 영업 실적적")

#색과 라벨 수정
pct <-round(x/sum(x)*100)
label <- paste(label,pct)
label <- paste(label,"%",sep="")
pie(x,
    labels=label,
    init.angle=90,
    col=rainbow(length(x)),
    main="부서별 영업 실적")

#paste 함수는 문자와 문자를 연결하는 경우에는 paste 함수를 이용하여 연결을 해야한다.
#round 함수는 반올림을 해주는 함수 이다.

#3D 파이 차트
pie3D(x,
      labels=label,
      explode = 0.1,
      labelcex = 0.8,
      main="부서별 영업실적")


# 기본 바 차트
height <- c(9,15,20,6)
name <- c("영업 1팀","영업 2팀","영업 3팀","영업 4팀")
barplot(height,names.arg = name,main="부서별 영업 실적적")

#막대의 색 지정
barplot(height,
        names.arg = name,
        main="부서별 영업 실적",
        col = rainbow(length(height)))

#y축 MAX값 지정
barplot(height,
        names.arg=name,
        main="부서별 영업실적",
        col=rainbow(length(height)),
        xlab="부서",
        ylab="영업 실적(억 원)",
        ylim =c(0,25))

#데이터 라벨 출력
bp <-barplot(height,
        names.arg=name,
        main="부서별 영업실적",
        col=rainbow(length(height)),
        xlab="부서",
        ylab="영업 실적(억 원)",
        ylim =c(0,25))

#데이터 바 밖에 값을 출력하기.
text(x=bp,y=height,labels = round(height,0),pos=3)

#데이터 바 안쪽에 값을 출력하기.
text(x=bp,y=height,labels = round(height,0),pos=1)

#바차트를 수평회전(가로 막대)
barplot(height,
        names.arg=name,
        main="부서별 영업실적",
        col=rainbow(length(height)),
        xlab="부서",
        ylab="영업 실적(억 원)",
        xlim =c(0,25),
        horiz=TRUE,
        width=50)

# 스택형 바 차트(Stacked Bar Chart)
height1 <- c(4,18,5,8)
height2 <- c(9,15,20,6)
height <- rbind(height1,height2)
View(height)

name <-c("영업 1팀","영업 2팀","영업 3팀","영업 4팀")
legend_lbl <- c("2014년","2015년")

barplot(height,
        names.arg=name,
        main="부서별 영업실적",
        xlab="부서",
        ylab="영업 실적(억 원)",
        ylim =c(0,35),
        col=c("darkblue","red"),
        legend.text=legend_lbl,
        width=50)

height1 <- c(4,18,5,8)
height2 <- c(9,15,20,6)
height3 <- c(5,12,11,14)
height <- rbind(height1,height2,height3)
View(height)

name <-c("영업 1팀","영업 2팀","영업 3팀","영업 4팀")
legend_lbl <- c("2014년","2015년","2016년")

barplot(height,
        names.arg=name,
        main="부서별 영업실적",
        xlab="부서",
        ylab="영업 실적(억 원)",
        ylim =c(0,50),
        col=c("darkblue","red","yellow"),
        legend.text=legend_lbl,
        width=50)

# 그룹형 바 차트(Group Bar Chart)
barplot(height,
        names.arg=name,
        main="부서별 영업실적",
        xlab="부서",
        ylab="영업 실적(억 원)",
        ylim =c(0,50),
        col=c("darkblue","red","yellow"),
        legend.text=legend_lbl,
        beside=TRUE,
        args.legend=list(x='topright'))

#일반적인 X-Y 플로팅
View(women)

weight <- women$weight
height <- women$height

plot(height,weight,xlab="키",ylab="몸무게")

#플로팅 문자의 출력

plot(height,weight,
     xlab="키",ylab="몸무게",
     pch=23, #모양의 종류
     col="blue", #테두리선 색
     bg="yellow", #채워지는색 
     cex=1.5) #글자의 크기

#지진의 강도에 대한 히스토그램
head(quakes)

mag <- quakes$mag
mag
View(mag)
hist(mag,
     main="지진 발생 강도의 분포",
     xlab="지진 강도",ylab="발생건수수")

#히스토그램과 막대그래프의 차이 
#히스토그램은 구간에 대한 높이를 만들어주고(데이터의 개수)
#막대그래프는 데이터의 크기를 나타낸다.(데이터들의 값)
#계급 구간과 색
colors <-c("red","orange","yellow","green","blue","navy","violet")

hist(mag,
     main="지진 발생 강도의 분포",
     xlab="지진 강도",ylab="발생건수수",
     col=colors,
     breaks=seq(4,6.5,by=0.5))


#확률 밀도
hist(mag,
     main="지진 발생 강도의 분포",
     xlab="지진 강도",ylab="발생건수수",
     col=colors,
     breaks=seq(4,6.5,by=0.5),
     freq=FALSE)
lines(density(mag)) #선을 그려주는 함수 lines, 확률 밀도 함수 density

#박스 플롯
mag <- quakes$mag
min(mag)
max(mag)
median(mag)
quantile(mag,c(0.25,0.5,0.75))


boxplot(mag,
        main="지진 발생 강도의 분포",
        xlab="지진",ylab="발생 건수",
        col="red")

