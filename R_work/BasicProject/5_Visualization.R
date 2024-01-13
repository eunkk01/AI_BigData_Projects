#### 기본 내장 그래프 ####

## 1. plot() : 산포도
# plot(y축 데이터, 옵션)
# plot(z축 데이터, y축 데이터, 옵션)

y <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
plot(y, xlim = c(0, 10), ylim = c(0, 5)) # 데이터가 하나면 자동으로 1, 2, ..로 할당

x <- c(1:10)
y <- c(1:10)
plot(x, y)

# type : "p", "l", "b", "o", "n" (선의 모양)
# pch : 점의 모양
# cex : 점의 크기를 비율로 지정
# col : 선과 점의 색상 지정
# lty : "solid", "blank", "dashed", "dotted", "datdash", "longdash", "twodash"
plot(x, y, xlim=c(0, 20), ylim=c(0, 30), main="Graph", type="l", pch=2, cex=2, col="blue", lty="dashed")

str(cars)  # dist : 제동거리
head(cars)
plot(cars, type="o")
View(cars)


# 같은 속도일 때 제동거리가 달라 대체적인 추세를 알기 어려움
# 속도에 따른 평균제동거리 구해 그래프로 그려보자
library(dplyr)
cars2 <- cars %>% group_by(speed) %>% summarise(dist_mean = mean(dist))
plot(cars2)

?tapply # 그룹별로 묶어서?
cars3 <- tapply(cars$dist, cars$speed, mean)
plot(cars3, ylab="distance")


## 2. points() : 그래프 위에 추가하기
plot(iris$Sepal.Width, iris$Sepal.Length)
plot(iris$Petal.Width, iris$Petal.Length)

with(iris,
     {
       plot(Sepal.Width, Sepal.Length)
       points(Petal.Width, Petal.Length)
     })


## 3. lines() : 기본 그래프 위에 선 그래프 추가
plot(cars)
lines(cars$dist)


## 4. barplot(), hist(), pie(), mosaicplot(), pair(), persp(), contour(), ...


## 5. 그래프 배열
head(mtcars)
str(mtcars)

# (1) 그래프 4개를 동시에 그리기
par(mfrow=c(2,2))  # 그래프 그릴 자리 생성
plot(mtcars$wt, mtcars$mpg)
plot(mtcars$wt, mtcars$disp)
hist(mtcars$wt)
boxplot(mtcars$wt)

par(mfrow=c(1,1))
plot(mtcars$wt, mtcars$mpg)

# (2) 행과 열마다 그래프 개수를 다르게 설정
?layout
# layout(matrix(c(1, 2, 2, 3), 2, 2, byrow=T))
# c(자리마다 그릴 그래프 지정), 그래프 판 지정
layout(matrix(c(1, 2, 2, 3), 2, 2, byrow=T))

plot(mtcars$wt, mtcars$mpg)
plot(mtcars$wt, mtcars$disp)
hist(mtcars$wt)
boxplot(mtcars$wt)

par(mfrow=c(1,1))


## 6. 특이한 그래프

# (1) arrows()
x <- c(1, 3, 6, 8, 9)
y <- c(12, 56, 78, 32, 9)

plot(x, y)
arrows(3, 56, 1, 12)    # ((시작점), (도착점))
text(4, 40, "이것은 샘플입니다", srt=65)     # 좌표, 내용, srt=각도지정

# (2) 꽃잎 그래프(sunflowerplot) : 같은 값일 때 몇 개가 겹쳐있는지 확인할 수 있음
x <- c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 4, 5, 6, 6, 6)
y <- c(2, 1, 4, 2, 3, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1)

plot(x, y)  # 같은 위치에 그려지는 점들이 많음 -> 분포 현황을 알 수 없음

sunflowerplot(x, y)


# (3) 별 그래프(stars)
# 데이터의 전체적인 윤곽을 살펴보는 그래프
# 데이터 항목에 대한 변화의 정도를 한눈에 파악

?stars
str(mtcars)

# key.loc : 범례 나타내기, flip.labels=F : 레이블을 같은 위치에 고정
# draw.segments=T : 원그래프처럼 분할된 모양으로 나옴
stars(mtcars[1:4], key.loc=c(13, 1.5), flip.labels=F, 
      draw.segments=T)

# (4) symbols
x <- c(1, 2, 3, 4, 5)
y <- c(2, 3, 4, 5, 6)
z <- c(10, 5, 100, 20, 10)

symbols(x, y, z)  # z값에 의해 점의 크기 결정

#### ggplot2 ####

# install.package("ggplot2")
# http://www.r-graph-gallery.com/ggplot2-package.html

# 레이어 지원
#   1) 배경 설정(데이터)
#   2) 그래프 추가(점, 선, 막대, ..)
#   3) 설정(옵션) 추가(축 범위, 범례, 색, ..)

library(ggplot2)
?ggplot


## (1) 산포도(geom_point)

mpg <- ggplot2::mpg
str(mpg)

ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) + geom_point() + xlim(3, 6) + ylim(10, 30)

# midwest 데이터 이용해 전체인구(poptotal)와 아시아인구(popasian) 간에
# 어떤 관계가 있는지 알아보려고 한다
# x축은 전체 인구, y축은 아시아인구로 된 산포도 작성
# 단, 전체 인구는 30만명 이하, 아시아 인구는 1만명 이하인 지역만 그리기

midwest2 <- midwest %>% filter(poptotal <= 300000, popasian <= 10000) %>%
  select(poptotal, popasian)
ggplot(data=midwest2, mapping=aes(x=poptotal , y=popasian)) + geom_point()

options(scipen = 99) # 지수를 숫자로 표현해주는 옵션
ggplot(data=midwest, mapping=aes(x=poptotal , y=popasian)) + geom_point() + 
  xlim(0, 300000) + ylim(0, 10000)


## (2) 막대 그래프(주로 빈도수) : geom_col(), 히스토그램(y축 하나의 데이터만) : geom_bar()

# mpg 데이터에서 구동 방식(drv)별로 고속도로 평균 연비를 조회하고
# 그 결과를 막대 그래프로 표현

# mpg$total = (mpg$cty + mpg$hwy)/2
# mpg2 <- mpg %>% group_by(drv) %>% summarise(total_mean=mean(total))
# ggplot(data=mpg2, mapping=aes(x=drv, y=total_mean)) + geom_col()

mpg3 <- mpg %>% group_by(drv) %>% summarise(hwy_mean=mean(hwy))
ggplot(data=mpg3, mapping=aes(x=drv, y=hwy_mean)) + geom_col()

# reorder(x, y) : x데이터 자리에 넣면 y값 크기 순으로 정렬 가능
ggplot(data=mpg3, mapping=aes(reorder(drv, hwy_mean), y=hwy_mean)) + geom_col()  # 오름차순
ggplot(data=mpg3, mapping=aes(reorder(drv, -hwy_mean), y=hwy_mean)) + geom_col() # 내림차순

ggplot(data=mpg, mapping=aes(drv)) + geom_bar()
ggplot(data=mpg, mapping=aes(hwy)) + geom_bar()

# 어떤 회사에서 생산한 "SUV"차종의 도시 연비가 높은지 알아보려 한다.
# SUV차종을 대상으로 평균 cty가 가장 높은 회사 5곳을 조회하고 그래프로 출력

mpg2 <- mpg[mpg$class=="suv", ] %>% group_by(manufacturer) %>% select(class, cty) %>%
  summarise(mean_cty = mean(cty)) %>% arrange(desc(mean_cty)) %>% head(5)
ggplot(data=mpg2, mapping=aes(reorder(manufacturer, -mean_cty), y=mean_cty)) + geom_col()


# 자동차 종류 중에서 어떤 종류(class)가 가장 많은지 알아보려 한다.
# 자동차 종류별 빈도를 그래프로 출력

mpg3 <- mpg %>% group_by(class) %>% summarise(class_count = n())
ggplot(data=mpg3, mapping=aes(x=class, y=class_count)) + geom_col()


## (3) 선 그래프 : geom_line()
str(economics)
head(economics)
tail(economics)

# 날짜별 저축률
ggplot(data=economics, mapping=aes(x=date, y=psavert)) + geom_line()

# 날짜별 실업률
ggplot(data=economics, mapping=aes(x=date, y=uempmed)) + geom_line()


## (4) 상자 그래프 : geom_boxplot()
ggplot(data=mpg, mapping=aes(x=drv, y=hwy)) + geom_boxplot()

# mpg 데이터에서 class가 "compact", "subcompact", "suv"인 자동차의 도시 연비가 어떻게 다른지 비교해보려 한다.
# 이 세 차종의 도시 연비를 비교하기 위한 그래프를 그려보고 분석해보자

mpg2 <- mpg %>% group_by(class) %>% filter(class %in% c("compact", "subcompact", "suv"))
mpg2
ggplot(data=mpg2, mapping=aes(x=class, y=cty)) + geom_boxplot()
# 평균적으론 compact의 연비가 더 좋다고 볼 수 있다.
# 같은 경차 중에선 compact보다 subcompact의 분포가 더 넓게 분포되어 있는 걸로 보아 ..?


## (5) iris 실습 1
str(iris)

# 꽃받침(Sepal)의 길이에 따라서 폭의 크기가 어떤 관계인지 분포 확인

ggplot(data=iris, mapping=aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point(color="red", pch=16, size=3)

ggplot(data=iris, mapping=aes(x=Sepal.Length, y=Sepal.Width, 
                              color=Species, size=Petal.Length)) + geom_point(pch=16) +
  labs(title="꽃받침의 비교", 
       subtitle="꽃받침의 길이에 대한 폭의 크기 확인",
       caption="주석 달기", 
       x="꽃받침의 길이", y="꽃받침의 폭")

ggplot(data=iris, mapping=aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point(color=c("red", "green", "blue")[iris$Species], pch=c(16, 17, 18)[iris$Species], size=3)


# 같은 y축에 여러 그래프 그리기(선 그래프) -> long형으로 바꿔줘야함

# (1) 순서를 갖는 데이터 준비
head(iris)
seq <- as.numeric(row.names(iris))
iris_data <- cbind(seq=seq, iris)
head(iris_data)

# (2) 각 데이터에 대한 색상(rainbow(), heat.colors, terrain.colors(), topo.colors(), cm.colors())
ex <- topo.colors(30)
pie(rep(1, 30, col=ex))

# (3) wide형을 long형으로
library(reshape2)

mdata <- melt(iris_data, id.vars = c("seq", "Species"))
View(mdata)

ggplot(mdata) + geom_line(aes(x=seq, y=value, color=variable), cex=0.8)

cols <- topo.colors(6:10, alpha = 0.5)   # topo에서 색상 받아오기
names(cols) <- names(iris_data)[2:5]  # 색별로 이름 매핑 시키기
ggplot(mdata) + geom_line(aes(x=seq, y=value, color=variable), cex=0.8) + 
  scale_color_manual(name="변수명", values=cols)
