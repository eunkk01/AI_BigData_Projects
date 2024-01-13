#### 실습1 : 담배값과 매출액의 관계가 있는가? ####

## 주제 : 담배값 인상 전 월별 매출액과 인상 후 월별 매출액의 관계가 있는가? ##
## 담배값과 매출액의 관계가 있는지

x <- c(70, 72, 62, 64, 71, 76, 0, 65, 75, 72)  # 인상 전
y <- c(70, 74, 65, 68, 72, 74, 61, 66, 76, 75) # 인상 후

?cor
cor(x, y, method = "pearson")  # 0.7767669 : 양의 상관 관계(강한 관계)
cor(x, y, method = "spearman") # 0.929878
cor(x, y, method = "kendall")  # 0.8409091

# cor.test() : cor보다 다양한 종류의 정보를 알려주는 함수
cor.test(x, y, method = "pearson")

#### 실습2 : 인구 증가율과 노령 인구 비율간의 관계가 있는가? ####

## 주제 : 인구 증가율과 노령 인구 비율간의 관계가 있는가? ##
# pop_growth : 인구 증가율, eldery_rate : 노령 인구 비율, finance : 재정 자립도
# cultural_center : 인구 10만명당 문화 기반 시설 수

mydata <- read.csv("../data/cor.csv", fileEncoding = "euc-kr")
mydata

plot(x=mydata$pop_growth, y=mydata$elderly_rate)

cor(x=mydata$pop_growth, y=mydata$elderly_rate, method="pearson") # -0.4069218


# 한번에 여러 변수들의 관계 파악하기 #
attach(mydata) # 메모리에 부착, 언급 안해줘도 됨
x <- cbind(pop_growth, birth_rate, elderly_rate, finance, cultural_center)
cor(x)
#                pop_growth  birth_rate elderly_rate     finance cultural_center
#pop_growth       1.0000000  0.16152885  -0.40692178  0.23078789     -0.20736363
#birth_rate       0.1615289  1.00000000   0.05672361 -0.09981501      0.08881914
#elderly_rate    -0.4069218  0.05672361   1.00000000 -0.43573354      0.49885306
#finance          0.2307879 -0.09981501  -0.43573354  1.00000000     -0.06016467
#cultural_center -0.2073636  0.08881914   0.49885306 -0.06016467      1.00000000
detach(mydata) # 메모리 부착 해제

#### 실습3 : 부모의 키와 자식의 키가 관계가 있는가? ####

## 주제 : 부모의 키와 자식의 키가 관계가 있는가? ##

install.packages("UsingR")
library(UsingR)

str(galton)

plot(child ~ parent, data=galton)     # 같은 값이 겹쳐져 나와 정확한 분포 알 수 없음
cor.test(galton$child, galton$parent) # 0.4587624

out <- lm(child ~ parent, data=galton)
summary(out) # p-value: < 2.2e-16

abline(out, col="red")
sunflowerplot(galton)

# jitter : 값이 같은 점들을 흐트러놓아줌
plot(jitter(child, 5) ~ jitter(parent, 5), data=galton)

#### 실습4 : 각 관측소끼리 관계가 있는가?(여러 그래프 사용해보는..) ####

## 주제 : 각 관측소마다 관계가 있는가? ##

install.packages("SwissAir")
library(SwissAir)

# 세 관측소에서 관측한 오존농도, 일산화질소, 이산화질소를 30분마다 측정한 값
View(AirQual)

Ox <- AirQual[ , c("ad.O3", "lu.O3", "sz.O3")] + 
  AirQual[ , c("ad.NOx", "lu.NOx", "sz.NOx")] -
  AirQual[ , c("ad.NO", "lu.NO", "sz.NO")]
names(Ox) <- c("ad", "lu", "sz")  # 컬럼이름 관측소명으로 바꿔주기

plot(lu ~ sz, data=Ox) # 겹치는 부분이 너무 많아 어디가 가장 많이 겹치는지 알 수 없음

Ox <- na.omit(Ox) # 결측치 행 제외, 결측치가 없어야 상관관계 확인 가능
cor(Ox$lu, Ox$sz) # 0.9035144


install.packages("hexbin")
library(hexbin)

bin <- hexbin(Ox$lu, Ox$sz, xbins=50) # 육각형으로 분포 보여줌
plot(bin)


smoothScatter(Ox$lu, Ox$sz)  # 블러처리한 것처럼 분포 보여줌


install.packages("IDPmisc")
library(IDPmisc)

iplot(Ox$lu, Ox$sz)  # 히트맵처럼 분포 보여줌
