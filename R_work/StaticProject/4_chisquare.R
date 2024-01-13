#### 실습1 : 자동차의 실린더 수와 변속기의 관계 : chisq.test ####

## 주제 : 자동차의 실린더 수(cyl)와 변속기(am)의 관계를 알고싶다. ##

View(mtcars)

# 실린더 수와 변속기 종류 파악
table(mtcars$cyl, mtcars$am)

# 테이블의 가독성을 높이기 위해 전처리
mtcars$tm <- ifelse(mtcars$am==0, "auto", "manual")
result <- table(mtcars$cyl, mtcars$tm)
result

barplot(result)
# auto의 눈금이 벗어났기 때문에 최대값을 알 수 없음 -> 눈금 조정(xlim, ylim)
barplot(result, ylim=c(0, 20))
# 범례 추가(legend)
barplot(result, ylim=c(0, 20), legend=paste(rownames(result), "cyl"))
# 그래프를 수직으로 나누기(beside=T)
barplot(result, ylim=c(0, 20), legend=paste(rownames(result), "cyl"), beside=T)
# 그래프를 수평으로 나누기(beside=T)
barplot(result, ylim=c(0, 20), legend=paste(rownames(result), "cyl"), beside=T, horiz=T)
# 색상추가하기(col=c("tan1", "coral2", "firebrick2"))
barplot(result, ylim=c(0, 20), legend=paste(rownames(result), "cyl"), beside=T, 
        horiz=T, col=c("tan1", "coral2", "firebrick2"))

addmargins(result) # 행과 열의 합계를 추가해줌

# 질적변수 -> 카이제곱 검정
chisq.test(result)
# p-value = 0.01265 -> 관계가 있다.
# 경고메시지(들): 카이제곱 approximation은 정확하지 않을수도 있습니다
# -> 데이터 수 부족 -> Fisher's Test 이용하기

fisher.test(result)
# p-value = 0.009105 -> 관계가 있다.

#### 실습2 : 시군구와 다가구 자녀 지원 조례의 관계 : fisher.test ####

## 주제 : 시군구와 다가구 자녀 지원 조례가 관계가 있는가? ##

mydata <- read.csv("../data/anova_two_way.csv", fileEncoding = "euc-kr")
View(mydata)

result <- table(mydata$ad_layer, mydata$multichild)

barplot(result, legend=rownames(result), ylim=c(0, 80), beside=T, 
        col=c("tan1", "coral2", "firebrick2"))

chisq.test(result)  # p-value = 0.7133

fisher.test(result) # p-value = 0.7125 -> 관계가 없다.

#### 실습3 : Cocran-Armitage Trend Test : prop.trend.test() ####

## 주제 : 흡연 여부(질적변수)와 고혈압 유무(질적변수)가 서로 관련이 있는가? ##

library(moonBook)
View(acs)

result <- table(acs$HBP, acs$smoking)
result

# 순서 재배치(factor) -> 서열변수로..
acs$smoking <- factor(acs$smoking, levels=c("Never", "Ex-smoker", "Smoker"))
result <- table(acs$HBP, acs$smoking)
result


## chi-squre test ##
chisq.test(result) # p-value = 6.19e-10 -> 큰 차이를 보임(관계가 있음)


## cocran-armitage trend test : prop.trend.test(x, n) ##
# x : 사건이 발생한 횟수(고혈압, 결과변수), n : 시도한 횟수(흡연유무, 원인변수)

result[2,]      # 사건이 발생한 횟수 => 고혈압 환자가 발생한 사람 수
colSums(result) # 시도한 횟수 => 흡연여부의 합
# colSums : 열의 합계를 뽑아내주는 함수

prop.trend.test(result[2,], colSums(result))
# p-value = 9.282e-11 -> 큰 차이를 보임(관계가 있음)


## 시각화 ##
mosaicplot(t(result), color=c("tan1", "firebrick2"), 
           xlab="Smoking", ylab="Hypertension")

colors()       # 색 종류 확인
demo("colors") # 색 미리 보기

mytable(smoking~age, data=acs) # 두 컬럼을 요약해서 보여줌
# 위에서 고혈압과 흡연여부로만 봤을 땐 흡연을 안하는 사람들의 고혈압 걸리는 경우가
# 더 많다는 결론을 내리고 싶겠지만 그러면 안된다!(둘의 관계 여부 정도만 파악 가능)
# 다른 변수들도 같이 고려해서 결과를 도출해야 함!
