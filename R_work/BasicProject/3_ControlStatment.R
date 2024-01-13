#### 조건문 ####

## 난수 준비
?runif()

x <- runif(1)  # 0~1사이의 균일분포, rnorm()
x

# x가 0.5보다 크면 출력
if(x > 0.5){
  print(x)
}

# x가 0.5보다 작으면 1-x로 출력, 그렇지 않으면 x를 출력
if(x < 0.5){
  print("x가 0.5보다 작습니다.")
  print(1-x)
}else{
  print("x가 0.5보다 큽니다.")
  print(x)
}

if(x < 0.5){print("x가 0.5보다 작습니다."); print(1-x)}else{print("x가 0.5보다 큽니다."); print(x)}


## ifelse()
ifelse(x<0.5, 1-x, x)


## 다중조건
avg <- scan()

if(avg >= 90){
  print("당신의 학점은 A학점입니다.")
}else if(avg >= 80){
  print("당신의 학점은 B학점입니다.")
}else if(avg >= 70){
  print("당신의 학점은 C학점입니다.")
}else if(avg >= 60){
  print("당신의 학점은 D학점입니다.")
}else{
  print("당신의 학점은 F학점입니다.")
}


## switch(변수, 실행문1, 실행문2, ...) : 같다라는 조건에서만 간단하게 사용
## -> 문자열로만 비교 가능

a <- "중1"
switch(a, "중1"=print("14살"), "중2"=print("15살"), "중3"=print("16살"))
switch(a, "중1"="14살", "중2"="15살", "중3"="16살") # print() 생략가능

b <- 3
switch(b, "14살", "15살", "16살")  # 별도의 비교값이 없으면 인덱스값으로 됨

empname <- scan(what="")
switch(empname, hong=250, lee=350, kim=200, kang=400)


# 위 다중조건을 switch로 작성
avg <- scan()

switch(as.character(floor(avg/10)), 
       "10"=print("당신의 학점은 A학점입니다."),
       "9"=print("당신의 학점은 A학점입니다."), 
       "8"=print("당신의 학점은 B학점입니다."), 
       "7"=print("당신의 학점은 C학점입니다."), 
       "6"=print("당신의 학점은 D학점입니다."), 
       print("당신의 학점은 F학점입니다."))

avg <- scan() %/% 10  # %/% : 나눴을 때 몫만 가져오기

result <- switch(as.character(avg), "10"="A", "9"="A", "8"="B", "7"="C", "6"="D", "F")
cat("당신의 학점은", result, "입니다.")


## which() : 값의 위치(index)를 찾아주는 함수
?which

# vector에서 사용
x <- c(2:10)
x

which(x == 3)  # 3이 있는 인덱스를 출력
x[which(x==3)] # 3이 있는 인덱스를 찾은 후, 해당 인덱스 값 출력

# matrix에서 사용
m <- matrix(11:22, 3, 4)
m
which(m%%3==0)
which(m%%3==0, arr.ind=F) # 인덱스 활용 X -> 열 기준 위치로 출력
which(m%%3==0, arr.ind=T) # 인덱스 활용 O -> 배열의 위치가 출력

# data.frame에서 사용
no <- c(1:5)
name <- c("홍길동", "유비", "관우", "장비", "전우치")
score <- c(85, 78, 90, 56, 84)
exam <- data.frame(학번=no, 이름=name, 성적=score)
exam

# 이름이 장비인 사람 검색
which(exam$이름 == "장비")          # 해당값이 존재하는가
which(exam$이름 == "우비")          # 존재X -> integer(0)
exam[which(exam$이름 == "장비"), ]  # 인덱스로 활용 가능
exam[exam$이름 == "장비", ]
exam[4, ]


## whict.max(), which.min() : 숫자에서만 사용

# 가장 높은 점수를 가진 학생?
which.max(exam$성적)
exam[which.max(exam$성적), ]
which.min(exam$성적)
exam[which.min(exam$성적), ]


## any(), all()
x <- runif(5)  # runif(n) : 랜덤으로 0~1사이의 실수 n개를 뽑아내는 함수
x

# x값들 중에서 0.8이상이 있는가?
any(x >= 0.8)

# x값들이 모두 0.7이하인가?
all(x <= 0.7)


#### 반복문 ####

# 1~10까지의 합계
sum <- 0
for(i in seq(1, 10)){
  sum <- sum + i
}
sum

sum <- 0
for(i in seq(1, 10)) sum <- sum + i
sum

#### 함수 ####

## 함수 만들기

# 인자 없는 함수
test1 <- function(){
  x <- 10
  y <- 20
  return(x*y)
}
test1()

# 인자 있는 함수
test2 <- function(x, y){
  a <- x
  b <- y
  return(a-b)
}
test2(10, 5)
test2(y=10, x=5)

# 가변인수
test3 <- function(...){
  # print(list(...))
  for(i in list(...)) print(i)
}
test3(1)
test3(1, 2)
test3(3, "홍길동", 300)

test4 <- function(a, b, ...){
  print(a)
  print(b)
  print("-------------------")
  print(list(...))
}
test4(1, 2, 3, 4, 5)


## 문자열 함수

shopping_list <- c("apples x4", "bag of flour", "bag of sugar", "milk x2")

str_extract(shopping_list, "[a-z]{1,6} [a-z]{1,2}")
str_extract(shopping_list, "[a-z]{1,2} [a-z]{1,2}\\b")
str_extract(shopping_list, "\\b[a-z]+\\b")

# stringr : 정규표현식
install.packages("stringr")
library(stringr)  # = require(stringr)


str1 <- "홍길동35이순신45임꺽정35"

str_extract(str1, "\\d{2}")  # 역슬래쉬 \\ 두번 입력 해줘야함
str_extract_all(str1, "\\d{2}")
str_extract_all(str1, "[0-9]{2}")
class(str_extract_all(str1, "\\d{2}"))


str2 <- "hongkd105leess1002you25TOM400강감찬2005"

str_extract_all(str2, "\\D+")
str_extract_all(str2, "[a-zA-Z가-힣]+")


length(str2)      # ""로 묶인 건 하나로 봄
str_length(str2)  # 총 문자의 개수 출력

str_locate(str2, "강감찬")
str_locate_all(str2, "o")

str_c(str2, "유비55")  # 뒤에 추가하기
str2


## 기본함수

sample <- data.frame(c1=c("abc_abcdefg", "abc_ABCDE", "ccd"), c2=1:3)
sample

nchar(sample[1, 1])   # 문자의 개수 출력
nchar(str2)

which(sample[,1]=="ccd")  # 위치 알려줌

toupper(sample[1, 1])     # 대문자로
tolower(sample[2, 1])     # 소문자로

substr(sample[,1], start = 1, stop = 2)  # 문자 뽑아오기

paste0(sample[,1], sample[,2])           # 단순히 연결만 해줌(1열와 2열이 합쳐짐)
paste(sample[,1], sample[,2], sep="@@")  # 붙일 때 sep지정 가능


## 문자열을 분리해서 하나의 컬럼을 두 개의 컬럼으로 확장

install.packages("splitstackshape")
library(splitstackshape)

cSplit(sample, splitCols = "c1", sep = "_")

# 패키지의 함수 목록 확인
ls("package:splitstackshape")
ls("package:stringr")
