#### 변수 ####

goods = "냉장고"
goods

# 변수 사용 시 객체 형태로 사용하는 것을 권장
goods.name = "냉장고"
goods.code = "ref001"
goods.price = 600000

# 값을 대입할 경우 = 대신 <- 사용
goods.name <- "냉장고"
goods.code <- "ref001"
goods.price <- 600000

# 데이터 타입 확인
class(goods.name)
class(goods.price)
mode(goods.name)
mode(goods.price)

#### Vector ####

# c()
V <- c(1, 2, 3, 4, 5)
V
class(V)
mode(V)

(V <- c(1, 2, 3, 4, 5))      # ()안에 지정하면 따로 출력없이 바로 출력 가능
print(V <- c(1, 2, 3, 4, 5))

c(1:5)
c(1, 2, 3, 4, "5")    # 전부 문자열로 변환(배열이니까~)

# seq()
? seq()
seq(from=1, to=10, by=1)
seq(1, 10, 2)

# rep()
rep(1:3, 3)

# 벡터의 접근
v = c(1:50)
v[10:45]
length(v)

v[10:length(v)-5]   # 시작과 끝 위치 모두 -5
v[10:(length(v)-5)]

v1 <- c(13, -5, 20:23, 12, -2:3)
v1

v1[1]
v1[c(2, 4)]    # c()를 인덱스 값으로 사용
v1[c(4, 5:8, 7)]
v1[-2]    # "-"제거의 의미, 2번째 값을 제거
v1[c(-2, -4)]
v1[-c(2, 4)]

# 집합 연산
x <- c(1, 3, 5, 7)
y <- c(3, 5)

union(x, y); setdiff(x, y); intersect(x, y)

# 컬럼명 지정
age <- c(30, 35, 40)
names(age) <- c("홍길동", "임꺽정", "신돌석")
age
class(age)

# 특정 변수의 데이터 제거
age <- NULL  # 변수만 삭제
age

# 벡터 생성의 또다른 표현
x <- c(2, 3)
x <- (2:3)  # c 생략 가능(범위로 생성할 때만)


#### Factor ####

# 범주형 데이터로 만들어줌
# 1, 2 .. 숫자로 구분 -> mode()가 numeric으로 나옴

gender <- c("man", "woman", "woman", "man", "man")
gender
class(gender)
mode(gender)
is.factor(gender)

ngender <- as.factor(gender)
ngender
class(ngender)   # 클래스 타입을 알려줌
mode(ngender)    # 클래스 안에 데이터 타입을 알려줌
is.factor(ngender)

plot(gender)     # 범주형이 아니라 안그려짐
plot(ngender)    # default = 막대그래프
table(ngender)   # 빈도표

? factor() # 함수, lever 순서 지정 가능
gfac <- factor(gender, levels = c("woman", "man"))
gfac
plot(gfac)

#### Matrix ####

# matrix()
? matrix()

matrix((1:5))  # 5행 1열
matrix(c(1:11), nrow = 2)                  # 행의 수 지정 가능, 짝이 안맞으면 처음 데이터부터 다시 채워짐
m <- matrix((1:10), nrow=2, byrow = TRUE)  # 데이터 채워지는 방향 지정 가능(byrow=TRUE:행 방향으로 채워짐)
class(m)
mode(m)

# rbind(), cbind()
x1 <- c(3, 4, 50:52)
x2 <- c(30, 5, 6:8, 7, 8)
x1
x2

rbind(x1, x2)    # 행으로 합치기
cbind(x1, x2)    # 열로 합치기

# matrix의 차수 확인
x <- matrix(c(1:9), ncol = 3)
x

length(x); nrow(x); ncol(x); dim(x)  # 전체 길이; 행의 길이; 열의 길이; 차원

# 컬럼명 지정
colnames(x) <- c("one", "two", "three")
x
colnames(x)  # 그냥 사용하면 컬럼이름 불러옴

# apply()
?apply()
apply(x, 1, max)  # 행의 최대값
apply(x, 2, max)  # 열의 최대값
apply(x, 1, sum)  # 행의 합
apply(x, 2, sum)  # 열의 합



#### DataFrame ####

# data.frame
no <- c(1, 2, 3)
name <- c("hong", "lee", "kim")
pay <- c(150.25, 250.18, 300.34)

emp <- data.frame(No=no, Name=name, Payment=pay)  # 컬럼이름=데이터
emp

# read.csv(), read.table(), read.delim() : 파일 읽어올 때 사용
getwd()     # 현재 작업 중인 폴더 알려줌

read.table("../data/emp.txt", fileEncoding = "euc-kr")
txtemp <- read.table("../data/emp.txt", header=TRUE, sep=" ", fileEncoding="euc-kr")            
          # 첫번째 행을 컬럼이름으로, 구분자
class(txtemp)
mode(txtemp)

setwd("../data")  # 자주 사용하는 경로 지정해두기
getwd()


csvemp <- read.csv("emp.csv", fileEncoding = "euc-kr")
csvemp

read.csv("emp.csv", col.names=c("사번", "이름", "급여"), fileEncoding = "euc-kr")
        # 컬럼이름 변경해서 가져올 수 있음
read.csv("emp2.csv", header=FALSE, col.names=c("사번", "이름", "급여"), fileEncoding = "euc-kr") 
        # 컬럼명이 없는 데이터의 경우 FALSE로!


read.table("AWS_sample.txt", sep="#", fileEncoding = "euc-kr") # 구분자 인식 못함
aws <- read.delim("AWS_sample.txt", sep="#", fileEncoding = "euc-kr") # 복잡한 구분자도 인식 잘함

head(aws)      # 6개 데이터만 가져와줌
View(aws)      # 전체 데이터를 엑셀처럼 띄어줌

# 접근
aws[1, 1]      #행, 열 순으로
x1 = aws[1:3, 2:4]
x1

x2 = aws[9:11, 2:4]
x2

cbind(x1, x2)
rbind(x1, x2)

mode(aws[,1])
aws[,1]        # 열 하나만 뽑아오면 벡터
aws[, c(1, 2)] # 배열

aws$AWS_ID     # 컬럼에 잡근시 $ 사용
aws$TM

# 구조 확인
str(aws)       # python info와 비슷

# 기본 통계량
summary(aws)

# apply()
df <- data.frame(x=c(1:5), y=seq(2, 10, 2), z=c("a", "b", "c", "d", "e"))
df

apply(df[, c(1, 2)], 1, sum)  # 행의 합계
apply(df[, c(1, 2)], 2, sum)  # 열의 합계

# 데이터 일부 추출
subset(df, x>=3)
subset(df, x>=2 & y<=6)

# 병합
height <- data.frame(id=c(1, 2), h=c(180, 175))
weight <- data.frame(id=c(1, 2), w=c(80, 75))
merge(height, weight, by.x="id", by.y="id")

#### Array ####

v <- c(1:12)
v

arr <- array(v, c(4, 2, 3))   # 4행 2열 3면면
arr

# 접근
arr[, , 1]    # 첫번째 면
arr[, , 2]    # 두번째 면

# 추출
arr[2, 2, 1]
arr[, , 1][2, 2]  # 첫번째 면을 가져온 후 2행 2열값을 추출

#### List ####

# list()
x1 <- 1
x2 <- data.frame(var1=c(1, 2, 3), var2=c("a", "b", "c"))
x3 <- matrix(c(1:12), ncol = 2)
x4 <- array(1:20, dim=c(2, 5, 2))

x5 <- list(c1=x1, c2=x2, c3=x3, c4=x4)

x5$c2

list1 <- list(c("lee", "kim"), "이순신", 95)
list1

list1[1]
list1[[1]][1]

# unlist()
list1 <- list("lee", "이순신", 95)
list1

un <- unlist(list1)
un
class(un)

# apply는 2차원 데이터만 받음.
# lapply(), sapliy() : vector 또는 그 이상의 차원을 입력받기 위한 방법으로 사용
# lapply() : 반환형이 list
# sapply() : 반환형이 matrix 또는 vector(lapply의 wrapper)

a <- list(c(1:5))
b <- list(c(6:10))

c <- c(a, b)
class(c)

apply(c, 1, max)        # apply는 2차원에서만 사용 가능
x <- lapply(c, max)     # lapply는 list에서 사용하기 위해 만들어진 apply함수
x1 <- unlist(x)
x1

sapply(c, max)
#### 기타 데이터 타입 ####

# 날짜
Sys.Date()        # 현재 날짜
Sys.time()        # 현재 날짜와 시간

a <- "23/3/31"
class(a)
b <- as.Date(a)   # 날짜형으로 변환
b
class(b)

as.Date(a, "%y/%m/%d")   # 포맷팅 해주기
