###############################  2025.11.17.  ###############################
### 1
set.seed(9)
n <- 10
x <- 1:100
y <- seq(-3, 3, by=0.01)

smps <- matrix(rnorm(n * length(x)), ncol=n)

xbar <- apply(smps, 1, mean)
se <- 1 / sqrt(10)
alpha <- 0.05
z <- qnorm(1 - alpha/2)
low <- xbar - z * se
upper <- xbar + z * se

plot(y, type="n", xlab="표본추출", ylab="z", xlim=c(1, 100), ylim=c(-1.5, 1.5), cex.lab=1.8)
abline(h=0, col="red", lwd=2, lty=2)
l.c <- rep(NA, length(x))
l.c <- ifelse(low * upper > 0, "red", "black")
arrows(1:length(x), low, 1:length(x), upper, code=3, angle=90, length=0.02, col=l.c, lwd=1.5)

### 2


# 모평균에 대한 추정(대표본의 경우)
#install.packages(BSDA)
library(BSDA)
x=c(70, 40, 50, 30, 60, 50, 90, 40, 60, 60, 60, 80, 90, 90, 60, 30, 20,
    120, 60, 50, 20, 60, 30, 120, 60, 30, 90, 20, 30, 40, 50, 40, 30, 
    40, 20, 30, 60, 50, 60, 80)
z.test(x, sigma.x = sd(x), conf.level = 0.95)

x=c(80.5, 87.2, 88.4, 87.2, 87.6, 85.1, 86.1, 87.4, 88.2, 89.1)
t.test(x, conf.level = 0.95)





### 3
height <- c(168,160,170,162,168,163,164,167,175,179,161,155)
mean(height) # x=166
sd(height) #s=6.6469
# 95% 신뢰수준 a=0.05/2=0.025 
# 학생 12명이므로 n=12, 즉, n-1=11
# 1/a, n-1에 대한 t분포도 값인 2.201로 계산함!




################################  (3)  2026.9.17.  ################################


df_100 <- data.frame(english = sample(100),
                     math = sample(100),
                     class = sample(2))
mean(df_100$english)
qplot(data = df_100, x = english)



df_100_random <- data.frame(english = rnorm(100, mean = 75, sd = 10),
                            math    = rnorm(100, mean = 70, sd = 15),
                            class   = sample(4))
mean(df_100_random$english)
qplot(data = df_100_random, x = english)


df_exam <- read_excel("3-Data_excel_exam.xlsx")
df_exam_novar <- read_excel("3-Data_excel_exam_novar.xlsx", col_names = F)
head(df_exam_novar)
sub_name <- c("ID", "Class", "Math", "English", "Science")
colnames(df_exam_novar) <- sub_name
head(df_exam_novar)












################################  (2) 2026.9.10.  ################################
a <- 1
a
b <- 2
b
c <- 3
c
d <- 3.5
d
e <- a
f <- g
h = 1
k == 1

t <- c("책")

1b <- a
b1 <- a
B1 <- b


ave_1
ave_2

a+b
a+b+c
4/b
5*b

# 숫자 다섯 개로 구성된 var1 생성
var1 <- 
  
  var1 <- c(1, 2, 5, 7, 8)
var1

# 1~5까지 연속값으로 var2 생성
var2 <- c(1:5)
var2

# 1~5까지 연속값으로 var3 생성
var3 <- seq(1,5)
var3

# 1~10까지 2 간격 연속값으로 var4 생성 
var4 <- seq(1, 10, by=8)   # by=3, by=4, …
var4

var1
var1 + 2
var1 + var2

str1 <- "a" #출력된 값의 앞뒤에 따옴표가 붙어 있으면 문자로 구성된 변수 의미!

str1_1 <- 1
str1_1 + 1

str1_2 <- "1"
str1_2 + 1

str2 <- "text"

str4 <- c("a", "b", "c", "d")
str5 <- c("Hello!", "World", "is", "good!") 
str4 + str5

# 변수 만들기
x <- c(1, 2, 3, 5, 757, 7564, 34, 23, 356) 

# 함수 적용하기
mean(x) #평균
max(x) #최댓값
min(x) #최솟값

# 함수의 결과물을 새 변수로 만들기
x_mean <- mean(x)
x_mean * 2


# 문자를 다루는 함수
str5 <- c("Hello!", "World", "is", "good!")

# 쉼표를 구분자로 str5의 단어들 하나로 합치기
paste(str5, collapse = ",")
paste(str5, collapse = " ")

# ggplot2 패키지 설치
#install.packages("ggplot2")  

# ggplot2 패키지 로드
library(ggplot2)             
mpg
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot") 

qplot(data = mpg, x = hwy) #x축에 hwy (highway)변수 지정 
qplot(data = mpg, x = cty) #x축에 cty (city)
qplot(data = mpg, x = drv, y = hwy) #x축 drv, y축 hwy 
qplot(data = mpg, x = drv, y = hwy, geom = "line") #선 그래프 
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot", color = drv) #상자 그림, drv별 색 표현









################################  (2)  2026.9.10.  ################################

# Exercise 1. Text mining 

#install.packages("wordcloud") # https://cran.r-project.org/web/packages/wordcloud/wordcloud.pdf
#install.packages("readxl")    # https://cran.r-project.org/web/packages/readxl/readxl.pdf
#install.packages("tm")        # https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf

library(wordcloud)
library(tm)
library(RColorBrewer)


# Text of lyrics
golden <- readLines("2-lyrics.text", encoding = "UTF-8")

# 1) Excel or others (not shown)
# 2) R package: tm

# Create a text corpus of Golden lyrics
golden <- Corpus(VectorSource(golden))
?Corpus

# Clean the text
golden <- tm_map(golden, content_transformer(tolower))       # lower case
golden <- tm_map(golden, removePunctuation)                  # remove punctuation
golden <- tm_map(golden, removeNumbers)                      # remove numbers
golden <- tm_map(golden, removeWords, stopwords("english"))  # remove stop words

# Create a term-document matrix
tdm_golden <- TermDocumentMatrix(golden)
mat_golden <- as.matrix(tdm_golden)
word_freqs <- sort(rowSums(mat_golden), decreasing = TRUE)
df <- data.frame(word = names(word_freqs), freq = word_freqs)

# Generate the wordcloud
set.seed(1234)
wordcloud(words = df$word, 
          freq = df$freq, 
          min.freq = 1,
          max.words = 100, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Set3"))



##--------------------  Assignment in class  --------------------##

#install.packages("RColorBrewer") 

library(wordcloud)
library(readxl)
library(RColorBrewer)

df_enz <- read_excel("2-res_enz.xlsx", sheet = "processed")

pal <- brewer.pal(8, "Dark2")

set.seed(1234)
wordcloud(words = df_enz$Enzyme,
          freq = df_enz$Cut,
          min.freq = 1,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.3),
          colors = pal)

