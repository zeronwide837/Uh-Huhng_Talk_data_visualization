setwd("C:/Users/CountBall/Desktop/어흥톡/데이터시각화/monthmanager")

#library(tm) #텍스트 마이닝 함수
#library(wordcloud) #단어구름
#library(wordcloud2) #단어구름2
#library(SnowballC) #어간추출 함수
#library(dplyr) #파이프 연산자
#library(KoNLP) #한글 텍스트마이닝
#library(stringr) #KoNLP 사용 전 데이터 전처리용
#library(tidytext) #unnest_tokens 함수
#library(RColorBrewer) #색깔
#library(widyr)
#library(forcats)#시각화
#library(ggplot2)#시각화
#library(igraph)#네트워크 그림1
#library(ggraph)#네트워크 그림2
#library(tidygraph)#그래프 중심성 계산
#library(ggrepel)#이름 띄우기

testdata <- read.csv("Sepmana.csv")#데이터 읽기
doc = Corpus(VectorSource(testdata$plainText)) #문자열 데이터로 만들기
#inspect(doc)
doc = tm_map(doc, removeNumbers) #숫자 제거
doc = tm_map(doc,removePunctuation) #구두점 제거
doc = tm_map(doc,stripWhitespace) #공백문자 제거
a <- as.character(unlist(doc))#문자형으로 변환

a <- unique(a)
a <- str_replace_all(a,"[^[:alpha:][:blank:]]","") #특수문자 제거

userch <- str_replace_all(a,"\\W"," ") #특수문자 제거2 
usnoun <- extractNoun(userch) #한글 명사 추출
wordcount <- table(unlist(usnoun)) #명사 개수 세기
df_word <- as.data.frame(wordcount,stringsAsFactors = F) #데이터프레임으로 변환
df_word <- rename(df_word, word = Var1, freq = Freq) #변수명 바꾸기
df_word <- filter(df_word, nchar(word) >= 2) #한글자 단어 제거
df_word <- df_word %>% arrange(desc(freq)) #내림차순으로 정렬

abc <- c(df_word[df_word$word == "zoom" | df_word$word == "collaborate", ])
abc
