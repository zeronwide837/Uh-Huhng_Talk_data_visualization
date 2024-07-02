setwd("C:/Users/CountBall/Desktop/어흥톡/데이터시각화/network/manager")

library(tm) #텍스트 마이닝 함수
library(wordcloud) #단어구름
library(wordcloud2) #단어구름2
library(SnowballC) #어간추출 함수
library(dplyr) #파이프 연산자
library(KoNLP) #한글 텍스트마이닝
library(stringr) #KoNLP 사용 전 데이터 전처리용
library(tidytext) #unnest_tokens 함수
library(widyr)
library(forcats)#시각화
library(ggplot2)#시각화
library(igraph)#네트워크 그림1
library(ggraph)#네트워크 그림2
library(tidygraph)#그래프 중심성 계산
library(ggrepel)#이름 띄우기

userdata <- read.csv("managerkor.csv")#데이터 읽기

doc = Corpus(VectorSource(userdata$plainText)) #문자열 데이터로 만들기

doc = tm_map(doc, removeNumbers) #숫자 제거
doc = tm_map(doc,removePunctuation) #구두점 제거
doc = tm_map(doc,stripWhitespace) #공백문자 제거
a <- as.character(unlist(doc))#문자형으로 변환

a <- unique(a)
a <- str_replace_all(a,"[^[:alpha:][:blank:]]","") #특수문자 제거

atb <- tibble::as_tibble(a)#tibble로 변환
atb$sentance = 1:dim(atb)[1] #문장별 인덱스 추가

atb %>%
  unnest_tokens(input = value, output = sent, token="sentences") %>%
  mutate(id=as.numeric(1:n())) %>%
  unnest_tokens(input = sent, output=pos, token=SimplePos09) %>%
  select(id,pos) %>%
  filter(str_detect(pos, "/n|/v(|a)")) %>%
  mutate(pos = str_remove_all(pos, "/.*$")) %>%
  filter(nchar(pos) >1) %>%
  filter(pos != "있다" & pos != "하다" & pos != "되다" & pos != "있다" & pos != "안녕하" & pos != "감사" & pos != "않다" & pos != "보다" & pos != "감사합" & pos != "한번" ) %>%
  pairwise_count(pos, id, sort=T, upper = F) -> pw

pw %>% filter(item1 == "블랙보드" | item1 == "교수님" | item1 == "시험" | item1 == "문제" |item1 == "설정" | item1 == "수업" | item1 == "코스") -> spec #특정 단어와 같이 나오는 단어 동시출현 빈도수

spec %>%
  head(163) %>%
  graph_from_data_frame() ->
  pw_graph

set.seed(123)

a <- grid::arrow(type="closed", length = unit(0.01,"inches")) #화살표 그리기

pw_graph %>% as_tbl_graph(directed=F) %>% mutate(eigen = centrality_eigen(), group = group_infomap()) %>% ggraph() + geom_edge_link(color="gray50",alpha=.2) + geom_node_point(aes(color=factor(group),size=eigen)) + geom_node_text(aes(label=name),size=3,repel=T)+theme_graph()+theme(legend.position="none") #고유벡터 중심성 표현하기(중요한 이웃이 얼마나 많은가)
