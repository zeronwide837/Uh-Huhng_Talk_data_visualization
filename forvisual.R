setwd("C:/Users/CountBall/Desktop/어흥톡")

#library(tm) #텍스트 마이닝 함수
#library(wordcloud) #단어구름
#library(wordcloud2) #단어구름2
#library(SnowballC) #어간추출 함수
#library(dplyr) #파이프 연산자
#library(tidytext)
#library(RColorBrewer) #색깔
#library(tidyverse)
#library(stringr)
#library(tidytext)
#library(igraph)
#library(ggraph)
#library(ggrepel)
#library(tidygraph)

testdata <- read.csv("managerfor.csv")
doc = Corpus(VectorSource(testdata$plainText)) #말뭉치에서 표현 및 컴퓨팅

inspect(doc)

doc = tm_map(doc,content_transformer(tolower))#소문자 변환
doc = tm_map(doc, removeNumbers) #숫자 제거
doc = tm_map(doc,removeWords, stopwords('english')) #지정한 단어 제거
doc = tm_map(doc,removePunctuation) #구두점 제거
doc = tm_map(doc,stripWhitespace) #공백문자 제거

dtm = DocumentTermMatrix(doc) #dim 함수는 DTM의 행과 열의 개수를 알려줌, inspect 함수는 상세 내용을 요약해서 보여줌

dim(dtm)
inspect(dtm)

m = as.matrix(dtm)
v = sort(colSums(m), decreasing=T)
d = data.frame(word=names(v), freq=v)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 100, random.order = F, rot.per = 0.35)
wordcloud2(d)

d2 <- d[d$word != "네" & d$word != "감사합니다" & d$word != "thanks" & d$word != "thank" & d$word != "yes" & d$word != "okay" & d$word != "hello" & d$word != "just" & d$word != "one" & d$word != "much" & d$word != "will" & d$word != "can" & d$word != "many" & d$word != "sure" & d$word != "안녕하세요" & d$word != "수" & d$word != "이" & d$word != "다른" & d$word != "락다운브라우저" & d$word != "블랙보드" & d$word != "없습니다",] #한글, 의미없는 단어 제거 user

d2 <- d[d$word != "네" & d$word != "감사합니다" & d$word != "thanks" & d$word != "thank" & d$word != "yes" & d$word != "okay" & d$word != "hello" & d$word != "just" & d$word != "one" & d$word != "much" & d$word != "will" & d$word != "can" & d$word != "many" & d$word != "sure" & d$word != "안녕하세요" & d$word != "수" & d$word != "이" & d$word != "다른" & d$word != "문의" & d$word != "블랙보드" & d$word != "것"& d$word != "수" & d$word != "선생님" & d$word != "및" & d$word != "넵" & d$word != "다시" & d$word != "학생" & d$word != "지금"& d$word != "합니다"& d$word != "사이트" & d$word != "원격지원" & d$word != "로그인" & d$word != "지원"& d$word != "어떤"& d$word != "이거"& d$word != "할까요" & d$word != "버튼" & d$word != "락다운" & d$word != "것" & d$word != "혹시" & d$word != "또는" & d$word != "우선"& d$word != "환경"& d$word != "교수님"& d$word != "접속"& d$word != "ㅠㅠ"& d$word != "삭제"& d$word != "쿠키"& d$word != "또는"& d$word != "그냥"& d$word != "있는데"& d$word != "해당"& d$word != "혹시"& d$word != "교수자"& d$word != "될까요"& d$word != "같습니다" & d$word != "앗" & d$word != "아"& d$word != "제가"& d$word != "드리겠습니다"& d$word != "그러면"& d$word != "아래의"& d$word != "알겠습니다" & d$word != "브라우저를" & d$word != "컴퓨터" & d$word != "시험" & d$word != "고려대학교" & d$word != "같아요" & d$word != "연결" & d$word != "있습니다" & d$word != "줌" & d$word != "더" & d$word != "후" & d$word != "턴잇인" & d$word != "아마" & d$word != "잘" & d$word != "현재",] #한글, 의미없는 단어 제거 user


d2 %>% filter(freq > 5) %>% wordcloud2

findFreqTerms(dtm, lowfreq = 12) #상위 12개 단어만 표시

findAssocs(dtm, terms = "class", corlimit = 0.3) #terms 단어와 상관관계가 일정 이상인 단어만 표시

windows(width=10,height=10)

barplot(d[1:10,]$freq, las=2, names.arg=d[1:10,]$word, col="lightblue",xlab="word", ylab="frequency")

d2 <- as.character(unlist(doc))#문자형으로 변환
d2tb <- tibble::as_tibble(d2)#tibble로 변환
d2tb$sentance = 1:dim(d2tb)[1] #문장별 인덱스 추가

d2tb %>%
  unnest_tokens(input = value, output = sent, token="sentences") %>%
  mutate(id=as.numeric(1:n())) %>%
  unnest_tokens(input = sent, output=pos, token="words") %>%
  filter(nchar(pos) >1) %>%
  filter(pos != "네" & pos != "감사합니다" & pos != "thanks" & pos != "thank" & pos != "yes" & pos != "okay" & pos != "hello" & pos != "just" & pos != "one" & pos != "much" & pos != "will" & pos != "many" & pos != "sure" & pos != "안녕하세요" & pos != "수" & pos != "이" & pos != "다른" & pos != "버튼" & pos != "또는" & pos != "아래의" & pos != "연결" & pos != "다른" & pos != "원격지원" & pos != "사이트" & pos != "접속" & pos != "hi" & pos != "now") %>%
  pairwise_count(pos, id, sort=T, upper = F) -> d2pw
#외국어 동시 출현

d2pw %>% head(422) %>%
  graph_from_data_frame() ->
  d2pw_graph
d2pw_graph #동시 빈도수 3 이상인 단어 관계 그래프로 표현하기

#for(i in 1:dim(d)[1]){
  #for(j in 1:length(V(d2pw_graph)$name)){
    #if (d$word[i] == V(d2pw_graph)$name[j]){
      #V(d2pw_graph)$wordn[j] <- d$freq[i]
    #} 
  #}
#} #V(d2pw_graph)에서 첫번째 행에 있는 단어들의 총 빈도수 추가하기(문제점:너무 오래 걸림)


#V(d2pw_graph)$name
#V(d2pw_graph)$wordn

set.seed(123)

#a <- grid::arrow(type="closed", length = unit(0.01,"inches")) #화살표 그리기
#ggraph(d2pw_graph) + geom_edge_link(aes(edge_alpha = n) ,show.legend=F, arrow=a, end_cap = circle(.07, "inches")) + geom_node_point(color = "black",fill = "blue" ,shape = 21 ,size = unlist(V(d2pw_graph)$wordn)*0.25) + geom_node_text(aes(label = name), vjust = 1, hjust = 1, repel = T) + theme_void() #그래프 그림 정점 크기는 그 단어의 총 빈도수, 화살표 크기는 연결된 단어들의 동시 출현 빈도수

d2pw_graph %>% as_tbl_graph(directed=F) %>% mutate(degree = centrality_degree(), group = group_infomap()) %>% ggraph() + geom_edge_link(color="gray50",alpha=.2) + geom_node_point(aes(color=factor(group),size=degree)) + geom_node_text(aes(label=name),size=3,repel=T)+theme_graph()+theme(legend.position="none") #중심성 표현하기(이웃이 얼마나 많은가)

d2pw_graph %>% as_tbl_graph(directed=F) %>% mutate(eigen = centrality_eigen(), group = group_infomap()) %>% ggraph() + geom_edge_link(color="gray50",alpha=.2) + geom_node_point(aes(color=factor(group),size=eigen)) + geom_node_text(aes(label=name),size=3,repel=T)+theme_graph()+theme(legend.position="none") #고유벡터 중심성 표현하기(중요한 이웃이 얼마나 많은가)

#can 없는 버전
d2tb %>%
  unnest_tokens(input = value, output = sent, token="sentences") %>%
  mutate(id=as.numeric(1:n())) %>%
  unnest_tokens(input = sent, output=pos, token="words") %>%
  filter(nchar(pos) >1) %>%
  filter(pos != "네" & pos != "감사합니다" & pos != "thanks" & pos != "thank" & pos != "yes" & pos != "okay" & pos != "hello" & pos != "just" & pos != "one" & pos != "much" & pos != "will" & pos != "many" & pos != "sure" & pos != "안녕하세요" & pos != "수" & pos != "이" & pos != "다른" & pos != "버튼" & pos != "또는" & pos != "아래의" & pos != "연결" & pos != "다른" & pos != "원격지원" & pos != "사이트" & pos != "접속" & pos != "hi" & pos != "now" & pos != "can") %>%
  pairwise_count(pos, id, sort=T, upper = F) -> d2pw2

d2pw2 %>% head(417) %>%
  graph_from_data_frame() ->
  d2pw2_graph
d2pw2_graph #동시 빈도수 3 이상인 단어 관계 그래프로 표현하기

#for(i in 1:dim(d)[1]){
  #for(j in 1:length(V(d2pw2_graph)$name)){
    #if (d$word[i] == V(d2pw2_graph)$name[j]){
      #V(d2pw2_graph)$wordn[j] <- d$freq[i]
    #} 
  #}
#} #V(d2pw_graph)에서 첫번째 행에 있는 단어들의 총 빈도수 추가하기(문제점:너무 오래 걸림)

set.seed(123)

a <- grid::arrow(type="closed", length = unit(0.01,"inches")) #화살표 그리기

#ggraph(d2pw2_graph) + geom_edge_link(aes(edge_alpha = n) ,show.legend=F, arrow=a, end_cap = circle(.07, "inches")) + geom_node_point(color = "black",fill = "blue" ,shape = 21 ,size = unlist(V(d2pw2_graph)$wordn)*0.25) + geom_node_text(aes(label = name), vjust = 1, hjust = 1, repel = T) + theme_void() #그래프 그림 정점 크기는 그 단어의 총 빈도수, 화살표 크기는 연결된 단어들의 동시 출현 빈도수


d2pw2_graph %>% as_tbl_graph(directed=F) %>% mutate(degree = centrality_degree(), group = group_infomap()) %>% ggraph() + geom_edge_link(color="gray50",alpha=.2) + geom_node_point(aes(color=factor(group),size=degree)) + geom_node_text(aes(label=name),size=3,repel=T)+theme_graph()+theme(legend.position="none") #중심성 표현하기(이웃이 얼마나 많은가)

d2pw2_graph %>% as_tbl_graph(directed=F) %>% mutate(eigen = centrality_eigen(), group = group_infomap()) %>% ggraph() + geom_edge_link(color="gray50",alpha=.2) + geom_node_point(aes(color=factor(group),size=eigen)) + geom_node_text(aes(label=name),size=3,repel=T)+theme_graph()+theme(legend.position="none") #고유벡터 중심성 표현하기(중요한 이웃이 얼마나 많은가)
