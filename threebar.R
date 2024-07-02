setwd("C:/Users/CountBall/Desktop/어흥톡/데이터시각화/monthmanager/vacation")

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

vacationdata1 <- read.csv("Janmana.csv")#데이터 읽기
vacationdata2 <- read.csv("Febmana.csv")#데이터 읽기
vacationdata3 <- read.csv("Julmana.csv")#데이터 읽기
vacationdata4 <- read.csv("Augmana.csv")#데이터 읽기

testdata <- rbind(vacationdata1,vacationdata2,vacationdata3,vacationdata4) #특정기간 데이터 통합

doc = Corpus(VectorSource(testdata$plainText)) #문자열 데이터로 만들기
#inspect(doc)
doc = tm_map(doc, removeNumbers) #숫자 제거
doc = tm_map(doc,removePunctuation) #구두점 제거
doc = tm_map(doc,stripWhitespace) #공백문자 제거
a <- as.character(unlist(doc))#문자형으로 변환

a <- unique(a)
a <- str_replace_all(a,"[^[:alpha:][:blank:]]","") #특수문자 제거

atb <- tibble::as_tibble(a)#tibble로 변환
atb$sentance = 1:dim(atb)[1] #문장별 인덱스 추가

words <- unnest_tokens(tbl = atb, input = value, output = word, token = SimplePos09) #형태소 단위로 쪼개기

unnest_tokens(atb, input=value, output = word, token = SimplePos09) %>% mutate(pos_order = 1:n()) -> pos_res #형태소 단위로 쪼개고 순서값 추가

pos_res %>%
  filter(str_detect(word, "/n")) %>%
  mutate(pos_done = str_remove(word, "/.*$")) -> n_done #명사만 추출하고 형태소 정보 제거

pos_res %>%
  filter(str_detect(word, "/p")) %>%
  mutate(pos_done = str_replace_all(word, "/.*$", "다")) -> p_done #형용사만 추출하고 형태소 정보 제거

n_done %>% arrange(pos_order) %>% filter(nchar(pos_done) >1) %>% select(sentance, pos_done) -> n_done #명사 1글자 제거
p_done %>% arrange(pos_order) %>% filter(nchar(pos_done) >1) %>% select(sentance, pos_done) -> p_done #형용사 1글자 제거

n_done %>% count(pos_done, sort=T) -> nn #명사 개수 세기
p_done %>% count(pos_done, sort=T) -> pn #형용사 개수 세기

wordcloud2(nn)

vacationnn <- nn[nn$pos_done != "확인" & nn$pos_done != "저희" & nn$pos_done != "있습니" & nn$pos_done != "같습니" & nn$pos_done != "감사합" & nn$pos_done != "감사" & nn$pos_done != "감사하겠습니" & nn$pos_done != "되겠습니" & nn$pos_done != "안녕하" & nn$pos_done != "권장드립니" & nn$pos_done != "학우님" & nn$pos_done != "학우분" & nn$pos_done != "확인해주" & nn$pos_done != "확인해보" & nn$pos_done != "확인할" & nn$pos_done != "확인해주시" & nn$pos_done != "경우" & nn$pos_done != "때문" & nn$pos_done != "가능합" & nn$pos_done != "시도해주시" & nn$pos_done != "말씀해주" & nn$pos_done != "말씀해주시" & nn$pos_done != "확인하시" & nn$pos_done != "양해" & nn$pos_done != "해당" & nn$pos_done != "부분" & nn$pos_done != "알겠습니" & nn$pos_done != "조심하시" & nn$pos_done != "문의주신" & nn$pos_done != "^ㅋ" & nn$pos_done != "^ㅎ" & nn$pos_done != "죄송" &nn$pos_done != "죄송합" & nn$pos_done != "하루" & nn$pos_done != "한번" & nn$pos_done != "말씀" & nn$pos_done != "말씀하신" & nn$pos_done != "하겠습니" & nn$pos_done != "확인해" & nn$pos_done != "확인해보겠습니" & nn$pos_done != "해보겠습니" & nn$pos_done != "드리겠습니" & nn$pos_done != "확인했습니" & nn$pos_done != "문의해주시" & nn$pos_done != "말씀하시" & nn$pos_done != "시도해보" & nn$pos_done != "사용하시" & nn$pos_done != "요청해주" & nn$pos_done != "문의사항" & nn$pos_done != "사항" & nn$pos_done != "변경해주시" & nn$pos_done != "부탁드리겠습니" & nn$pos_done != "답변" & nn$pos_done != "^ㅋ^ㅋ^ㅋ^ㅋ^ㅋ" & nn$pos_done != "이거" & nn$pos_done != "문의드립니" & nn$pos_done != "안녕하세",] #필요없는 단어 제거

#library(gridExtra) #ggplot 그래프 병렬 배치용

names(examnn) <- c("word","freq")
names(vacationnn) <- c("word","freq")
names(othernn) <- c("word","freq")

ep <- examnn %>% head(10) %>% mutate(word = fct_reorder(word, freq, .desc=T)) %>% ggplot(aes(x = word, y = freq)) + geom_bar(stat="identity", fill = "skyblue") +coord_flip() + ggtitle("시험기간명사빈도")#상위 1% 단어 빈도수 막대그래프

vp <- vacationnn %>% head(10) %>% mutate(word = fct_reorder(word, freq, .desc=T)) %>% ggplot(aes(x = word, y = freq)) + geom_bar(stat="identity", fill = "orange") +coord_flip() +ggtitle("방학기간명사빈도")#상위 1% 단어 빈도수 막대그래프

op <- othernn %>% head(10) %>% mutate(word = fct_reorder(word, freq, .desc=T)) %>% ggplot(aes(x = word, y = freq)) + geom_bar(stat="identity", fill = "lightgreen") +coord_flip() + ggtitle("기타기간명사빈도")#상위 1% 단어 빈도수 막대그래프

grid.arrange(ep,vp,op,ncol=3)