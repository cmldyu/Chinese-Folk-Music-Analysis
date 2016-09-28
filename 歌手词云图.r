lizhi<-subset(smallgroup,smallgroup$artistname=="李志")
mix=worker(bylines=T)

wd<-function(name){
library(jiebaR)
temp<-subset(project,project$artistname==name)
temp=strsplit(x = temp$seg,split = " ")
temp=unlist(temp)
termfreq<-as.data.frame(table(temp),stringsAsFactors = F)
termfreq$temp<-tolower(termfreq$temp)
termfreq<-termfreq[!termfreq$temp %in%stopwords$V1,]
termfreq <- termfreq[order(-termfreq$Freq), ]#按降序排列
library(wordcloud)
colors <- brewer.pal(9,"PuBuGn")[4:9]#设定颜色
wordcloud(termfreq$temp, termfreq$Freq, scale = c(3,.5), max.words = 200,  colors = colors,random.order = F, rot.per = 0.1)}#scale控制着词的最大值和最小值
wd("邵夷贝")
wd("魏如萱")
wd("赵雷")
wd("赵照")
wd("野孩子")
wd("痛仰乐队")
wd("左小祖咒")
wd("纣王老胡")
wd("万晓利")
wd("李志")
wd("窦唯")
wd("张楚")
