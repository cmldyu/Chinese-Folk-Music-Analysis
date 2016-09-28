topwords<-paste(termfreq$temp[1:5],collapse = "|")
sges<-strsplit(project$seg," ")
project$t10=sapply(sges, function(i) sum(grepl(topwords,i)))
#首先从project数据库中抽取需要分析的部分
at = project[, c("trackname",
  "artistname",
  "albumsize",
  "year",
  "nchar",
  "nchar.unique",
  "popularity",
  "tracktime",
  "t10"
)]
#duplicate用于计算
duplicate=data.frame(table(as.character(project$artistname)),stringsAsFactors = F)
colnames(duplicate)<-c("artistname","freq")
at=merge(at,duplicate,by = "artistname")
#直觉上，高产的歌手进入民谣歌单的可能性更大——但是我还没有每个歌手的歌曲产量信息！

tt <-
  ddply(
    at,
    "artistname",
    summarise,
    t10 = mean(t10),
    tracktime = mean(tracktime / 1000 / 60),
    pop = mean(popularity),
    size = mean(albumsize),
    nchar = mean(nchar),
    nchar.unique = mean(nchar.unique),
    diversity = mean(nchar.unique / nchar)*100,
    n = length(unique(trackname)),
    dur = max(year) - min(year)
  )
#高频词在总歌词中的占比（假定：整首歌都只是重复一些常用的意象，则这个比率更高，歌曲越不受欢迎）
#最后建模的结果如下
#歌曲长度
#歌曲的平均受欢迎程度
#专辑的平均大小
#歌词总数
#歌词中的独立词
#歌词的平均丰富度
#歌手在歌单中出现的次数
#第一张专辑和最近一张专辑的时间差，用于粗略度量歌手的“职业生涯”

a<-sapply(listids, function(i) unique(subset(project2$artistname,project2$listid==i)))
a<-unlist(a)
a=as.data.frame(table(a),stringsAsFactors = F)
colnames(a)<-c("artistname","inlist")
tt=merge(a,tt,by="artistname",all.y = T)
tt=merge(track.amount,tt,by="artistname",all.y = T)
tt=na.omit(tt)
track.amount$trackname<-rownames(track.amount)
tt=subset(tt,tt$V1-tt$n>=0)

m=lm(log(inlist)~dur+t10+V1+pop,tt);m;summary(m)

#这个半弹性模型刻画了出道时间、歌曲流行度，和在民谣歌单中被提名的次数之间的关系，我

