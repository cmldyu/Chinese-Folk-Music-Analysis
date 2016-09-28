#获取歌单的详细信息，包括创建者信息、歌单中的歌曲信息

get.tracks1<-function(listid){
  library(RCurl)
  library(rvest)
  library(RJSONIO)
  musiclist<-paste0("http://music.163.com/api/playlist/detail?id=",listid,"&updateTime=-1")
  
  page_html <- getURL(musiclist)
  
  musiclist<-fromJSON(page_html)
  
  trackCount<-musiclist$result$trackCount#歌曲数
  
  listinfo<-musiclist$result[-5]#去除歌曲信息
  
  listinfo$tags<-paste(listinfo$tags,collapse = "|")
  
  listinfo$creator$expertTags<-paste(listinfo$creator$expertTags,collapse = "|")
  
  listinfo=unlist(listinfo)
  
  listinfo=as.data.frame(t(data.frame(listinfo,stringsAsFactors = F)),stringsAsFactors = F)
  
  listid<-listinfo$id
  
  tracks<-data.frame()
  
  for (i in 1:trackCount){
    track.name<-musiclist$result$tracks[[i]]$name
    track.id<-musiclist$result$tracks[[i]]$id
    playTime<-musiclist$result$tracks[[i]]$lMusic$playTime
    alubm.name<-musiclist$result$tracks[[i]]$album$name
    alubm.id<-musiclist$result$tracks[[i]]$album$id
    album.size<-musiclist$result$tracks[[i]]$album$size
    album.publishtime<-musiclist$result$tracks[[i]]$album$publishTime
    album.publishtime=(album.publishtime+28800000)/86400000
    album.publishtime=as.Date(album.publishtime, origin=as.Date("1970-01-01"))
    artist.name<-musiclist$result$tracks[[i]]$artists[[1]]$name
    artist.id<-musiclist$result$tracks[[i]]$artists[[1]]$id
    track.popularity<-musiclist$result$tracks[[i]]$popularity
    df<-data.frame(trackname=track.name,
                   trackid=track.id,
                   tracktime=playTime,
                   artistname=artist.name,
                   artistid=artist.id,
                   alubmname=alubm.name,
                   albumid=alubm.id,
                   albumsize=album.size,
                   albumpublishtime=album.publishtime,
                   popularity=track.popularity,
                   listid=listid,stringsAsFactors = F)
    tracks<-rbind(df,tracks)}
  return(list(tracks,listinfo))}
#要运行大约10分钟
tracks.raw<-list()
i=1
for (i in i:nrow(list.df)){
  tracks.raw[[i]]<-get.tracks1(list.df$listid[i])
  cat("------------------\n 正处理第",i,"个歌单歌，","共",nrow(list.df),"个歌单歌。\n------------------\n")
  Sys.sleep(0.5)}

list.df2<-ldply(tracks.raw, as.data.frame)
trackdf=list.df2[,1:11]#提取歌单中的歌曲列表
list.df2<-list.df2[,12:61];list.df2<-unique(list.df2)#提取歌单的详细信息
names(list.df2)
list.df3<-list.df2[,c("id","creator.province","creator.gender","creator.city","createTime","playCount","trackUpdateTime","updateTime","shareCount","commentCount")]

list.df3=as.data.frame(sapply(list.df3, as.numeric))

to.date<-function(datenum){
  date=(datenum+28800000)/86400000
  date=as.Date(date, origin=as.Date("1970-01-01"))
               return(date)
}

list.df3$trackUpdateTime=to.date(list.df3$trackUpdateTime)
list.df3$createTime=to.date(list.df3$createTime)
list.df3$updateTime=to.date(list.df3$updateTime)

list.df3$dur<-as.numeric(list.df3$trackUpdateTime-list.df3$createTime)
# write.csv(list.df2,"歌单列表.csv")
a=lm(playCount~shareCount+commentCount+dur,list.df3)
summary(a)
summary(list.df3$playCount)
#使用比较强的过滤：播放量、分享量以及评论量都应该大于中位数
listids<-subset(list.df3$id,list.df3$playCount>=median(list.df3$playCount)&list.df3$shareCount>=median(list.df3$shareCount)&list.df3$commentCount>=median(list.df3$commentCount))
#筛选出这些歌曲
project1<-trackdf[trackdf$listid %in% listids,]
temp=track.df2[,c("trackid","lyric")]
project1<-merge(project1,temp,by="trackid")
project1$lyric<-toTrad(project1$lyric,T)
#探测歌曲语言
ly<-detectLanguage(project1[,"lyric"])[1]=="Chinese"|detectLanguage(project1[,"lyric"])[1]=="ChineseT"
ar<-detectLanguage(project1[,"artistname"])[1]=="Chinese"|detectLanguage(project1[,"artistname"])[1]=="ChineseT"
al<-detectLanguage(project1[,"alubmname"])[1]=="Chinese"|detectLanguage(project1[,"alubmname"])[1]=="ChineseT"
tr<-detectLanguage(project1[,"trackname"])[1]=="Chinese"|detectLanguage(project1[,"trackname"])[1]=="ChineseT"
jp<-grepl(pattern = "[ぁ-ん]|[ァ-ヶ]",x = paste0(project1[,"artistname"],project1[,"lyric"]))

CN<-(ly|ar|al|tr)&!jp#通过字符串编码来确定语言
#提取华语歌曲
project2<-subset(project1,CN==1)
#计算歌手在我可获得的范围内所有单曲数(产量)
allnames<-unique(project$artistname)

# trackamount=function(allnames){
#   temp=subset(track.df2,track.df$artistname==allnames)#选取歌手名
#   alubm.id<-unique(temp[,c("alubmname","albumsize")])
#   sum(alubm.id$albumsize)}

trackamount=function(allnames){
  temp=subset(track.df2,track.df$artistname==allnames[1])#选取歌手名
  temp$trackname<-as.character(temp$trackname)
  length(unique(temp[,c("trackname")]))}

track.amount<-sapply(allnames, function(i) trackamount(i))
track.amount<-t(data.frame(as.list(track.amount)))
track.amount<-as.data.frame(track.amount)
track.amount$artistname<-rownames(track.amount)
project<-project2[,-11]
project<-unique(project)
