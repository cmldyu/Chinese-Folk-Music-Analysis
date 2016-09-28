library(semnet)
library(extrafont)
#####下面探索那些倾向于在一首曲子中出现的词#####
tdm_track_removed=removeSparseTerms(tdm_track,0.99)#筛选不常用词
g<-coOccurenceNetwork(as.matrix(t(tdm_track_removed)), measure = "conprob")#构造网络
#使用经验累积函数来确认
df<-data.frame()#确认较好的阈值来进行筛选
for(i in  seq(0,1,.01)){
  temp <- length(which(E(g)$weight >= i))
  freq<-data.frame(i,temp)
  df<-rbind(df,freq)
}
plot(df)
g1=delete.edges(g, which(E(g)$weight <=0.42));g1=delete.vertices(g1,which(degree(g1)<1)) # 输入阈值进行筛选
vcount(g1)#节点数
ecount(g1)#边数
V(g1)$cluster = edge.betweenness.community(g1)$membership#进行聚类算法
g1 = setNetworkAttributes(g1, size_attribute=V(g1)$freq, cluster_attribute=V(g1)$cluster)#设置网络图格式
plot.igraph(g1,
            vertex.label.family="STXihei",
            edge.arrow.size=0.1,
            edge.curved=T,
            edge.arrow.width=1)#输出

#####探索用词风格相似的歌手#####
tdm_artist_removed=removeSparseTerms(tdm_artist,0.99)#筛选不常用词
tdm_artist_removed=as.matrix(t(tdm_artist_removed))
# temp=as.data.frame(rowSums(tdm_artist_removed))
# ggplot(temp,aes(x=temp))+geom_histogram(bins=1000)
bigger100<-rowSums(tdm_artist_removed)>=3000
tdm_artist_removed=as.data.frame(tdm_artist_removed)
tdm_artist_removed<-subset(tdm_artist_removed,bigger100==1)
tdm_artist_removed<-t(tdm_artist_removed)
getCosine <- function(m1, m2=NULL){
  m1=as(as(m1, 'dgCMatrix'), 'dgTMatrix')
  norm = sqrt(Matrix::colSums(m1^2))
  m1@x = m1@x / norm[m1@j+1]  
  if(!is.null(m2)){
    norm = sqrt(Matrix::colSums(m2^2))
    m2@x = m2@x / norm[m2@j+1]
    cp = Matrix::crossprod(m1,m2) 
  } else cp = Matrix::crossprod(m1)
  as.matrix(cp)
}#计算余弦相似度
artist.dist=as.matrix(getCosine(tdm_artist_removed))
#进行聚类分析(尝试中)
library(seriation)
o <- seriate(as.matrix(artist.dist))
pimage(artist.dist, main = "Random")
pimage(artist.dist, o, main = "Reordered")
cbind(random = criterion(artist.dist), reordered = criterion(artist.dist, o))
#
g<-coOccurenceNetwork(as.matrix(tdm_artist_removed), measure = "cosine")#构造网络
df2<-data.frame()#确认较好的阈值来进行筛选
g=delete.edges(g, which(E(g)$weight==1))
for(i in seq(0.1,1,.1)){
  temp <- length(which(E(g)$weight >= i))
  freq<-data.frame(i,temp)
  df2<-rbind(df2,freq)
cat(i,"\n")}
plot(df2)
g=delete.edges(g, which(E(g)$weight<=.4))

g=delete.vertices(g,which(degree(g)<1)) # 输入阈值进行筛选
vcount(g)#节点数
ecount(g)#边数
V(g)$cluster = edge.betweenness.community(g)$membership#进行聚类算法
g = setNetworkAttributes(g, size_attribute=5, cluster_attribute=V(g)$cluster)#设置网络图格式
plot.igraph(g,vertex.label.family="STXihei")#输出
# vcount(g_backbone)#计算节点数
# ecount(g_backbone)#计算边数
# V(g_backbone)$cluster = edge.betweenness.community(g_backbone)$membership
# g_backbone = setNetworkAttributes(g_backbone, size_attribute=5, cluster_attribute=V(g_backbone)$cluster)
# plot.igraph(g_backbone,vertex.label.family="STXihei")
