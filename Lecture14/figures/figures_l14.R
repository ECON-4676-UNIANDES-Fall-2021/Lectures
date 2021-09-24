##########################################################
#  Author: Ignacio Sarmiento-Barbieri (ignaciomsarmiento at gmail.com)
# please do not cite or circulate without permission
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")



require("ggplot2")
require("dplyr")

set.seed(23)
N<-7
x<-runif(N)
y<- 4*x-.08*x^2 + abs(rnorm(N))
y[1]<- 4
#
sample<-c("twest",rep("train",N-1))
#sample<-c(rep("train",N))

db<-data.frame(x=x,y=y,sample=sample)
db<- db %>% arrange(x)
db$y[2]<- 1.8
db$y[3]<- 2.5
db$y[5]<- 2.5
max(db$y)

p<-ggplot() +
  geom_point(data=db %>% filter(sample=="train"),aes(x=x,y=y,group=sample,shape=sample,col=sample),size=4) +
  scale_shape_manual(values=c(15, 16))+
  xlim(0.2,1) +
  ylim(1,5.5) +
  theme_bw()  +
  theme(legend.position = "none",
         axis.title =element_blank(),
         panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
         )
p
ggsave("figures/fig_1a.pdf",height = 5, width = 12)

p2 <- p+
    stat_smooth(data=db %>% filter(sample=="train"),aes(x=x,y=y),method="lm", se=FALSE, fill=NA, formula=y ~ poly(x,1, raw=FALSE),colour="black",size=.2,lty=1)
p2
ggsave("figures/fig_1b.pdf",height = 5, width = 12)


p3<- p+
  stat_smooth(data=db %>% filter(sample=="train"),aes(x=x,y=y),method="lm", se=FALSE, fill=NA, formula=y ~ poly(x,2, raw=FALSE),colour="black",size=.3,lty=8) 
p3
ggsave("figures/fig_1c.pdf",height = 5, width = 12)


p4<- p+
  stat_smooth(data=db %>% filter(sample=="train"),aes(x=x,y=y),method="lm", se=FALSE, fill=NA, formula=y ~ poly(x,3, raw=FALSE),colour="black",size=.3,lty=8) 
p4
ggsave("figures/fig_1d.pdf",height = 5, width = 12)

p5<- p+
  stat_smooth(data=db %>% filter(sample=="train"),aes(x=x,y=y),method="lm", se=FALSE, fill=NA, formula=y ~ poly(x,4, raw=FALSE),colour="black",size=.3,lty=8) 
p5
ggsave("figures/fig_1e.pdf",height = 5, width = 12)

p6<- p+  
  stat_smooth(data=db %>% filter(sample=="train"),aes(x=x,y=y),method="lm", se=FALSE, fill=NA, formula=y ~ poly(x,5, raw=FALSE),colour="black",size=.3,lty=8) 
p6
ggsave("figures/fig_1f.pdf",height = 5, width = 12)



p7<-ggplot() +  geom_point(data=db ,aes(x=x,y=y,group=sample,shape=sample,col=sample),size=4) +
  scale_shape_manual(values=c(15, 16))+
  xlim(0.2,1) +
  ylim(1,5.5) +
  theme_bw()  +
  theme(legend.position = "none",
        axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  ) 
p7
ggsave("figures/fig_1g.pdf",height = 5, width = 12)



p8<-p7+
  stat_smooth(data=db ,aes(x=x,y=y),method="lm", se=FALSE, fill=NA, formula=y ~ poly(x,1, raw=FALSE),colour="black",size=.2,lty=1) +
  stat_smooth(data=db %>% filter(sample=="train"),aes(x=x,y=y),method="lm", se=FALSE, fill=NA, formula=y ~ poly(x,5, raw=FALSE),colour="black",size=.3,lty=8) 
p8
ggsave("figures/fig_1h.pdf",height = 5, width = 12)
