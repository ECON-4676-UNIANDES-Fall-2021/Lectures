#######################################################################
#  Author: Ignacio Sarmiento-Barbieri (i.sarmiento at uniandes.edu.co)
# please do not cite or circulate without permission
#######################################################################

rm(list=ls())
cat("\014")

require("dplyr")
require("ggplot2")
set.seed(10101)

N<-30
x = runif(N,2,10)
u<- rchisq(N,20)
f<- 2 + 5*x
y = f +u 

db<-data.frame(x=x,y=y,sample="base")


p<-ggplot() +
  geom_point(data=db,aes(x=x,y=y,color=sample,shape=sample),alpha=1,size=2) + 
  theme_bw()  +
  #ylim(8,15) +
  theme(legend.position = "none",
        #axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

p
ggsave("fig_1.pdf",height = 7, width = 12)

p2<-p + 
  geom_smooth(data=db[db$sample=="base",],aes(x=x,y=y),method = lm, se = FALSE, col = "red",size=.5) +
  annotate("text", x = 8, y = 70, label = "y=19.38+5.44x",col="red") 
p2
ggsave("fig_1b.pdf",height = 7, width = 12)


db2<-data.frame(x=10,y=30,sample="outlier")
db<-rbind(db,db2)

p3<-ggplot() +
  geom_point(data=db,aes(x=x,y=y,color=sample,shape=sample),alpha=1,size=2) + 
  theme_bw()  +
  #ylim(8,15) +
  theme(legend.position = "none",
        #axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )+ 
  geom_smooth(data=db[db$sample=="base",],aes(x=x,y=y),method = lm, se = FALSE, col = "red",size=.5) +
  annotate("text", x = 8, y = 70, label = "y=19.38+5.44x",col="red") 
p3
ggsave("fig_1c.pdf",height = 7, width = 12)
p3 + 
  geom_smooth(data=db ,aes(x=x,y=y),method = lm, se = FALSE, col = "black",size=.5)  +
  annotate("text", x = 8, y = 70, label = "y=19.38+5.44x",col="red") +
  annotate("text", x = 8, y = 50, label = "y=23.37+5.57x",col="black") 
ggsave("fig_1d.pdf",height = 7, width = 12)

lm(y~x,db)
lm(y~x,db %>% filter(sample=="base"))
