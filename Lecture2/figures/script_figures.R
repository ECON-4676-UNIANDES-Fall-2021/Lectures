#######################################################################
#  Author: Ignacio Sarmiento-Barbieri (i.sarmiento at uniandes.edu.co)
# please do not cite or circulate without permission
#######################################################################

rm(list=ls())
cat("\014")

require("ggplot2")
set.seed(10101)

N<-20
x = runif(N,2,10)
u<- rchisq(N,25)
f<- 2 + 5*x
y = f +u 

db<-data.frame(x=x,y=y)

p<-ggplot(db,aes(x=x,y=y)) +
  geom_point(shape=1,alpha=1,size=2) + 
  theme_bw()  +
  #ylim(8,15) +
  theme(legend.position = "none",
        #axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

p
ggsave("fig_1.pdf",height = 7, width = 12)

p + 
  geom_smooth(method = lm, se = FALSE, col = "black",size=.5) +
  geom_segment(aes(y = predict(lm(y ~ x)), yend = y, x = x, xend = x), col = "red")
ggsave("fig_1b.pdf",height = 7, width = 12)

p + 
  geom_line(data=data.frame(spline(db, n=N*10)))
ggsave("fig_1c.pdf",height = 7, width = 12)




# # -----------------------------------------------------------------------
# Risk Example ------------------------------------------------------------
# # -----------------------------------------------------------------------

p<-seq(0,1,0.01)
n<-c(4,400,4000)
p1<-function(p) p*(1-p)/2
p2<-function(p,n) n/(4*(n+sqrt(n))^2)


