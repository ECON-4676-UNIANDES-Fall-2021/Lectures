#######################################################################
#  Author: Ignacio Sarmiento-Barbieri (i.sarmiento at uniandes.edu.co)
# please do not cite or circulate without permission
#######################################################################

rm(list=ls())
cat("\014")

require("dplyr")
require("ggplot2")
set.seed(1010)

X<-matrix(c(1,1,1,2,2,3), ncol = 2)
X

y<-c(1,4,2)

qr(X)$qr
qr.Q(qr(X))
qr.R(qr(X))


# Gradient Descent Example ------------------------------------------------


dta<-tibble(lnwage=c(5,10,12.50),educ=c(8,12,16))
y<-dta$lnwage
X<-model.matrix(~educ,data=dta)


beta<-solve(t(X)%*%X)%*%t(X)%*%y


beta[2]

p<-ggplot(dta,aes(x=educ,y=lnwage)) +
  geom_point(alpha=1,size=4) +
  theme_bw()  +
  xlab("Education") +
  ylab("Ln(Wage)") +
  xlim(0,20) +
  ylim(-5,20) +
  theme(legend.position = "none",
        #axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=16)
  )
p
ggsave("fig_1.pdf",height = 7, width = 12)
summary(lm(lnwage~educ,dta))

p<-p+geom_abline(intercept = beta[1], slope = beta[[2]], col="blue",lty="solid",size=1)
p
ggsave("fig_1b.pdf",height = 7, width = 12)
?geom_abline


functest<-function(alpha,beta){ ((5-alpha-beta*8)+(10-alpha-beta*12)+(12.5-alpha-beta*16))}

f_alpha<-function(alpha,beta){ -(2/3)*((5-alpha-beta*8)+(10-alpha-beta*12)+(12.5-alpha-beta*16))}
f_beta<-function(alpha,beta){ -(2/3)*(8*(5-alpha-beta*8)+12*(10-alpha-beta*12)+16*(12.5-alpha-beta*16))}



beta1<-list()
beta2<-list()
beta1[[1]]<- -1
beta2[[1]]<- 2

eta<-0.005
i<-2
tol<-0.000001

difff<-2

while( (difff>tol)==TRUE ){
  
  beta1[[i]]<-beta1[[i-1]]-eta*f_alpha(beta1[[i-1]],beta2[[i-1]])
  beta2[[i]]<-beta2[[i-1]]-eta*f_beta(beta1[[i-1]],beta2[[i-1]])
  
  difff<-abs(functest(beta1[[i]],beta2[[i]])-functest(beta1[[i-1]],beta2[[i-1]]))
  
  i<-i+1
  
}
i-1
beta1[[i-1]]
beta2[[i-1]]


for(j in c(2,3,4,5,7211)){
  p+
    geom_abline(intercept = beta1[[j]], slope = beta2[[j]],size=3, col="red",lty="dashed")
  ggsave(paste0("fig_1_",j,".pdf"),height = 7, width = 12)
  print(beta1[[j]])
  print(beta2[[j]])
}


