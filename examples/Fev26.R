#Importar a BD:

library(haven)
path <- "examples/UCI.sav"

UCI_1_ <- read_sav(path)
View(UCI_1_)

#Mudar o nome e apagar a UCI_1_:
UCI <- UCI_1_
rm(UCI_1_)

#Mudar o tipo da variável categórica P1:

UCI$P1 <- factor(UCI$P1, levels=1:3, labels=c("Coimbra", "Lisboa", "Porto"))

mod1 <- lm(Logtempointern~P1, data=UCI)
summary(mod1)

mod2 <- lm(Logtempointern~PRISM, data=UCI)
summary(mod2)

mod3 <- lm(Logtempointern~P1+PRISM, data=UCI)
summary(mod3)

mod4 <- lm(Logtempointern~P1+PRISM+P1*PRISM, data=UCI)
summary(mod4)

#A interação não é significativa. 

anova(mod1,mod3)
anova(mod2,mod3)

#mod3 é preferível. 

anova(mod3,mod4)

#mod3 é preferível. 

library(interactions)
interact_plot(mod4, pred=PRISM, modx=P1, interval=T, plot.points = T)
