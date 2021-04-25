library(dplyr)
library(ggplot2)
library(mosaicData)
library(datarium)
library(plotly)

Galton <- Galton

str(Galton)

colSums(is.na(Galton))

# average height- both parents looking at both child genders----


Galton$avg_parents=""

avg_h <- (Galton[,2] + Galton[,3])/2

Galton$avg_parents <- avg_h

View(Galton)

colnames(Galton)

Galton <- Galton[,c(1,2,3,7,4,5,6)]

Galton_parents <- Galton[,-c(2,3)]

Galton_parents <- Galton_parents %>% 
  rename(
    child_gender = "sex"
  )


# visuals
ggplot(Galton_parents, aes(x=avg_parents, y=height))+
  geom_point()+
  theme_bw()

ggplot(Galton_parents, aes(x=avg_parents, y=height))+
  geom_point(aes(color=child_gender))+
  theme_bw()

ggplot(Galton_parents, aes(x=avg_parents, y=height))+
  geom_point()+
  facet_wrap(~child_gender)+
  theme_bw()

# correlation btwn parents height and child height
cor(Galton_parents$avg_parents, Galton_parents$height)


# model
Galton_parents_lm <- lm(height~avg_parents, data = Galton_parents) 

summary(Galton_parents_lm)

Galton_lm <- lm(height~mother+father, data= Galton)

summary(Galton_lm)

# father's height on both child genders----

Galton_father <- Galton[,-c(3,4,7)]

Galton_father <- Galton_father %>% 
  rename(
    child_gender = "sex"
  )

# visuals
ggplot(Galton_father, aes(x=father, y=height))+
  geom_point()+
  theme_bw()

ggplot(Galton_father, aes(x=father, y=height))+
  geom_point(aes(color=child_gender))+
  theme_bw()

ggplot(Galton_father, aes(x=father, y=height))+
  geom_point()+
  facet_wrap(~child_gender)+
  theme_bw()

# correlation between father's height and both child heights
cor(Galton_father$father, Galton_father$height)

# model

Galton_father_lm <- lm(height~father, data= Galton_father)

summary(Galton_father_lm)

# mother's height on both child genders----
Galton_mother <- Galton[,-c(2,4,7)]

Galton_mother <- Galton_mother %>% 
  rename(
    child_gender = "sex"
  )

# visuals
ggplot(Galton_mother, aes(x=mother, y=height))+
  geom_point()+
  theme_bw()

ggplotly(ggplot(Galton_mother, aes(x=mother, y=height))+
  geom_point(aes(color=child_gender))+
  theme_bw())

ggplot(Galton_mother, aes(x=mother, y=height))+
  geom_point()+
  facet_wrap(~child_gender)+
  theme_bw()

# correlation btwn mother's height and both child heights
cor(Galton_mother$mother, Galton_mother$height)

# model
Galton_mother_lm <- lm(height~mother, data= Galton_mother)

summary(Galton_mother_lm)





# father's height on male child height----
Galton_father_male <- Galton[,-c(3,6,7)]

Galton_father_male <- Galton_father_male %>% 
                        filter(sex=="M")

  
Galton_father_male <- Galton_father_male %>% 
  rename(
    child_gender = "sex"
  )

# visual
ggplotly(ggplot(Galton_father_male, aes(x=father, y=height))+
  geom_point()+
  theme_bw())

# mother's height - highest
Galton %>% 
  filter(height==79.0 & father==70.0 & sex=="M")

#compare height with male siblings
Galton %>% 
  filter(family==72)


# correlation btwn father's height and male child heights
cor(Galton_father_male$father, Galton_father_male$height)

#model
Galton_father_male_lm <- lm(height~father, data= Galton_father_male)

summary(Galton_father_male_lm)


# mother's height on male child----
Galton_mother_male <- Galton[,-c(2,4,7)]

Galton_mother_male <- Galton_mother_male %>% 
  filter(sex=="M")

Galton_mother_male <- Galton_mother_male %>% 
  rename(
    child_gender = "sex"
  )

# visual
ggplotly(ggplot(Galton_mother_male, aes(x=mother, y=height))+
           geom_point()+
           theme_bw())

#father's height - lowest
Galton %>% 
  filter(height==60.0 & mother==64.5 & sex=="M")

# compare with siblings
Galton %>% 
  filter(family==109)

# correlation btwn mother's height and male child heights
cor(Galton_mother_male$mother, Galton_mother_male$height)

#model
Galton_mother_male_lm <- lm(height~mother, data= Galton_mother_male)

summary(Galton_mother_male_lm)


# father's height on female child height----
Galton_father_female <- Galton %>% 
  filter(sex=="F")

Galton_father_female <- Galton_father_female[,-c(3,4,7)]

Galton_father_female <- Galton_father_male %>% 
  rename(
    child_gender = "sex"
  )

# visual
ggplotly(ggplot(Galton_father_female, aes(x=father, y=height))+
  geom_point()+
  theme_bw())

# mother's height - lowest
Galton %>% 
  filter(height==56.0 & father==68.0)

# compared to siblings
Galton %>% 
  filter(family==155)

# compared to the highest height- same father height
Galton %>% 
  filter(height==68.0 & father==68.0 & sex=="F")

# correlation btwn father's height and female child height
cor(Galton_father_female$father, Galton_father_female$height)

# model
Galton_father_female_lm <- lm(height~father, data= Galton_father_female)

summary(Galton_father_female_lm)



#mother's height on female child height----
Galton_mother_female <- Galton %>% 
  filter(sex=="F")

Galton_mother_female <- Galton_mother_female[,-c(2,4,7)]

Galton_mother_female <- Galton_mother_female %>% 
  rename(
    child_gender = "sex"
  )

# visual
ggplotly(ggplot(Galton_mother_female, aes(x=mother, y=height))+
           geom_point()+
           theme_bw())

# outlier - tallest mother
Galton %>% 
  filter(mother==70.5, height==61.7)

# compare with sibling
Galton %>% 
  filter(family==128)

# correlation btwn mother's height and female child height
cor(Galton_mother_female$mother, Galton_mother_female$height)

#model
Galton_mother_female_lm <- lm(height~mother, data= Galton_mother_female)

summary(Galton_mother_female_lm)
