rm(list=ls())

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(gridExtra)
library(dplyr)
library(data.table)
library(emojifont)
library(emoGG)
library(RColorBrewer)
library(remoji)

hInfo <- read.csv('~/Downloads/heroes_information.csv')
hPowers <- read.csv('~/Downloads/super_hero_powers.csv')

# there are many Captain Marvels
# there is 1 male and 1 female captain Marvel in Marvel Comics

a <- hInfo[hInfo$name %in% c("Captain Marvel","Captain Marvel II","Captain Mar-vell"),]
View(a)

# only 1 Captain Marvel here
h <- hPowers[hPowers$hero_names %in% c("Captain Marvel","Captain Marvel II"),]
View(h)

# only the top 18 MCU heroes + Captain Marvel + Deadpool coz I love him

mHeroes <- hInfo[hInfo$name %in% c('Quicksilver','Iron Man', 'Hawkeye', 'War Machine', 
                                   'Black Widow', 'Star-Lord','Spider-Man', 'Scarlet Witch',
                                   'Falcon', 'Winter Soldier', 'Vision', 'Valkyrie', 'Ant-Man',
                                   'Hulk', 'Doctor Strange', 'Thor', 'Black Panther', 'Captain America', 
                                   'Captain Marvel','Deadpool'),]

View(mHeroes)


# many spiders mans!! needed to do some research to pick the right one.
mHeroes <- mHeroes[!mHeroes$X %in% c('622','624'),]


# get the powers of the 18 MCU superheroes + Captain Marvel + Deadpool coz I love him


mPowers <- hPowers[hPowers$hero_names %in% c('Quicksilver','Iron Man', 'Hawkeye', 'War Machine', 
                                             'Black Widow', 'Star-Lord','Spider-Man', 'Scarlet Witch',
                                             'Falcon', 'Winter Soldier', 'Vision', 'Valkyrie', 'Ant-Man',
                                             'Hulk', 'Doctor Strange', 'Thor', 'Black Panther', 
                                             'Captain America','Captain Marvel','Deadpool'),]

# one hot encode

mPowers[,2:ncol(mPowers)] <- sapply(mPowers[,2:ncol(mPowers)], function(x) as.integer(as.logical(x)))
head(mPowers)


# let's remove all the columns that have only 0 values so that we can narrow down our superheroe's powers
# right now our data has 168 powers
# after only including powers that our superheroes have we have narrowed number of powers down to 76

mPowers <- mPowers[, colSums(mPowers != 0) > 0]
head(mPowers)

mPowers$hero_names <- factor(mPowers$hero_names)
str(mPowers)

# let's get a count of the powers for each of the superheroes
# more power == more powerful superhero

superHeroPowers <- as.data.frame(cbind(as.character(mPowers[,1]),rowSums(mPowers[,2:ncol(mPowers)])))
colnames(superHeroPowers) <- c('Name','nPowers')
str(superHeroPowers)
superHeroPowers$nPowers <- as.numeric(as.character(superHeroPowers$nPowers))
str(superHeroPowers)


ggplot(superHeroPowers, aes(x=reorder(Name,nPowers), y=nPowers)) +
  emoGG::geom_emoji(emoji="1f4aa") + theme_classic() + coord_flip() + ggtitle('Superhero Power Ranking') +
  labs(y='No. of Powers',x="") + theme(plot.title = element_text(hjust = 0.5), axis.text.y=element_text(colour="black"))

# let's take a look at all the powers by superhero
# transpose the mPowers data

t <- as.data.frame(t(mPowers))
t <- data.frame(t)
colnames(t) <- as.character(t[1,])
colnames(t) <- as.character(unlist(t[1,]))
t <- t[-1,]
head(t)

head(t)


colnames(t)

z <- as.data.frame(matrix(ncol=2,nrow = 0))

for (i in 1:ncol(t)) {
  c <- as.data.frame(matrix(ncol=2,nrow = 0))
  a <- rownames(t[t[,i]==1,])
  for (j in 1:length(a)) {
    b <- a[j]
    f <- data.frame(cbind(b, colnames(t[i])))
    c <- rbind(c,f)
  }
  z <- rbind(z,c)
}
View(z)
colnames(z) <- c('Power','shName')

table(z$shName)
View(as.data.frame(table(z$Power)))

# add points for each power

View(as.data.frame(table(z$Power)))


z$points <- ifelse(z$Power=='Intelligence',0.5,
                   ifelse(z$Power=='Size.Changing',1,
                          ifelse(z$Power=='Agility',0.5,
                                 ifelse(z$Power=='Stealth',1,
                                        ifelse(z$Power=='Marksmanship',0.5,
                                               ifelse(z$Power=='Weapons.Master',2.5,
                                                      ifelse(z$Power=='Stamina',0.5,
                                                             ifelse(z$Power=='Enhanced.Senses',0.5,'none'))))))))



z$points <- ifelse(z$Power=='Peak.Human.Condition',3,
                   ifelse(z$Power=='Enhanced.Memory',5,
                          ifelse(z$Power=='Reflexes',0.5,
                                 ifelse(z$Power=='Enhanced.Hearing',0.5,
                                        ifelse(z$Power=='Enhanced.Smell',0.5,
                                               ifelse(z$Power=='Vision...Telescopic',0.5,
                                                      ifelse(z$Power=='Vision...Night',0.5,
                                                             ifelse(z$Power=='Longevity',1,
                                                                    ifelse(z$Power=='Durability',0.5,
                                                                           ifelse(z$Power=='Weapon.based.Powers',0.5,
                                                                                  ifelse(z$Power=='Accelerated.Healing',5,
                                                                                         ifelse(z$Power=='Cold.Resistance',2,
                                                                                                ifelse(z$Power=='Energy.Absorption',2.5,
                                                                                                       ifelse(z$Power=='Flight',0.5,
                                                                                                              ifelse(z$Power=='Danger.Sense',2.5,
                                                                                                                     ifelse(z$Power=='Super.Strength',0.5,
                                                                                                                            ifelse(z$Power=='Energy.Blasts',0.5,
                                                                                                                                   ifelse(z$Power=='Super.Speed',0.5,
                                                                                                                                          ifelse(z$Power=='Teleportation',12.5,
                                                                                                                                                 ifelse(z$Power=='Magic',7.5,
                                                                                                                                                        ifelse(z$Power=='Clairvoyance',1,
                                                                                                                                                               ifelse(z$Power=='Immortality',25,
                                                                                                                                                                      ifelse(z$Power=='Invulnerability',5,
                                                                                                                                                                             ifelse(z$Power=='Energy.Constructs',2.5,
                                                                                                                                                                                    ifelse(z$Power=='Self.Sustenance',0.5,
                                                                                                                                                                                           ifelse(z$Power=='Precognition',1,
                                                                                                                                                                                                  ifelse(z$Power=='Hypnokinesis',0.5,
                                                                                                                                                                                                         ifelse(z$Power=='Levitation',0.5,
                                                                                                                                                                                                                ifelse(z$Power=='Energy.Manipulation',0.5,z$points)))))))))))))))))))))))))))))
                                                                                                                                                                                                                       


z$points <- ifelse(z$Power=='Toxin.and.Disease.Resistance',2.5,
                   ifelse(z$Power=='Omnilingualism',0.5,
                          ifelse(z$Power=='Magic.Resistance',2.5,
                                 ifelse(z$Power=='Super.Breath',0.5,
                                        ifelse(z$Power=='Dimensional.Awareness',0.5,
                                               ifelse(z$Power=='Regeneration',12.5,
                                                      ifelse(z$Power=='Telepathy.Resistance',0.5,
                                                             ifelse(z$Power=='Mind.Control.Resistance',5,
                                                                    ifelse(z$Power=='Telepathy',5,
                                                                           ifelse(z$Power=='Telekinesis',1,
                                                                                  ifelse(z$Power=='Dimensional.Travel',5,
                                                                                         ifelse(z$Power=='Phasing',0.5,
                                                                                                ifelse(z$Power=='Astral.Projection',1.5,
                                                                                                       ifelse(z$Power=='Summoning',0.5,
                                                                                                              ifelse(z$Power=='Force.Fields',0.5,
                                                                                                                     ifelse(z$Power=='Intangibility',0.5,
                                                                                                                            ifelse(z$Power=='Banish',1.5,
                                                                                                                                   ifelse(z$Power=='Time.Travel',20,
                                                                                                                                          ifelse(z$Power=='Illusions',0.5,
                                                                                                                                                 ifelse(z$Power=='Time.Manipulation',15,
                                                                                                                                                        ifelse(z$Power=='Invisibility',7.5,
                                                                                                                                                               ifelse(z$Power=='Animal.Oriented.Powers',0.5,
                                                                                                                                                                      ifelse(z$Power=='Gliding',0.5,
                                                                                                                                                                             ifelse(z$Power=='Jump',0.5,
                                                                                                                                                                                    ifelse(z$Power=='Radiation.Immunity',2, 
                                                                                                                                                                                           ifelse(z$Power=='Radiation.Absorption',0.5,z$points))))))))))))))))))))))))))
                                                                                                                                                                                                  



z$points <- ifelse(z$Power=='Underwater.breathing',1,
                   ifelse(z$Power=='Energy.Beams',0.5,
                          ifelse(z$Power=='Power.Suit',5,
                                 ifelse(z$Power=='Magnetism',7.5,
                                        ifelse(z$Power=='Vision...Thermal',0.5,
                                               ifelse(z$Power=='Molecular.Manipulation',7.5,
                                                      ifelse(z$Power=='Probability.Manipulation',5,
                                                             ifelse(z$Power=='Reality.Warping',12.5,
                                                                    ifelse(z$Power=='Animal.Attributes',0.5,
                                                                           ifelse(z$Power=='Substance.Secretion',2, 
                                                                                  ifelse(z$Power=='Natural.Weapons',0.5,
                                                                                         ifelse(z$Power=='Wallcrawling',0.5,
                                                                                                ifelse(z$Power=='Web.Creation',2.5,
                                                                                                       ifelse(z$Power=='Element.Control',0.5,
                                                                                                              ifelse(z$Power=='Weather.Control',5,
                                                                                                                     ifelse(z$Power=='Odin.Force',40,
                                                                                                                            ifelse(z$Power=='Density.Control',1,
                                                                                                                                   ifelse(z$Power=='Technopath.Cyberpath',1,
                                                                                                                                          ifelse(z$Power=='Natural.Armor',0.5,
                                                                                                                                                 ifelse(z$Power=='Heat.Resistance',2,z$points))))))))))))))))))))



z$points <- ifelse(z$Power=='Intelligence',0.5,
                   ifelse(z$Power=='Size.Changing',1,
                          ifelse(z$Power=='Agility',0.5,
                                 ifelse(z$Power=='Stealth',1,
                                        ifelse(z$Power=='Marksmanship',0.5,
                                               ifelse(z$Power=='Weapons.Master',2.5,
                                                      ifelse(z$Power=='Stamina',0.5,
                                                             ifelse(z$Power=='Enhanced.Senses',0.5,
                                                                    ifelse(z$Power=='Peak.Human.Condition',3,
                                                                           ifelse(z$Power=='Enhanced.Memory',5,
                                                                                  ifelse(z$Power=='Reflexes',0.5,
                                                                                         ifelse(z$Power=='Enhanced.Hearing',0.5,
                                                                                                ifelse(z$Power=='Enhanced.Smell',0.5,
                                                                                                       ifelse(z$Power=='Vision...Telescopic',0.5,
                                                                                                              ifelse(z$Power=='Vision...Night',0.5,
                                                                                                                     ifelse(z$Power=='Longevity',1,
                                                                                                                            ifelse(z$Power=='Durability',0.5,
                                                                                                                                   ifelse(z$Power=='Weapon.based.Powers',0.5,
                                                                                                                                          ifelse(z$Power=='Accelerated.Healing',5,
                                                                                                                                                 ifelse(z$Power=='Cold.Resistance',2,
                                                                                                                                                        ifelse(z$Power=='Energy.Absorption',2.5,
                                                                                                                                                               ifelse(z$Power=='Flight',0.5,
                                                                                                                                                                      ifelse(z$Power=='Danger.Sense',2.5,
                                                                                                                                                                             ifelse(z$Power=='Super.Strength',0.5,
                                                                                                                                                                                    ifelse(z$Power=='Energy.Blasts',0.5,
                                                                                                                                                                                           ifelse(z$Power=='Super.Speed',0.5,
                                                                                                                                                                                                  ifelse(z$Power=='Teleportation',12.5,
                                                                                                                                                                                                         ifelse(z$Power=='Magic',7.5,
                                                                                                                                                                                                                ifelse(z$Power=='Clairvoyance',1,
                                                                                                                                                                                                                       ifelse(z$Power=='Immortality',25,
                                                                                                                                                                                                                              ifelse(z$Power=='Invulnerability',5,
                                                                                                                                                                                                                                     ifelse(z$Power=='Energy.Constructs',2.5,
                                                                                                                                                                                                                                            ifelse(z$Power=='Self.Sustenance',0.5,
                                                                                                                                                                                                                                                   ifelse(z$Power=='Precognition',1,
                                                                                                                                                                                                                                                          ifelse(z$Power=='Hypnokinesis',0.5,
                                                                                                                                                                                                                                                                 ifelse(z$Power=='Levitation',0.5,
                                                                                                                                                                                                                                                                        ifelse(z$Power=='Energy.Manipulation',0.5,
                                                                                                                                                                                                                                                                               ifelse(z$Power=='Heat.Resistance',2,
                                                                                                                                                                                                                                                                                      ifelse(z$Power=='Toxin.and.Disease.Resistance',2.5,
                                                                                                                                                                                                                                                                                             ifelse(z$Power=='Omnilingualism',0.5,
                                                                                                                                                                                                                                                                                                    ifelse(z$Power=='Magic.Resistance',2.5,
                                                                                                                                                                                                                                                                                                           ifelse(z$Power=='Super.Breath',0.5,
                                                                                                                                                                                                                                                                                                                  ifelse(z$Power=='Dimensional.Awareness',0.5,
                                                                                                                                                                                                                                                                                                                         ifelse(z$Power=='Regeneration',12.5,
                                                                                                                                                                                                                                                                                                                                ifelse(z$Power=='Telepathy.Resistance',0.5,
                                                                                                                                                                                                                                                                                                                                       ifelse(z$Power=='Mind.Control.Resistance',5,
                                                                                                                                                                                                                                                                                                                                              ifelse(z$Power=='Telepathy',5,
                                                                                                                                                                                                                                                                                                                                                     ifelse(z$Power=='Telekinesis',1,
                                                                                                                                                                                                                                                                                                                                                            ifelse(z$Power=='Dimensional.Travel',5,
                                                                                                                                                                                                                                                                                                                                                                   ifelse(z$Power=='Phasing',0.5,
                                                                                                                                                                                                                                                                                                                                                                          ifelse(z$Power=='Astral.Projection',1.5,
                                                                                                                                                                                                                                                                                                                                                                                 ifelse(z$Power=='Summoning',0.5,
                                                                                                                                                                                                                                                                                                                                                                                        ifelse(z$Power=='Force.Fields',0.5,
                                                                                                                                                                                                                                                                                                                                                                                               ifelse(z$Power=='Intangibility',0.5,
                                                                                                                                                                                                                                                                                                                                                                                                      ifelse(z$Power=='Banish',1.5,
                                                                                                                                                                                                                                                                                                                                                                                                             ifelse(z$Power=='Time.Travel',20,
                                                                                                                                                                                                                                                                                                                                                                                                                    ifelse(z$Power=='Illusions',0.5,
                                                                                                                                                                                                                                                                                                                                                                                                                           ifelse(z$Power=='Time.Manipulation',15,
                                                                                                                                                                                                                                                                                                                                                                                                                                  ifelse(z$Power=='Invisibility',7.5,
                                                                                                                                                                                                                                                                                                                                                                                                                                         ifelse(z$Power=='Animal.Oriented.Powers',0.5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                ifelse(z$Power=='Gliding',0.5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                       ifelse(z$Power=='Jump',0.5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                              ifelse(z$Power=='Radiation.Immunity',2, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ifelse(z$Power=='Radiation.Absorption',0.5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ifelse(z$Power=='Underwater.breathing',1,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ifelse(z$Power=='Energy.Beams',0.5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ifelse(z$Power=='Power.Suit',5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ifelse(z$Power=='Magnetism',7.5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ifelse(z$Power=='Vision...Thermal',0.5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ifelse(z$Power=='Molecular.Manipulation',7.5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ifelse(z$Power=='Probability.Manipulation',5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ifelse(z$Power=='Reality.Warping',12.5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ifelse(z$Power=='Animal.Attributes',0.5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ifelse(z$Power=='Substance.Secretion',2, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ifelse(z$Power=='Natural.Weapons',0.5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ifelse(z$Power=='Wallcrawling',0.5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ifelse(z$Power=='Web.Creation',2.5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ifelse(z$Power=='Element.Control',0.5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ifelse(z$Power=='Weather.Control',5,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ifelse(z$Power=='Odin.Force',40,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ifelse(z$Power=='Density.Control',1,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ifelse(z$Power=='Technopath.Cyberpath',1,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ifelse(z$Power=='Natural.Armor',0.5,'none')))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

View(z)

# now sum up all the superhero powers

str(z)
z$points <- as.numeric(z$points)

superHero_points <- z %>%
                      group_by(shName) %>%
                      summarise(totalPoints = sum(points)) %>%
                      arrange(-totalPoints)

View(superHero_points)

# add column for number of powers to see what the relationship is between # of powers & power points

superHero_powersPoints <- merge(superHeroPowers,superHero_points, by.x='Name',by.y ='shName',all.x = TRUE )
View(superHero_powersPoints)



# plot most powerful superheroes by number of power points

# get the emoji

search_emoji('fire')
emoji(search_emoji('fire'))

emoji_search('fire')

ggplot(superHero_powersPoints, aes(x=reorder(Name,totalPoints), y=totalPoints)) +
  emoGG::geom_emoji(emoji="1f525") + theme_classic() + coord_flip() + ggtitle('Super Hero Power Points Ranking') +
  labs(y='Power Points',x="") + theme(plot.title = element_text(hjust = 0.5))


# plot power points and shade according to 25%, 50% and 75% quartile thresholds of number of points

# higher number of powers don't necessarily mean more bonus points
# lower number of powers do correspond with lower bonus points

summary(superHero_powersPoints$nPowers)

superHero_powersPoints$nPointLevel <- ifelse(superHero_powersPoints$nPowers>=18.50,'Above Average',
                                       ifelse(superHero_powersPoints$nPowers>=13.20,'Average','Below Average'))


superHero_powersPoints$nPointLevel <- as.factor(superHero_powersPoints$nPointLevel)

str(superHero_powersPoints)

# set color range

my_red = brewer.pal(n = 4, "YlOrRd")[4:2]

ggplot(superHero_powersPoints, aes(x=reorder(Name,totalPoints), y=totalPoints, color = nPointLevel, label=emoji('fire'))) +
  geom_text(family="EmojiOne", size=6) + theme_classic() + coord_flip() + 
  ggtitle('MCU Super Hero Power Profile') + 
  geom_point(size=6, alpha = 0) +
  scale_color_manual(values = my_red,name = "No. Of Powers") +
  labs(y='Power Points',x="") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.8, 0.2),
        axis.text.y=element_text(colour="black")) +
  guides(colour = guide_legend("No. of Powers", override.aes = list(size = 4, alpha = 1)))

# most extreme powers
# powers above 2.5 are extreme powers


View(z)
summary(z$points)

z$pointValue <- ifelse(z$points>2.5,"Extreme","Not Extreme")
table(z$pointValue)

extremePowers <- z[z$pointValue=="Extreme",]
extremePowers$shName <- factor(extremePowers$shName)

table(extremePowers$shName)

# plot which superheros have extreme powers and how many extreme powers they have

str(extremePowers)

emoji_search("trophy")

# sum the points by superhero name

shEP <- extremePowers %>%
          group_by(shName) %>%
          summarise(totalPoints = sum(points)) 

shEPnum <- extremePowers %>%
  group_by(shName) %>%
  summarise(totalPowers = n()) 

View(shEPnum)

shEP <- arrange(shEP, desc(totalPoints))
View(shEP)

str(shEP)

ggplot(shEP, aes(shName, totalPoints)) +
  geom_linerange(
    aes(x = reorder(shName,totalPoints), ymin = 0, ymax = totalPoints), 
    color = "lightgray", size = 1.5
  ) +
  emoGG::geom_emoji(emoji="1f3c6")+
  #ggpubr::color_palette("jco") + 
  coord_flip() + ggtitle('Total Extreme Power Points by Superhero') + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x="",y="Total Points")

# plot number of extreme powers by superhero

ggplot(shEPnum, aes(shName, totalPowers)) +
  geom_linerange(
    aes(x = reorder(shName,totalPowers), ymin = 0, ymax = totalPowers), 
    color = "lightgray", size = 1.5
  ) +
  emoGG::geom_emoji(emoji="1f3c6")+
  #ggpubr::color_palette("jco") + 
  coord_flip() + ggtitle('Total Extreme Powers by Superhero') + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(colour = "black")) + 
  labs(x="",y="No. Of Extreme Powers")




# extremePowers$emoji <- "1f3c6"
# 
# ggplot(extremePowers, aes(shName, points, emoji=emoji))+ emoGG::geom_emoji(label=emoji('1f3c6'))
# 
# ggplot(extremePowers, aes(x = reorder(shName,points), y = points)) +
#   geom_bar(aes(color="black",fill = "black"), stat = "identity", width = 0.5) +
#   scale_fill_hue("", labels = sub_emoji("Extreme :trophy:")) +
#   xlab("") + ylab("No. of Points") + ggtitle("Heroes with Extreme Powers") + coord_flip()
# 
# View(extremePowers)

# ggplot(extremePowers, aes(x = reorder(shName,points), y = points)) +
#   geom_bar(aes(fill = "black"), stat = "identity", width = 0.5) +
#   #scale_fill_hue("", labels = sub_emoji("Extreme :trophy:")) +
#   xlab("") + ylab("No. of Points") + ggtitle("Heroes with Extreme Powers") + coord_flip()




# foo$name_emoji <- as.factor(foo$name_emoji)
# foo$emoji <- as.factor(foo$emoji)
# 
# ggplot(extremePowers, aes(shName, extremePowers$points)) + 
#   geom_bar(stat = "identity") +
#   scale_x_discrete(breaks = sub_emoji("Extreme :trophy:"), labels = sub_emoji("Extreme :trophy:")) +
#   coord_flip() + theme_classic() + ggtitle('Total Extreme Power Points by Superhero') + xlab("Points") +
#   ylab("")


# library(ggplot2)
# library(ggtree)
# 
# dd <- data.frame(x=LETTERS[1:3], y=1:3)
# pie <- ggplot(dd, aes(x=1, y, fill=x)) + 
#   geom_bar(stat="identity", width=1) + 
#   coord_polar(theta="y") + theme_tree() + 
#   xlab(NULL) + ylab(NULL) + 
#   theme_transparent()
# 
# x <- sample(2:9)
# y <- sample(2:9)
# width <- sample(seq(0.05, 0.15, length.out=length(x)))
# height <- width
# 
# p <- ggplot(data=data.frame(x=c(0, 10), y=c(0, 10)), aes(x, y))+geom_blank()
# print(p)
# for (i in seq_along(x)) {
#   p %>% subview(pie, x[i], y[i], width[i], height[i])
#   print(p)
# }

# top 10 most common powers

nPowers <- as.data.frame(table(z$Power))
colnames(nPowers) <- c('Power','Freq')

# Top 10 most common powers and top 10 most rare powers

commonP <- as.data.frame(tail(nPowers[order(nPowers$Freq),]),10)


ggplot(tail(nPowers[order(nPowers$Freq),],10), aes(x=reorder(Power,Freq), y=Freq)) +
  emoGG::geom_emoji(emoji="1f434") + theme_classic() + coord_flip() + ggtitle('Top 10 Most Common Powers') +
  labs(y='Powers',x="") + theme(plot.title = element_text(hjust = 0.5))





# Top 10 most rare powers
rarePowers <- head(nPowers[order(nPowers$Freq),],10)
rarePowers <- rarePowers$Power

heroesWithRarePowers <- z[z$Power %in% rarePowers,]
heroesWithRarePowers

heroesWithRarePowers$Freq <- rep(0.1,10)
heroesWithRarePowers$Freq2 <- rep(0.2,10)

ggplot(heroesWithRarePowers, aes(x=Power, y=Freq)) +
  geom_emoji(emoji="1f984") + theme_classic() + coord_flip() + ggtitle('Top 10 Most Rare Powers') +
  labs(y='',x="") + geom_text(aes(label=shName), hjust=1.2) + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), 
        axis.ticks.y = element_blank(),axis.ticks.x = element_blank(), line=element_blank()) 

#iMan <- z[z$shName=='Iron Man',]

##### Plot most common and rare powers

# top 10 most common powers

commonPowers <- tail(nPowers[order(nPowers$Freq),],10)
rarePowers <- head(nPowers[order(nPowers$Freq),],10)

crPowers <- rbind(commonPowers,rarePowers)

ggplot(crPowers, aes(x=reorder(Power,Freq), y=Freq)) + 
  geom_bar(fill="white",colour="black",stat="identity") +
  add_emoji(emoji="1f4a5") + theme_classic() + coord_flip() + ggtitle('Top 10 Most Common & Rare Powers') +
  labs(y='No. Of Superheroes',x="") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.y=element_text(colour="black"))


# Plot the bonus points of common & rare powers

# check powers with least bonus points and most bonus points

# most bonus points

z$Power <- factor(z$Power)

# only want the power & bonus points

bPoints <- z[,c("Power","points","pointValue")]

View(bPoints[duplicated(bPoints$Power)==TRUE,])

d <- as.data.frame(bPoints[duplicated(bPoints)==TRUE,])

f <- bPoints[!duplicated(bPoints)==TRUE,] 
View(f)

View(bPoints)

mostBonusPoints <- as.data.frame(tail(f[order(f$points),],10))
leastBonusPoints <- as.data.frame(head(f[order(f$points),],10))

mostLeastBonusPoints <- rbind(mostBonusPoints,leastBonusPoints)
View(mostLeastBonusPoints)




# bonusPointsCommonRarePowers <- z[z$Power %in% c('Stamina','Marksmanship','Durability',
#                                                 'Agility','Super.Strength','Longevity','Reflexes','Super.Speed',
#                                                 'Flight','Energy.Blasts','Dimensional.Travel',
#                                                 'Telepathy.Resistance','Regeneration','Magic.Resistance',
#                                                 'Omnilingualism','Energy.Manipulation','Precognition',
#                                                 'Clairvoyance','Enhanced.Smell','Size.Changing'),]


ggplot(mostLeastBonusPoints, aes(x=reorder(Power,points), y=points)) + 
  geom_bar(fill="white",colour="black",stat="identity") +
  add_emoji(emoji="1f4af") + theme_classic() + coord_flip() + ggtitle('Top 10 Most & Least Bonus Points By Power') +
  labs(y='No. Of Points',x="") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.y=element_text(colour="black"))

# let's make a list of the extreme powers
View(extremePowers)

# let's plot how many extreme powers exist and how many superheroes have them

ePSh <- as.data.frame(f %>%
                        group_by(pointValue) %>%
                        summarise(num=n()))

View(ePSh)

dd=data.frame(x=emoji(c("grinning", "cry")), y=c(21, 62))

emoji_text=element_text(family="OpenSansEmoji", size=22)

p <- ggplot(dd, aes(x, y)) + geom_bar(stat='identity', fill="white",colour="black") + 
  ggtitle(paste("No. of Extreme",emoji("grinning"), " vs", "Not Extreme", emoji("cry")," Powers"))+ 
  theme(axis.text.x = emoji_text, plot.title = element_text(hjust = 0.5)) +
  xlab(NULL)+ylab(NULL) + theme_classic() + guides(fill=FALSE)

p+ theme(axis.text.x = emoji_text,plot.title = element_text(hjust = 0.5),axis.line.y = element_blank(),
         axis.text.y  = element_blank(), axis.line.x = element_blank(), axis.ticks = element_blank())

ggplot(ePSh, aes(pointValue,num)) + geom_bar(stat="identity")

# add plot for avg points of extreme vs regular powers

avgPoints <- as.data.frame(f %>%
              group_by(pointValue) %>%
              summarise(a = mean(points), min=min(points),max=max(points)))

View(avgPoints)

dd=data.frame(x=emoji(c("grinning", "smile","cry","cry")), y=c(3, 40,0.5,2.5))
View(dd)

View(f)

f$em <- ifelse(f$pointValue=="Extreme",emoji("grinning"),emoji("cry"))
View(f)

emoji_text=element_text(family="OpenSansEmoji", size=22)

p <- ggplot(f, aes(em,points)) + geom_boxplot() +
      ggtitle(paste("Bonus Point Quartiles of Extreme",emoji("grinning"), 
                    " vs", "Not Extreme", emoji("cry")," Powers")) +
  theme(axis.text.x = emoji_text, plot.title = element_text(hjust = 0.5)) +
  xlab(NULL)+ylab(NULL) + theme_classic() + guides(fill=FALSE)


p+ theme(axis.text.x = emoji_text,plot.title = element_text(hjust = 0.5),axis.line.y = element_blank(),
          axis.line.x = element_blank(), axis.text.y=element_text(colour="black"),axis.ticks = element_blank())


###### 

## add plot for the point distribution of extreme vs not extreme powers



# split powers into rare and common
# if more than 4 superheroes have a power it is common else it is rare
# 61 powers are rare and 22 powers are common

nPowers$Rare <- ifelse(nPowers$Freq>=4,0,1)
table(nPowers$Rare)

# plot the extreme vs not-extreme powers to compare the points given to each power


