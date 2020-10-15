library(statnet)
library(GGally)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

options(scipen = 999) 
wd <- getwd()
setwd(wd)
df1 <- read.csv('Datasets/blm1002to0916.csv', header = TRUE)
#df_f <- read.csv('top20_f.csv', header = TRUE)
df <- read.csv('filtered_blm1002to0916.csv', header = TRUE)
#top20_tweeps <- df1[df1$user_id %in% df_f$id,] 
##Update locations for known tweeps##
#####
df$location[df$user_id== "110396781"] <- "USA"
df$location[df$user_id== "51546100"] <- "USA"
df$location[df$user_id== "1263507019702640640"] <- "USA"
df$location[df$user_id== "1604931252"] <- "USA"
df$location[df$user_id== "1639605836"] <- "USA"
df$location[df$user_id== "3186988119"] <- "USA"
df$location[df$user_id== "329802695"] <- "USA"
df$location[df$user_id== "2623871254"] <- "USA"
df$location[df$user_id== "19174660"] <- "USA"
df$location[df$user_id== "1106758838055776256"] <- "USA"
df$location[df$user_id== "1223153582247161856"] <- "USA"
df$location[df$user_id== "1257412486036221952"] <- "USA"
df$location[df$user_id== "1304173589751640064"] <- "USA"
df$location[df$user_id== "168321389"] <- "USA"
df$location[df$user_id== "1279715873226469376"] <- "USA"
df$location[df$user_id== "4970411"] <- "Asia"
df$location[df$user_id== "543761210"] <- "USA"
df$location[df$user_id== "23083404"] <- "USA"
df$location[df$user_id== "239509917"] <- "USA"
df$location[df$user_id== "1136465778495164416"] <- "UK"
df$location[df$user_id== "1240471171226718208"] <- "USA"
df$location[df$user_id== "143169070"] <- "USA"
df$location[df$user_id== "1482663290"] <- "USA"
df$location[df$user_id== "16370091"] <- "USA"
df$location[df$user_id== "1099803821407199232"] <- "USA"
df$location[df$user_id== "1217159648400302080"] <- "USA"
df$location[df$user_id== "1271674230636187648"] <- "USA"
df$location[df$user_id== "1274684460802473984"] <- "USA"
df$location[df$user_id== "1297282642367385600"] <- "USA"
df$location[df$user_id== "1356002694"] <- "USA"
df$location[df$user_id== "154595025"] <- "USA"
df$location[df$user_id== "4561525173"] <- "USA"
df$location[df$user_id== "838634641"] <- "USA"

df$location[df$user_id== "1303024847711137792"] <- "Canada"
df$location[df$user_id== "2864306434"] <- "Africa"

df$location[df$user_id== "824236563072778240"] <- "Europe"
df$location[df$user_id== "987143438"] <- "USA"



df$location[df$user_id== "202924368"] <- "USA"
df$location[df$user_id== "253170287"] <- "USA"

df$location[df$screen_name== "Skeksis4"] <- "USA"

df$location[df$user_id== "215207998"] <- "USA"
df$location[df$user_id== "239468506"] <- "USA"
df$location[df$user_id== "245859231"] <- "USA"
df$location[df$user_id== "252751061"] <- "USA"
df$location[df$user_id== "27676444"] <- "USA"
df$location[df$user_id== "27793335"] <- "USA"
df$location[df$user_id== "2835451658"] <- "Canada"
df$location[df$user_id== "34113439"] <- "USA"
df$location[df$user_id== "373157754"] <- "USA"
df$location[df$user_id== "970207298"] <- "USA"
df$location[df$user_id== "72762011"] <- "USA"
df$location[df$user_id== "70740986"] <- "USA"
df$location[df$user_id== "415605847"] <- "USA"
df$location[df$user_id== "38228095"] <- "USA"
df$location[df$user_id== "3793889669"] <- "UK"
df$location[df$user_id== "4359164413"] <- "USA"
df$location[df$user_id== "1268316577537110016"] <- "USA"
df$location[df$user_id== "1271905564654743552"] <- "USA"
df$location[df$user_id== "1301597534478118912"] <- "USA"
df$location[df$user_id== "1305894568248750080"] <- "USA"
df$location[df$user_id== "415605847"] <- "India"
df$location[df$user_id== "753623149929869312"] <- "USA"
df$location[df$user_id== "799047255378391040"] <- "USA"
df$location[df$user_id== "1261360125203427328"] <- "USA"

#####
##Filter columns, Keep (12): "user_id", "reply_to_user_id", "mentions_user_id", "location", 
##"protected", "followers_count", "friends_count", "listed_count", "statuses_count", 
##"favourites_count","account_created_at","verified"
drops <- c("status_id", "created_at", "text","source", "display_text_width", "reply_to_status_id", 
           "is_quote", "favorite_count", "retweet_count","quote_count", "reply_count","hashtags",                
           "symbols", "urls_url", "urls_t.co", "media_url", "media_t.co", "media_expanded_url",      
           "media_type", "ext_media_url", "ext_media_t.co", "quoted_status_id", "quoted_text",             
           "quoted_created_at", "quoted_source", "quoted_favorite_count", "quoted_retweet_count",    
           "quoted_user_id", "quoted_followers_count", "quoted_friends_count", "quoted_statuses_count",  
           "quoted_location", "quoted_verified", "place_name", "place_type", "country_code")
names(df)

df <- df[ , !(names(df) %in% drops)]
colnames(df)

######
##Replace date of account creation with Boolean TRUE if account created before Floyd's death
time <- df$account_created_at
time <- as.Date(time)
class(time)
z=1
tlist = list()
g_date = as.Date("2020-05-25")
head(tlist)
for (i in time){
  if (i > g_date)
  {
    tlist[[z]] = "FALSE"
  }
  else
  {
    tlist[[z]]= "TRUE"
  }
  z= z+1
}

floyd_B <- unlist(tlist, use.names=FALSE)
df$account_created_at <- floyd_B

rm(tlist, floyd_B, drops, g_date, i, z, time)
##create data frame for unique nodes and extract features
drops <- c("reply_to_user_id", "mentions_user_id")
df_nodes <- df[ , !(names(df) %in% drops)]
df_nodes <- df_nodes %>% distinct(user_id, .keep_all = TRUE)
df_nodes$user_id <- as.character(df_nodes$user_id)
  #sort by user_id
df_nodes <- df_nodes[order(df_nodes$user_id),]


#Create edge list from mentions of other tweeps
#  filter(!is.na(mentions_user_id)) %>% #remove isolates
m_edgelist <-  df %>% #take our tweets and apply the rest of the commands in the pipe to it
  select(user_id, mentions_user_id) 
m_edgelist$user_id <- as.character(m_edgelist$user_id)
m_edgelist$mentions_user_id <- gsub("c\\(\"", "", m_edgelist$mentions_user_id)
m_edgelist$mentions_user_id <- gsub("\\)", "", m_edgelist$mentions_user_id)
m_edgelist$mentions_user_id <- gsub("\"", "", m_edgelist$mentions_user_id)

dlist = list()
z = 0
is.na(m_edgelist[2,]$mentions_user_id)
for(i in 1:nrow(m_edgelist)) {

  mentions <- str_split(m_edgelist[i,]$mentions_user_id, ', ')
  for (p in mentions[[1]]){
    z = z + 1
    conn<-data.frame(user = m_edgelist[i,]$user_id, mention = p)
    dlist[[z]] <- conn
    }
  
}

all_data <- do.call(rbind, dlist)
colnames(all_data) <- c("node1", "node2")
all_data$node2[is.na(all_data$node2)]<- "0"
all_data_ <- all_data %>%
  filter(node2 != node1)
all_data_ <- all_data_ %>%
  filter(all_data_$node2 != '0')
#Create edge list for replies
r_edgelist <-  df %>% #remove duplicate already added in mention
  filter(mentions_user_id != reply_to_user_id) %>% #take our tweets and apply the rest of the commands in the pipe to it
  select(user_id, reply_to_user_id) %>% #select only these two columns
  filter(!is.na(reply_to_user_id)) %>% #remove isolates
  filter(user_id != reply_to_user_id)
colnames(r_edgelist) <- c("node1", "node2")
## Merge replies and mentions to interaction edge list

interactions <- rbind(all_data_, r_edgelist)
interactions <- unique(interactions)
  #filter mentions of members out of the network
edge_list <- interactions %>%
  filter(interactions$node2 %in% interactions$node1 )

#8478 total interactionsin the network
rm(m_edgelist, dlist, df, all_data, all_data_,conn, mentions, r_edgelist, drops, i, p, z)
### unique interactions 4693
edge_list_u <-unique(edge_list)
edge_list_u <- edge_list_u[order(edge_list_u$node1),]

##4592 nodes
users <-c(edge_list_u$node1, edge_list_u$node2)
users <- unique(users)
users <- sort(users, decreasing = FALSE)
#Formula to Convert numeric to cat
convert_l <- function(x){
  if (as.numeric(x) >10000000) { return ("More than 10M")
  } else if (as.numeric(x) >1000000 & as.numeric(x) < 10000001) {return( "More than 1M") 
  } else if (as.numeric(x) >100000 & as.numeric(x) < 1000001) {return( "More than 100K") 
  } else if (as.numeric(x) >10000 & as.numeric(x) < 100001) { return( "More than 10K")
  } else if (as.numeric(x) >1000 & as.numeric(x) < 10001) {return( "More than 1K") 
  } else if (as.numeric(x) >100 & as.numeric(x) < 1001) {return( "More than 100") 
  } else if (as.numeric(x) >10 & as.numeric(x) < 101) { return( "More than 10") 
  }else if (as.numeric(x) < 11){ return( "10 or less") }
}

df_netnodes <- df_nodes %>% filter(df_nodes$user_id %in% users)
rm(df_nodes,edge_list, interactions, users)
df_netnodes <- df_netnodes[order(df_netnodes$user_id),]
###Locations####
df_netnodes$location[grep(", UK", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep(", Canada", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep(", ON", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep(", On", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep(" US", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" us", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" WA", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" CA", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" FL", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" IL", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" NY", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" TX", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" AZ", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" NM", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" CO", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" DE", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" HI", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" KS", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" MI", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" MA", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" MN", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" NC", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" IA", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" AL", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" NE", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" WI", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" NH", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" GA", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" UT", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" tx", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("regon", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" VA", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" Tex", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" ND", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("rkansas", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("ASSACH", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("oston", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("issour", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("laska", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" OR", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(" NS", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep("lorida", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Canada", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep("canada", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep(" ON", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep(" On", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep("Finlad", df_netnodes$location)] <- 'Europe'
df_netnodes$location[grep("england", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("England", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("London", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Ireland", df_netnodes$location)] <- 'Europe'
df_netnodes$location[grep("Scotland", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("ales", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Manchester", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Liverpool", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Germany", df_netnodes$location)] <- 'Europe'
df_netnodes$location[grep("France", df_netnodes$location)] <- 'Europe'
df_netnodes$location[grep("Ankara", df_netnodes$location)] <- 'Asia'
df_netnodes$location[grep("asian", df_netnodes$location)] <- 'Asia'
df_netnodes$location[grep(" PA", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Manhattan", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Yorkshire", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Birmingham", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep(" OH", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Wisconsin", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("ichita", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Detroit", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(", BC", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep("West", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("USA", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Columbia", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep("Wakanda", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(", RI", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Van", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep("Waldorf", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(", MS", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Spain", df_netnodes$location)] <- 'Europe'
df_netnodes$location[grep("Waldorf", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(", MS", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Upstate", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Vegas", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Michigan", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("United States", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Vermont", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("U.S.", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Mumbai", df_netnodes$location)] <- 'Asia'
df_netnodes$location[grep("Taiwan", df_netnodes$location)] <- 'Asia'
df_netnodes$location[grep("Tulsa", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Ukraine", df_netnodes$location)] <- 'Europe'
df_netnodes$location[grep("Fowey", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Fairfield", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("EUROPE", df_netnodes$location)] <- 'Europe'
df_netnodes$location[grep("Coast", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Evergreen", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("D.C.", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Louis", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("AR", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("LONDON", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("DC", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(", Ca", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("jersey", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Moscow", df_netnodes$location)] <- 'Europe'
df_netnodes$location[grep("MIT", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Minneapolis", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Iowa", df_netnodes$location)] <- 'USA'

df_netnodes$location[grep("6.55", df_netnodes$location)] <- 'Africa'
df_netnodes$location[grep("76.", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("33.", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("75.", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("122.", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("66.11", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("United ", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Territory", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Trump", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Washington", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Cardiff", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Ottawa", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep("Toronto", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep("America", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Beach", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Netherland", df_netnodes$location)] <- 'Europe'
df_netnodes$location[grep("California", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Texas", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("APPLE", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("apple", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Apple", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Wormholt", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Georgia", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("TEXAS", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Atlanta", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Republican", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("MD", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Coventry", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Denver", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Colorado", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Kentucky", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Mississippi", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Edinburgh", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("DALLAS", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("New York", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("CT", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Arkham", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Harlem", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Hamilton", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Halifax", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep("Finland", df_netnodes$location)] <- 'Europe'
df_netnodes$location[grep("Nevada", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Japan", df_netnodes$location)] <- 'Asia'
df_netnodes$location[grep("NJ", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Calif", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Hollywood", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Los Angeles", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Hawaii", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("South Africa", df_netnodes$location)] <- 'Africa'
df_netnodes$location[grep("Houston", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Zimb", df_netnodes$location)] <- 'Africa'
df_netnodes$location[grep("Guthorpe", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Dakota", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Britain", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("SC", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Maryland", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Ohio", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("FLORIDA", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("FL", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Ca.", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Europ", df_netnodes$location)] <- 'Europe'
df_netnodes$location[grep("Essex", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Alberta", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep("Epstein", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("English", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Etihad", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Rhode", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Massach", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Jersey", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Jerusalem", df_netnodes$location)] <- 'Asia'
df_netnodes$location[grep("Biden", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Kansas", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Kingston", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("TN", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Bangladesh", df_netnodes$location)] <- 'Asia'
df_netnodes$location[grep("Leeds", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("India", df_netnodes$location)] <- 'Asia'
df_netnodes$location[grep("Malawi", df_netnodes$location)] <- 'Africa'
df_netnodes$location[grep("Manila", df_netnodes$location)] <- 'Asia'
df_netnodes$location[grep("Maldives", df_netnodes$location)] <- 'Asia'
df_netnodes$location[grep("Mayo", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Balham", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Tampa", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Estonia", df_netnodes$location)] <- 'Europe'
df_netnodes$location[grep("Sydney", df_netnodes$location)] <- 'Australia'
df_netnodes$location[grep("Surrey", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Swansea", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Tadworth", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Sweden", df_netnodes$location)] <- 'Europe'
df_netnodes$location[grep("St. ", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(", Mo", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("michigan", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Louisiana", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("colorado", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Smashville", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Singapore", df_netnodes$location)] <- 'Asia'
df_netnodes$location[grep("SF", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Seattle", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Saskatchewan", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep("Francisco", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Diego", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Silicon", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Russia", df_netnodes$location)] <- 'Europe'
df_netnodes$location[grep("NV", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Virginia", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("New Hampshire", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Phoenix", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep(",PA", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Paris", df_netnodes$location)] <- 'France'
df_netnodes$location[grep("KY", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Angeles", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Pacific", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Oxford", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Oklahoma", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Ojibwa", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("NYC", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Ohip", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Nottingham", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("Nova Scotia", df_netnodes$location)] <- 'Canada'
df_netnodes$location[grep(", IN", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Queensland", df_netnodes$location)] <- 'Australia'
df_netnodes$location[grep("Newcastle", df_netnodes$location)] <- 'UK'
df_netnodes$location[grep("New York", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("New Orleans", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Delhi", df_netnodes$location)] <- 'Asia'
df_netnodes$location[grep("Nebraska", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Nevada", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Dallas", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Nash", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Mojave", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Alabama", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Modena", df_netnodes$location)] <- 'Europe'
df_netnodes$location[grep("Mid", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Miami", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Memphis", df_netnodes$location)] <- 'USA'
df_netnodes$location[grep("Melbourne", df_netnodes$location)] <- 'USA'

df_netnodes$location[grep("Philly", df_netnodes$location)] <- 'USA'

######
location <- df_netnodes$location
protected <- df_netnodes$protected

fo_count <- df_netnodes$followers_count
max(fo_count) #47604978
fo_count_1 = list()
for (i in 1:length(fo_count)){
  fo_count_1[[i]]=convert_l(fo_count[i])}
unique(fo_count_1)


fr_count <- df_netnodes$friends_count
max(fr_count) #695178
fr_count_1 = list()
for (i in 1:length(fr_count)){
  fr_count_1[[i]]=convert_l(fr_count[i])}
unique(fr_count_1)

l_count <- df_netnodes$listed_count
max(l_count) #51406
l_count_1 = list()
for (i in 1:length(l_count)){
l_count_1[[i]]=convert_l(l_count[i])}
unique(l_count_1)

s_count <- df_netnodes$statuses_count
max(s_count) #2166318
s_count_1 = list()
for (i in 1:length(s_count)){
  s_count_1[[i]]=convert_l(s_count[i])}
unique(s_count_1)

fv_count <- df_netnodes$favourites_count
max(fv_count) #825138
fv_count_1 = list()
for (i in 1:length(fv_count)){
  fv_count_1[[i]]=convert_l(fv_count[i])}
unique(fv_count_1)

gf_date <- df_netnodes$account_created_at
verified <- df_netnodes$verified

#Filter locations
###USA###
#########
usa<- c('Americaüá∫üá∏‚ô•Ô∏èüá∫üá∏','Baltimore, MDüá∫üá≤',"WA","A Republican ran state. üòÅ","West Hollywood, CA",
        "Winston-Salem, NC","Wisconsin, USA",'CO ‚û°Ô∏è Fayetteville, AR ‚û°Ô∏è CO',
        'College Station, Texas, USA','Colonial Park, PA','Colorado Springs','Colorado Springs, CO',
        'Colorado, USA ; in way of VA','Columbia MO','Columbus | Nashville | DC','Columbus, OH 2 Orlando, Fl ',
        'Communist California ','Compton', 'U S A ', 'TX,FL,WA', 'TX,FL,WA',
        "Wilmington NC","Albany, NY ",'Albany, NY ','Albuquerque, NM ',
  "Tulsa, OK","usa","USA ","Vermont","Vermont, USA","Virginia","Virginia Beach, VA","Vancouver, WA",
  "massachusetts","PA","memphis, tn", "East Lansing, MI", "Fort Worth, TX", "Honolulu, HI","Vermont",
        "Hutchinson, MN", "Maine, USA", "Queens, NY", "Richmond, VA", "Tampa, Fl", "Ventura, CA",
        "Lancaster, PA", "Lincoln, NE", "Merced, CA", "Milwaukee, WI", "new york city", "Norfolk, VA",
        "Orlando, FL", "Pasadena, CA", "Pittsburgh, PA", "Raleigh, NC", "Rhode Island", "Salem, MA",
        "San Antonio, Texas", "san francisco", "San Francisco Bay Area", "San Jose, CA", "Southern California ",
        "St. Petersburg, Florida", "Tempe, AZ", "Tennessee", "TEXAS", "Thousand Oaks, CA",
        "Colorado","Columbus, Ohio", "Connecticut", "Connecticut, USA", "Corpus Christi, TX","Dallas",
        "Flint, MI", "Gainesville, FL", "Houston", "Illinois", "Jacksonville, FL", "Kirkland, WA",
        "Lancaster, PA", "Lincoln, NE", "Merced, CA", "Milwaukee, WI", "new england", "new jersey",
        "New Jersey, USA", "New York", "Minneapolis, MN", "Michigan, USA", "Miami, FL",
        "United States of America", "Virginia, USA", "California", "Detroit, MI", "Florida",
        "Portland, Oregon", "Pennsylvania, USA", "Ohio, USA", "Columbus, OH", "Cleveland, OH",
        "Arizona, USA", "Oakland, CA", "Maryland, USA", "Louisville, KY", "Houston, TX",
        "Dallas, TX", "Chicago", "Denver, CO", "Austin, TX", "NYC", "San Diego, CA", "San Francisco, CA",
        "Los Angeles, CA", "United States", "Washington, DC", "Seattle, WA", "California, USA", 
        "New York, NY", "Portland, OR", "New York, USA", "Los Angeles", "Brooklyn, NY", "Atlanta, GA",
        "Boston, MA", "Florida, USA", "Texas, USA", "New York City", "Chicago, IL", "Philadelphia, PA",
        "North Carolina, USA","Charlotte, NC", "Washington, D.C.", "Washington DC", "Oregon, USA",
        "Kansas City, MO", "Las Vegas, NV", "Massachusetts, USA", "Missouri, USA", "Phoenix, AZ",
        "Illinois, USA", "Georgia", "Colorado, USA", "Tucson, AZ", "Texas", "Seattle", "Georgia, USA",
        "Indianapolis, IN", "Kentucky, USA", "Manhattan, NY", "Nashville, TN", "Orange County, CA",
        "Sacramento, CA", "San Antonio, TX", "South Carolina, USA", "Baltimore, MD", "Atlanta",
        "U.S.A.", "Southern California", "CA", "DC", "DMV", "Michigan", "Ohio", "New York ",
        "New Mexico, USA",'worldwide',  "SoCal", "Pennsylvania", "Salt Lake City, UT",
        "Washington, USA", "Tennessee, USA", "Philadelphia, PA", 'Amongst La Familia!',"Home",
        "The Upside Down", "Blue Georgia in the New South", "Doral, FL", "Wearing a Mask", "Cypress, CA",
        "Upstate NY, USA", "Montana, USA", "NYC, NY", "Minnesota", "#Amerikkka", "Eugene, OR",
        "EAGLES NATION NJ USA #wtp2020", "Bronx, NY", "Georgetown, KY, USA, Earth 2",
        "Las Vegas &  Indianapolis", "Ithaca, NY", "Roslindale, Boston", "North America", "Los Angeles", 
        "Santa Monica, CA", "Minnesota, USA", "New York City & New Jersey & Washington, D.C", "Minnesota, USA",
        "Maine", "37.235548, -115.801053", "Spring Valley, Minnesota", "Occupied Duwamish Territory", 
        "North Philly", "Upper Sandusky, OH", "H-Town vicious", "Buffalo, NY", "Houston, Texas",
        "ÜT: 27.9136024,-81.6078532", "Kansas", "United State", "Santa Barbara, CA", "Greenland, USA",
        "Lawton, OK", "Mpls/LA", "Santa Cruz, CA #LoveNotHate", "American River", "69th Street, Cool Town USA",
        "Soviet Florida USSA", "Tigard, OR", "San Antonio", "Somerset, NJ", "Lake Elsinore, California",
        "Indiana, USA", "Cleveland", "Buffalo, NY", "North Carolina", "Indiana", "Gladstone, MO", "Loisaida (LES), NYC",
        "Eastern Shore, MD & Phila, PA", "New Albany, IN", "On The Bayou-Texas, USA", "PNW",
        "West Valley City, Utah, USA", "Original Northwood, Baltimore", "Standing Rock", 
        "Kew Gardens (Queens) NY", "Alabama", "Liberal Coast", "California", 
        "Tulsa, Okla./Atlanta, Ga.", "Las Vegas & Palm Springs", "Port St. Lucie, FL", 
        "Somewhere in North America", "The Pecker of the USA", "#DaytonStrong Buckeye...", 
        "Cape Coral, FL USA", "Indiana, USA", "Michigan", "chicago", 
        "Montclair, NJ", "Carbondale, IL", "Fontana, CA", "Prim, Nevada", "Long Beach, NY",
        "Tulare, CA", "Alabama", "parts unknown, USA", "Monticello, MN", "Doylestown, PA",
        "Iowa City, IA", "Port St Lucie, FL", "King County, Washington", "PA, USA",
        "Fate, TX", "stolen land - Portland, OR", "Nowheresville", "Lexington, KY", 
        "missouri, USA", "Los Angeleno in Palm Beach FL",  "Huge Nutbag, Montana", 
        "Blacksburg, VA", "Monterey, CA.", "The US", "#Amerikkka", "North America",
        "i just be tweetin fr", "Los Angeles", "Kansas", "Ko Olina, HI", "America, Planet, Earth",
        "Citizen of the world", "Bklyn”, “Kirkland, WA",
        "#WorldNews @the411now", "New Hampshire", "Chinuk Illahee",  "East New York, Brooklyn",
        "Occupied Duwamish Territory",  "Hamilton Alabama", "Plattsburgh, NY", "The United States of Siliness", "NY / Philadelphia",
        "NY/NJ", "NYC Greatest City in the World", "NYC to DMV", "NYC via Seattle via Liberia","NYC-ATL-LA-MIA", "NYC, Baltimore & Del beaches", 
        "NYC, NC, FL, DC, Africa, India", "NYC(ish) || LA(ish)", "Oakland", "Oakland to Vegas", "Oakland, CA, USA, Earth, SOL", 
        "Oakland, California","Obama is a BETA who LOVES anal","Ocala, FL", "Occupied Anishinaabe Territory", "occupied Duwamish Land SEA", "occupied Duwamish territory", 
        "Occupied Duwamish Territory", "Occupied Fascist Territory USA", "Occupied Indigenous Land",  "Occupied land of Sioux/Ojibwa", "Oceanside, CA", 
        "Ocoee, FL", "Florence, SC", "Calabasas", "North America", "North Augusta, SC", "North Chicago, IL",
        "North County, California", "North Dakota, USA", "north of Batiquitos Lagoon", "North Puyallup, WA", 
        "North San Diego County Coastal", " New York  / P.A Area "," Oregon"," UPPER PENNINSULA"," Washington",
        "#Amerikkka ","#BidenHarris2020","#BidenHarris2020 ","#CrimsonTideFan Alabama, USA",
        "#DaytonStrong Buckeye... ","#DMV","#Doorstown LA VENICE BEACH USA","#FortBragg/FayettevilleNC",
        "~not exactly a Trump supporter","20th Arrondissement","2354 Ocean Blvd.","2444 Durant Berkeley CA 94704",
        "315 14th St NW Atlanta, GA","360 East 161st St, Bronx NY","40.505621,-74.484288","50 states, DC, & Puerto Rico",
        "50.6071° S, 165.9726° E","632 West 39th Street, KCMO ","729 W 181st Street, New York,","757 Virginia Beach, VA",
        "ACLU.org","Acworth, GA","Adairsville, GA","Adams, MA",
        "Agoura Hills CA, Pittsburgh PA","Alabama ","Alabama, USA","Albany, California",
        "Albany, New York","Albany, NY","Albuquerque, New Mexico, USA","Albuquerque, NM",
        "Allentown, PA","Aloha from Wakanda","America","America the Beautiful",
        "America, F Yeah!","Amherst","An awesomely BLUE state.","Anaheim, Ca.",
        "Anarchist Jurisdiction","Anarchist Jurisdiction ","Anarchist Jurisdiction Seattle","Anarchist Jurisdiction, USA",
        "Anderson, SC","Ann Arbor, Michigan","Annapolis, MD","Antelope Isle, Lake Bonneville","anti commie",
        "Antifa HQ","Antifa Soup Factory","Any Where, USA","Anywhere, USA","Appleton, WI/Birmingham, AL",
        "Arecibo, Puerto Rico","Arizona","Arkansas, USA","Arlington, Texas",
        "Arlington, TX","Armchair Activism America","Athens, GA","Athens, Jawja",
        "Atlanta GA","Atlanta, GA, International","Atlanta, Ga.","ATLANTIC CITY",
        "Audubon, NJ","Aurora, Illinois","Austin, Texas","Austin, TX ",
        "Austin, TX, United States","ᴀmᴇʀiᴄa ᴏғ statᴇs ᴜɴitᴇᴅ","Albuquerque, NM","Alexandria, VA",
        "Logan, UT","Long Beach, CA","Long Island, NY","Los Altos Hills, CA","Los Angeleno in Palm Beach FL ",
        "los angeles","Los Angeles ","Los Angeles / Boston","Los Angeles / Global","Los Angeles | New Hampshire",
        "Los Angeles and environs","Los Angeles and Pasadena, CA","Los Angeles area","Los Angeles CA",
        "Los Angeles via Brooklyn, NY","Los Angeles, CA & Houston, TX","Los Angeles, CA.","Los Angeles, California",
        "Los Angeles, California.  ","Los Angeles/ Worldwide","Louisiana, New France","Louisiana, USA",
        "Louisville, Kentucky","Louisville, KY/DC","Loveland, Ohio","Lower East Side/Chinatown, NYC",
        "Lubbock, TX","Las Vegas ","Las Vegas NV","Las Vegas, Nevada",
        "Las Vegas, Nevada USA","Las Vegas, NV.","Washington D.C.","Washington DC - New York City",
        "Washington DC & California","Washington Heights, NY","Washington State","Washington, DC - Zim - Dom Rep",
        "Washington, DC / New York","Washington, DC and Los Angeles","Washington, DC and Oakland, CA","Washington, DC Area",
        "Washington, DC in the USA","Washington, DC-area","Washington, DC, USA","Walt Disney World, FL",
        "Baltimore","Baltimore, MD ","Baltimore, Md.","Baltimore, Tokyo, Los Angeles",
        'Banning, CA','Basehor, KS','BassAdelphia, PA','Bastrop, TX',
        'Bay Area, CA','Bay Area.','Baytown, TX','Beaumont, Texas, USA','Believeland',
        'Believland','Bellevue, WA','Bellingham, WA','Bellybutton of California', "CO, USA",
        'Bensalem, PA','Berkeley, California','Between Ga. and Mi.','Beverly Hills',
        'Beverly Hills, CA','BIDENS BASE(MENT ALLY-ILL)','Bklyn ','Black Lives Matter, USA',
        'Blackwood, NJ','Bloomington, IN','Bloomington, MN','Blue Earth, MN','Blue Girl, Red State. Game On.',
        'Blue Ocean to Blue Ridge. ',"Boaz, AL","Boston","Boston ",'Boston-ish','Boston, Massachusetts',
        'Boulder, CO','Boynton Beach, FL','Bradenton, FL','Bradenton, Florida','Brattleboro, VT',
        "Brookline, MA","Brooklyn","Brooklyn :: NY","Brooklyn NY ","Brooklyn's in da house",
        "Buffalo","BumfuckEqypt, NV - Area 51","Burlington, VT","Burnsville, MN","Bushwick",
        "BXNY","Byron, NY","CA25 high desert","Caifornia","Californ I A","California ",
        "California (via Barcelona) ","California & United Kingdom ","California dreamin'",
        "California, USA ","california,usa","Calumet City, IL","Camarillo CA.","Cambridge, MA",
        "Capital district ","Capitol Hill","Carlsbad, CA","Carrollton, TX","Cave Creek AZ",
        "Cave Creek, AZ Orig From STL, ","Central and South Jersey","Central California",
        "Central Jersey Shore ","Chaldean Quarter, Old El Cajon","Champaign, IL",
        "Chapel Hill, NC","Charleston, SC","Charlotte","Charlottesville, VA","Chatham, NJ, USA",
        "CHAZ","Chelan, WA","Chelsea, NYC","Chesapeake Bay","Chi, IL","Chicago ","Chicago and beyond",
        "Chicago, IL. ","Chicago, Illinois","Chicago,IL","CHIQAGO ILLINOIS",
        "CHOP","Cincinnati","Cincinnati, OH","Cincinnati, Ohio","Clearwater Florida USA",
        "Clearwater, FL","Clermont, FL","cleveland","Cleveland, OH, USA","Cleveland, Ohio","Clover, SC",
        'detroit','Detroit','Detroit bred - Flint accepted','Detroit Resurgit Cinerbus',
        'Detroit, Michigan','Minnesota','Washington, DC','Los Angeles, CA',
        'Santa Monica, CA','Lawton, OK','USA','San Diego, CA','Tigard, OR',
        'East New York, Brooklyn','The United States of Siliness','Florence, SC',
        'Portland, OR','Los Angeles, CA','Doral, FL','Seattle, WA','chicago',
        'Los Angeles','New York, USA','Palm Bay, FL','PA, USA','New York, USA',
        'Portland, OR','Blue Georgia in the New South','New York City & Nationwide',
        'Standing Rock','United States','Dallas, TX','Louisville, KY','Denver, CO',
        'Louisville, Kentucky','St. Petersburg FL','Atlanta, GA','The Internet & NYC',
        'Brooklyn, NY','Louisville, KY','Baltimore, MD','Washington, DC','United States',
        'Iowa, USA','Wake Forest, NC','USA','Atlanta, GA','Portland, Oregon','United States',
        'Los Angeles','Missouri, USA','New York, USA','New York, NY','Bloomington, IN',
        'Louisville, KY','NJ','New York, NY','New York, USA','Los Angeles, California',
        'United States','Washington, DC','Chicago and beyond','New York','Florida ',
        'Cleveland, OH','Los Angeles, CA','Washington, DC and Oakland, CA','Washington, DC',
        'Portland, Oregon','Toronto, Ontario','Sarasota, Florida USA','United States',
        'US','Cleveland, OH','DMV','Washington, DC','New York City','Chicago, IL','Kickapoo land',
        'Truth or Consequences, NM','Washington, DC','New York City','Austin, TX',
        'USA','Los Angeles, CA','Washington, DC and Los Angeles','Los Angeles, CA','Boston, Massachusetts',
        'New York, NY','Washington, DC','Portland, OR', 'South Carolina, USA', 'Portland, OR',
        'Seattle, WA', "50 states, DC, & Puerto Rico", 'Santa Barbara, CA', 'Boston, MA',
        'Washington, DC', 'Washington, D.C.','Houston, TX','Los Angeles, CA','L.A., Calif.',
        'San Antonio, TX', 'Houston, TX & Washington, DC','Tallahassee, FL','Seattle ','Washington DC',
        'Pacific Northwest', 'Rose City','California (via Barcelona) ','So-Called North America', 
        'The Upside Down  ','757','PlanetEarth','GPG: 0x5B9EE576F9EF31BA', 'Everywhere','Fiddling with your mind',
        'CHO | DMV | BOS','#TheResistance','ON YOUR TV/RADIO/PODCAST','worldwide', 'Home','🌐 ᗯOᖇᒪᗪᗯIᗪE 🌐 ᗩᗰᑭᒪIᖴIEᗪ 🌐',
        'Wearing a Mask', 'he/him/they/them ','🔥🌎🔥','#EarthJustice')
for(z in usa){
  location<-replace(location, location==z, 'USA')
}
#############
##UK##
#############
uk<- c('Across the borough of Kingston','Glasgow, Scotland', "West Midlands, England", "Edinburgh, Scotland", "England ", 
       "Derby, England", "Northampton, England", "Oxford, England", "manchester",  
  'London, UK', 'Wales','United Kingdom','Manchester, England','Liverpool City Council',
       'United Kingdom',"England, United Kingdom", "London, England", "London","England", 
       "Cardiff, Wales", "Cambridge, England", "South West, England", 'England',
       "Brighton, England", "The Burgh!", "Bristol, England", "Birmingham, UK", "North East of England", 
       "North West, England", "Nottingham, England", ' England',' United Kingdom','66 The Cut, London, SE1 8LZ','Aberdeen, Scotland',
       'Aberdeenshire','Angelcynn.  Gaels. Breteyne. ','Angus','LONDON ',
       'London (NW3)','London / Los Angeles','London & Cirencester via Wales',
       'London & Quintana Roo, Mexico','London South Bank University','London Town',
       'London UK','London, England via Minnesota ','London, England, UK',
       'London, EU (obvs) ','London, FLORIDA,Trinidad ','London, New York, Los Angeles','London, Paris, Rio, Sao Paulo',
       'London, Views my own','Liverpool','Liverpool City Council','Liverpool, England',
       'Liverpool, England, UK','Leicester','Leicester UK','Leicester, England','Leicestershire and Rutland',
       'Wales, United Kingdom','Walsall UK. ','Warwickshire ','Watford','Watford, England',
       'Bedford, England','Bearwood, West Midlands, UK','BATH','Bath, England','Bassaleg, Newport','balham, london. ',
       'Barnet','Barnet House Whetstone N20 0EJ','Birmingham','Birmingham City','Birmingham, England',
       'Birmingham/Leicester, UK','Blackwood, Wales ','Bournemouth, England','Bradford & London, UK','Bradford uk',
       'Bristol','bristol uk','Bristol UK','Bristol, UK','Britain','Britannia Village Hall, London',
       'Brockley, SE4, London','Cambridgeshire, England','Cardiff','Cardiff | Caerdydd','Cheshire',
       'City of London, London','Clarence Rd, Cardiff. CF10 5FB','Clydebank, Scotland','Cockney',
       'uk','UK ','UK - London, Cumbria, Scotland','UK & Worldwide','UK and Ireland','UK, NYC, Dubai, APAC, India',"UK/USA",
       'Bristol UK','Streatham, London','Britain','20137622','London / Los Angeles','Cardiff | Caerdydd',
       'lead software developer 📍LDN')

for(z in uk){
  location<-replace(location, location==z, 'UK')
}
#########
###Canada###
#########
canada<- c("Toronto, Canada","Toronto, Ontario, Canada","Ontario, Canada","Vancouver, BC",
           "Toronto, Ontario", "Toronto", "Ottawa, Ontario", "Montréal, Québec", "Fergus, ON",'Canada',
           '#Autowa','Alberta','Alberta/Saskatchewan','Caledon, Ontario','Calgary','Calgary, Alberta',
           'Calgary, Alberta, Canada','canada','Canada ','Canada , Ottawa','Canada and the world',
           'Saskatchewan, Canada', 'Canada ', 'Shawnigan Lake BC','Calgary, Alberta')

for(z in canada){
  location<-replace(location, location==z, 'Canada')
}
###########

uscanada<- c('USA', 'Canada', 'Winnipeg')
for(z in uscanada){
  location<-replace(location, location==z, 'North America')
}

uscanada<- c('Don’t pay attention to this✨✨')
for(z in uscanada){
  location<-replace(location, location==z, 'North America')
}

uscanada<- c('San Francisco, CA🇺🇲  ')
for(z in uscanada){
  location<-replace(location, location==z, 'North America')
}



europe<- c('United kingdom','Edmonton, North London ','East London','Ealing, London','Durham','Dublin & Belfast','Dorchester', '#Azerbaijan #Sweden #Poland', 'De Cymru', 'Denmark',
  'Bangor, Northern Ireland','Join the movement','Bonn, Germany / Deutschland','Puiseux-en-France, France', 'France', 
           "Berlin, Germany", "Brussels, Belgium", "Germany", "Ireland", "Spain",'Alesia ','Almere, Nederland',
           'Armenia','Maastricht, The Netherlands','Brussels', 'Bruxelles','Berlin','Belgium','Bari',
           'Barcelona, Spain','Vilnius','Oslo, Norway','Maastricht, The Netherlands','Madrid, Comunidad de Madrid')
for(z in europe){
  location<-replace(location, location==z, 'Europe')
}
australia<- c('Dubai, United Arab Emirates','Dhaka, Bangladesh','Dehradun India',"Mumbai, India", "Ahmedabad",'Al Khobar, Saudi Arabia', 'Ankara, T√ºrkiye',
              "Bangkok, Thailand", 'Brisbane, Queensland', 'Bhopal, India',
              "Victoria, Australia","Melbourne, Victoria","Brisbane", 'New Zealand', 'Australia', "Queensland, Australia",'The land of Oz',
              "india",'India','Doha, Qatar', 'Karauli, Rajasthan & Mumbai','Norramby, Island of Sodor',
              'Bengaluru','Asia', 'Wellington, New Zealand')
for(z in  australia){
  location<-replace(location, location==z, 'Australia & NZ & Asia')
}

africa<- c('Nigeria','ghana','Kenya','Lagos, Nigeria','Nairobi','Nairobi, Kenya','South Africa',
           'Addis Ababa, Ethiopia','Cameroon','Lagos, Nigeria.', 'Uganda, Nigeria', 'Kenya/Somalia',
           'Khubvi Village', 'Lusaka Zambia', 'Mombasa, KENYA.')
for(z in africa){
  location<-replace(location, location==z, 'Africa')
}


ukeurope<- c('UK','Europe')
for(z in ukeurope){
  location<-replace(location, location==z, 'UK & Europe')
}

na_c<- c("Worldwide", "Everywhere", "", " ", "Earth", "Planet Earth","Global", "\U0001f1fa\U0001f1f8", 'NA')
for(z in na_c){
  location<-replace(location, location==z, 'NA or Other')
}

loc_coun<- c("North America", "Australia & NZ & Asia", "UK & Europe", "Africa")

###EXPLORING##
#######
loc2 <- location[!(location %in% loc_coun)]
loc4 <- sort(loc2, decreasing = FALSE)
loc4 <- unique(loc4)
loc4 <- data.frame(loc4)
write.csv(loc4,'top20_f.csv', row.names = FALSE)
#######
location[(location %in% loc_coun) == FALSE] <- NA
location[is.na(location)]<- "NA or Other"
df_netnodes$location[(df_netnodes$location %in% loc_coun) == FALSE] <- NA
df_netnodes$location[is.na(df_netnodes$location)]<- "NA or Other"
summary(df_netnodes$location)
summary(location)
unique(location)
#Create Network with Statenet
tweets_int <- network (edge_list_u, directed = T, matrix.type = "edgelist")

network::set.vertex.attribute (tweets_int, 'Location', as.character(location))

network::set.vertex.attribute (tweets_int, 'Followers_count', as.numeric(fo_count))
network::set.vertex.attribute (tweets_int, 'Friends_count', as.numeric(fr_count))
network::set.vertex.attribute (tweets_int, 'Listed_count', as.numeric(l_count))
network::set.vertex.attribute (tweets_int, 'Statuses_count', as.numeric(s_count))
network::set.vertex.attribute (tweets_int, 'Favorites_count', as.numeric(fv_count))

network::set.vertex.attribute (tweets_int, 'Followers_Category', as.character(unlist(fo_count_1)))
network::set.vertex.attribute (tweets_int, 'Friends_Category', as.character(unlist(fr_count_1)))
network::set.vertex.attribute (tweets_int, 'Listed_Category', as.character(unlist(l_count_1)))
network::set.vertex.attribute (tweets_int, 'Statuses_Category', as.character(unlist(s_count_1)))
network::set.vertex.attribute (tweets_int, 'Favorites', as.character(unlist(fv_count_1)))


network::set.vertex.attribute (tweets_int, 'Acc_Before_GFloyd_Death', as.logical(gf_date))
network::set.vertex.attribute (tweets_int, 'Verified', as.logical(verified))

network::list.vertex.attributes(tweets_int)
#########
##Variable Destruction Station####
rm()
#####
library(igraph)
library(intergraph)
tweet_graph <- asIgraph(tweets_int)
clique.number(tweet_graph)
largest.cliques(tweet_graph)
graph.density(tweet_graph)
length(cliques(tweet_graph,min=3))
coreness <- graph.coreness(tweet_graph)
table(coreness)
maxCoreness <- max(coreness)
maxCoreness

Vname <- get.vertex.attribute(tweet_graph,name='vertex.names',
                              index=V(tweet_graph))
V(tweet_graph)$name <- Vname
V(tweet_graph)$color <- coreness + 1
op <- par(mar = rep(0, 4))
plot(tweet_graph, vertex.size = 4, edge.width = 0.5, edge.arrow.width = 0.5,
     edge.arrow.size = 0.5,vertex.label=NA)
par(op)
colors <- rainbow(maxCoreness)
op <- par(mar = rep(0, 4))
plot(tweet_graph,
     vertex.color=colors[coreness], vertex.size = 3, vertex.label=NA)
par(op)

tweet_graph_5 <- induced.subgraph(tweet_graph,
                             vids=which(coreness > 4))
lay <- layout.fruchterman.reingold(tweet_graph)

plot(tweet_graph_5,  vertex.size = 4, edge.width = 0.5, edge.arrow.width = 0.5,
     edge.arrow.size = 0.5,vertex.label=NA, layout=lay[which(coreness > 4),], vertex.label="",
     main="k-cores 5")
cw <- cluster_walktrap(tweet_graph)
membership(cw)
modularity(cw)


grp_num <- as.numeric(factor(V(tweet_graph)$Location))
Location_grp_num <- modularity(tweet_graph, grp_num)

Verified_grp_num <- as.numeric(factor(V(tweet_graph)$Verified))
Verified_grp_num <-modularity(tweet_graph, Verified_grp_num)

Followers_grp_num <- as.numeric(factor(V(tweet_graph)$Followers_count))
Followers_grp_num <-modularity(tweet_graph, Followers_grp_num)

Friends_grp_num <- as.numeric(factor(V(tweet_graph)$Friends_count))
Friends_grp_num <-modularity(tweet_graph, Friends_grp_num)

Listed_grp_num <- as.numeric(factor(V(tweet_graph)$Listed_count))
Listed_grp_num <-modularity(tweet_graph, Listed_grp_num)

Statuses_grp_num <- as.numeric(factor(V(tweet_graph)$Statuses_count))
Statuses_grp_num <-modularity(tweet_graph, Statuses_grp_num)

Fav_grp_num <- as.numeric(factor(V(tweet_graph)$Favorites_count))
Fav_grp_num <-modularity(tweet_graph, Fav_grp_num)

Followersc_grp_num <- as.numeric(factor(V(tweet_graph)$Followers_Category))
Followersc_grp_num <-modularity(tweet_graph, Followersc_grp_num)

Friendsc_grp_num <- as.numeric(factor(V(tweet_graph)$Friends_Category))
Friendsc_grp_num <- modularity(tweet_graph, Friendsc_grp_num)

Listedc_grp_num <- as.numeric(factor(V(tweet_graph)$Listed_Category))
Listedc_grp_num <-modularity(tweet_graph, Listedc_grp_num)

Statusesc_grp_num <- as.numeric(factor(V(tweet_graph)$Statuses_Category))
Statusesc_grp_num <-modularity(tweet_graph, Statusesc_grp_num)

Favc_grp_num <- as.numeric(factor(V(tweet_graph)$Favorites))
Favc_grp_num <-modularity(tweet_graph, Favc_grp_num)

Date_grp_num <- as.numeric(factor(V(tweet_graph)$Acc_Before_GFloyd_Death))
Date_grp_num <- modularity(tweet_graph, Date_grp_num)

mod_net_var <- c()
Modularity_Net <- data.frame(Location_grp_num, Verified_grp_num, Date_grp_num,
                             Followers_grp_num, Followersc_grp_num,
                             Friends_grp_num, Friendsc_grp_num,
                             Listed_grp_num, Listedc_grp_num,
                             Statuses_grp_num, Statusesc_grp_num,
                             Fav_grp_num, Favc_grp_num)
detach("package:igraph", unload=TRUE)
#############
library(statnet)
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, node.color = "#000080", node.alpha = 0.75,
        label = FALSE,node.size = 2) +
ggtitle("Basic Sociogram of the #BlackLivesMatter Community on Twitter") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))
network.density (tweets_int)
summary (tweets_int)
largest_tweetnet <- component.largest(tweets_int, result = "graph")
?component.largest()
diameter <- geodist(largest_tweetnet)
max (diameter$gdist)
ggnet2(largest_tweetnet, label = TRUE, label.size = 3, arrow.size = 3, arrow.gap = 0.03, 
       mode = "fruchtermanreingold", node.size = 8) +
  ggtitle("Largest Component of the Network") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))
largest_tweetnet <- component.largest(tweets_int, connected='weak', result = "graph")
diameter <- geodist(largest_tweetnet)
max (diameter$gdist)
ggnet2(largest_tweetnet, label = FALSE, label.size = 3, arrow.size = 3, arrow.gap = 0.03, 
       mode = "fruchtermanreingold", node.size = 8) +
  ggtitle("Largest Component of the Network") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))
largest_tweetnet <- component.largest(tweets_int, result = "graph")
members_lc <- colnames(largest_tweetnet)
screen_names <- df1$screen_name[df1$user_id %in% members_lc]
unique(screen_names)

degdist <- degreedist (tweets_int)
degdist

components(tweets_int)
components(tweets_int, connected="weak")
gtrans (tweets_int, use.adjacency = F, "weak")
mutuality (tweets_int)
grecip (tweets_int, measure = "edgewise")

unique(gf_date)
bol_colours <- c('#a6cee3', '#e31a1c')
names(bol_colours) <-  (unique (gf_date))

l_colours <- c('#edea2b',  '#b2df8a',  '#fb9a99', '#1f78b4', '#bebada',  '#e31a1c', '#33a02c')
names(l_colours) <-  (unique(location))

colours <- c('#a6cee3',  '#e31a1c',  '#b2df8a', '#33a02c',  '#fb9a99',  '#1f78b4',  '#fdbf6f', '#ff7f00') 
             #'#cab2d6', '#6a3d9a', '#ffff99', '#b15928', '#8dd3c7', '#ffffb3', '#bebada', '#fb8072', '#80b1d3', '#fdb462', '#b3de69', '#fccde5', '#d9d9d9')
names(colours) <-  (unique(fo_count_1))
#Calculate Degrees

?degree()
indeg <- degree (tweets_int, cmode= 'indegree')
network::set.vertex.attribute (tweets_int, 'Indegree', as.numeric(indeg))
outdeg <- degree (tweets_int, cmode = 'outdegree')
network::set.vertex.attribute (tweets_int, 'Outdegree', as.numeric(outdeg))
max(indeg)
which.max(indeg)
df_netnodes[2173,] #Lebron James
unique(df1$screen_name[df1$user_id=='23083404'])
#1001 VERIFIED 
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Verified', legend.size = 12,
        color.palette = bol_colours, node.size = indeg/5, 
        max_size = 12) + guides (size = F)

#1002 Before or after Floyd's death, TRUE if account created before Floyd's death
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Acc_Before_GFloyd_Death', legend.size = 12, node.alpha = 0.5,
        color.palette = bol_colours, node.size = indeg/5,
        max_size = 12) + guides (size = F)

#1002.5 Before or after Floyd's death, Location, Indegree
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE,
        node.shape = 'Acc_Before_GFloyd_Death', 
        node.color = 'Location', legend.size = 12, 
        color.palette = l_colours, node.size = indeg/5,
        max_size = 12) + guides (size = F)

#1003 node size for listed, color for followers
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Followers_Category', legend.size = 12,
        color.palette = colours, node.size = "Listed_count", 
        max_size = 12) + guides (size = F)

#1004 node size for followers, color for statuses
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Statuses_Category', legend.size = 12,
        color.palette = colours, node.size = "Followers_count", 
        max_size = 12) + guides (size = F)

#1005 node size for indegree, color for followers
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Followers_Category', legend.size = 12,
        color.palette = colours, node.size = indeg/5, 
        max_size = 12) + guides (size = F)

#1006 node size for indegree, color for friends
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Friends_Category', legend.size = 12,
        color.palette = colours, node.size = indeg/5, 
        max_size = 12) + guides (size = F)

#1007 node size for indegree, color for favorite
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Favorites_Category', legend.size = 12,
        color.palette = colours, node.size = indeg/5, 
        max_size = 12) + guides (size = F)

#1008 node size for indegree, color for statuses
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Statuses_Category', legend.size = 12,
        color.palette = colours, node.size = indeg/5, 
        max_size = 12) + guides (size = F)

#1009 node size for indegree, color for listed
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Listed_Category', legend.size = 12,
        color.palette = colours, node.size = indeg/5, 
        max_size = 12) + guides (size = F)

#1010 node size for indegree, color for location
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Location', legend.size = 6, node.alpha = 0.75,
        color.palette = l_colours, node.size = indeg/5, 
        max_size = 12) + guides (size = F)

tweet_int_ <- get.inducedSubgraph(tweets_int,
                               which((tweets_int %v% "Indegree" > 1) | (tweets_int %v% "Outdegree" > 1)))

ggnet2 (tweet_int_, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Location', legend.size = 6, node.alpha = 0.75,
        color.palette = l_colours, node.size = 'Indegree', 
        max_size = 12) + guides (size = F)+
  ggtitle(" Network (Indegree>1 OR Outdegree > 1)") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))

largest_tweetnet_i <- component.largest(tweet_int_, result = "graph")

ggnet2(largest_tweetnet_i, label = TRUE, label.size = 3, arrow.size = 3, arrow.gap = 0.03, 
       mode = "fruchtermanreingold", node.size = 8) +
  ggtitle("Largest Component of the Network (Indegree>1 OR Outdegree > 1)") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))
members_lc_i <- colnames(largest_tweetnet_i)
screen_names <- df1$screen_name[df1$user_id %in% members_lc_i]
unique(screen_names)

#Basic Descriptions
summary.network(tweets_int, print.adj=F)
#Centrality measurements
clos <- round (sna::closeness (largest_tweetnet,cmode="directed"), 2)

#1011 largest component closeness
ggnet2 (largest_tweetnet, label = TRUE, node.size = clos*35, 
        max_size = 12, label.size = 3, node.color = "#7fcdbb") + guides (size = F)+
  ggtitle("Largest Component of the Network") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))

betw<- round (betweenness (tweets_int, cmode = "directed", rescale = T),6)
max (betw)
which.max (betw)
df_netnodes[1672,] #Black Lives Matter
unique(df1$screen_name[df1$user_id=='1604931252'])
#1012 betweeness
ggnet2 (tweets_int, label = FALSE, node.color = 'Location',  color.palette = l_colours, 
        node.alpha = 0.8, node.size = (betw*200)) + guides (size = F)+
  ggtitle("Node size adjusted for Betweeness score") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))

evcenter <-round (evcent (tweets_int, gmode = "digraph"),2)
max (evcenter)
which.max (evcenter)
df_netnodes[1883,] #3loudosboys
unique(df1$screen_name[df1$user_id=='1909038608'])
#1013 Eigenvector centrality
ggnet2 (tweets_int, label = FALSE, node.color = 'Location',  color.palette = l_colours, 
        node.alpha = 0.8, node.size = evcenter*30) + guides (size = F)+
  ggtitle("Node size adjusted for Eigenvector score") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))

#Centrality table
tweetnet_cent <- data.frame (
  indegree = indeg,
  outdegree = outdeg,
  betweenness = betw,
  eigenvector = evcenter, 
  id = tweets_int %v% "vertex.names",
  location = tweets_int %v% "Location")

head(tweetnet_cent)

clos <- data.frame (clos = clos, id = rownames (largest_tweetnet))
tweetnet_cent <- merge (tweetnet_cent, clos, by = "id",  all = T) 

tweetnet_cent <- tweetnet_cent [order (-tweetnet_cent$betweenness),]
tweetnet_cent
top20<- (head (tweetnet_cent, 20))
top20
top20_tweeps <- df_netnodes[df_netnodes$user_id %in% top20$id,] 

screennames_ <- unique(df1$screen_name[df1$user_id %in% top20$id] )
write.csv(top20_tweeps,'top20_f.csv', row.names = FALSE)

cent.deg<- round (centralization (tweets_int, FUN = degree),2)
cent.clos<- round (centralization (tweets_int, FUN = closeness),2)
cent.betw<- round (centralization (tweets_int, FUN = betweenness),2)
cent.eig<- round (centralization (tweets_int, FUN = evcent),2)

print (c (cent.deg, cent.clos, cent.betw, cent.eig))

cut <- cutpoints (tweets_int, mode = "digraph", return.indicator = T)
length (cut [cut==T])

cutpoints_tweet <-data.frame (cutpoint = cut [cut==T],  name = df_netnodes$user_id[cut==T], 
                            location = df_netnodes$location[cut==T]) 
top20$id %in% cutpoints_tweet$name
cut_snames <- df1$screen_name[df1$user_id %in% cutpoints_tweet$name]
unique(cut_snames)
set.vertex.attribute (tweets_int, 'cut', as.logical (cut)) 
tweetscut  <- tweets_int %s% which(tweets_int %v% "cut" == F) 

#1014 Without cut points
ggnet2 (tweetscut, label = F, node.color = 'Location',  color.palette = l_colours,
        node.alpha = 0.5, node.size = "Indegree", max_size = 12) + guides (size = F)+
  ggtitle("Network without cut-points") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))


tweetnet_cent <- data.frame (
  indegree = indeg,
  outdegree = outdeg,
  betweenness = betw,
  eigenvector = evcenter, 
  id = tweets_int %v% "vertex.names",
  Cutpoint = tweets_int %v% "cut")

head(tweetnet_cent)

clos <- data.frame (clos = clos, id = rownames (largest_tweetnet))
tweetnet_cent <- merge (tweetnet_cent, clos, by = "id",  all = T) 

tweetnet_cent <- tweetnet_cent [order (-tweetnet_cent$betweenness),]
tweetnet_cent
top20<- (head (tweetnet_cent, 20))
top20


tweetnet_brokerage <- brokerage (tweets_int, location)
str (tweetnet_brokerage)
tweetnet_brokerage$raw.gli
head(tweetnet_brokerage$raw.nli)

brokerage_rawscores <- as.data.frame(tweetnet_brokerage$raw.nli) 
brokerage_rawscores  <- brokerage_rawscores [order (-brokerage_rawscores$t),]
brok20 <- head(brokerage_rawscores, 20)
top20_tweeps_brok <- df1$screen_name[df1$user_id %in% rownames(brok20)] 
unique(top20_tweeps_brok)
top20$id %in% rownames (head (brokerage_rawscores, 20))

broknames <- df1$screen_name[df1$user_id %in% rownames(brok20)]
unique(broknames)

components(tweets_int)

##Sentiment Analysis #####
# library(tm)
# text_corpus <- Corpus(VectorSource(top20_tweeps$text))
# text_corpus <- tm_map(text_corpus, tolower)
# text_corpus <- tm_map(text_corpus, removeWords, 
#                       c("blacklivesmatter", "#blacklivesmatter", "blm", "black lives matter"))
# text_corpus <- tm_map(text_corpus, removeWords, 
#                       stopwords("english"))
# text_corpus <- tm_map(text_corpus, removePunctuation)
# text_df <- data.frame(text_clean = get("content", text_corpus), 
#                       stringsAsFactors = FALSE)
# blm_df <- cbind.data.frame(top20_tweeps, text_df)
library(SentimentAnalysis)
# 
# blm_sentiment <- analyzeSentiment(blm_df$text_clean)
# blm_sentiment <- dplyr::select(blm_sentiment, 
#                                  SentimentGI, SentimentHE,
#                                  SentimentLM, SentimentQDAP, 
#                                  WordCount)
# blm_sentiment <- dplyr::mutate(blm_sentiment, 
#                                  mean_sentiment = rowMeans(blm_sentiment[,-5]))
# blm_sentiment <- dplyr::select(blm_sentiment, 
#                                  WordCount, 
#                                  mean_sentiment)
# blm_df <- cbind.data.frame(blm_df, blm_sentiment)
# blm_negative <- filter(blm_df, mean_sentiment < 0)
# 
# nrow(blm_negative)
# 
# blm_positive<- filter(blm_df, mean_sentiment > 0)
# 
# nrow(blm_positive)
# 
library(quanteda)
# blm_tokenized_nlist <- tokens(blm_negative$text_clean)
# blm_tokenized_plist <- tokens(blm_positive$text_clean)
# 
# blm_ndfm <- dfm(blm_tokenized_nlist)
# blm_pdfm <- dfm(blm_tokenized_plist)
# 
# word_sums_n <- colSums(blm_ndfm)
# word_sums_p <- colSums(blm_pdfm)
# length(word_sums_n)
# length(word_sums_p)
# 
# freq_data_n <- data.frame(word = names(word_sums_n), 
#                         freq = word_sums_n, 
#                         row.names = NULL,
#                         stringsAsFactors = FALSE)
# 
# freq_data_p <- data.frame(word = names(word_sums_p), 
#                           freq = word_sums_p, 
#                           row.names = NULL,
#                           stringsAsFactors = FALSE)
# sorted_freq_ndata <- freq_data_n[order(freq_data_n$freq, decreasing = TRUE), ]
# sorted_freq_pdata <- freq_data_p[order(freq_data_p$freq, decreasing = TRUE), ]
#write.csv(sorted_freq_ndata,'frequency_p.csv', row.names = FALSE)
#write.csv(sorted_freq_pdata,'frequency_n.csv', row.names = FALSE)
#write.csv(blm_negative,'top_negative.csv', row.names = FALSE)
#write.csv(blm_positive,'top_positive.csv', row.names = FALSE)
library(tm)
blm_negative <- read.csv('top_negative.csv', header = TRUE)
blm_positive <- read.csv('top_positive.csv', header = TRUE)
blm_corpus_tmn <- Corpus(VectorSource(blm_negative[,91]))
blm_corpus_tmp <- Corpus(VectorSource(blm_positive[,91]))
blm_dtmn <- DocumentTermMatrix(blm_corpus_tmn)
blm_dtmp<- DocumentTermMatrix(blm_corpus_tmp)

blm_dtmn <- removeSparseTerms(blm_dtmn, 0.98)
blm_dtmp <- removeSparseTerms(blm_dtmp, 0.98)
library(devtools)
devtools::install_github("hfgolino/EGA")
library(EGAnet)
blm_df_ncluster <- as.data.frame(as.matrix(blm_dtmn))
blm_df_pcluster <- as.data.frame(as.matrix(blm_dtmp))
?EGA()
ega_blm_n <- EGA(blm_df_ncluster)
ega_blm_p<- EGA(blm_df_pcluster)
ega_blm_n$dim.variables
ega_blm_p$dim.variables