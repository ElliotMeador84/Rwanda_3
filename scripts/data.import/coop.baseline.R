dir.create('scripts/david.correspondence/')
file.create('scripts/david.correspondence/coop_vis.R')
file.edit('scripts/david.correspondence/coop_vis.R')


# Data import -------------------------------------------------------------
library(tidyverse)
library(igraph)
library(visNetwork)
## import data from SPSS file
setwd('/R/')
df_base.line <- haven::read_spss('Rwanda_3/Coop_and_Union_Baseline/Coops DataBaseline2017_18SocialCapitalStudy8_19_17.sav')
setwd('/R/Rwanda_3/')
dir.create('data/')
save(df_base.line,file = 'data/df_base.line.RData')
# Base.line clean ---------------------------------------------------------
#### load helping functions
source('functions/dplyr_helpers.R')
source('functions/functions.general.R')
#clean=====

# look(df_base.line,35)
## clean some names
tango <- df_base.line %>% 
    select(coop = Qi.Coopname,
           coop_type = Qii.Cooptype,
           first_name = Qiii.a.Respondent_Firstname,
           second_name = Qiii.b.Respondent_Lastname,
           member = Qiv.a.Member,
           member.years = Qiv.b.Membership_years,
           leadership = Qiv.c.Member_in_leadership,
           leadership.years = Qiv.d.Member_in_leadership_years,
           leadership.position = Qiv.c.Position,
           age = Qv.Age,
           gender= Qvi.Sex,
           person1 = Q3.a.Firstperson,
           person2 = Q3.b.Secondperson,
           person3 = Q3.c.thirdperson,
           coop_satified = CoopSatsfScale_4Items) %>% 
    mutate(gender = ifelse(gender == 0,'Female','Male')) %>%
    mutate_if(is.character,clean)
##=====
# tango %>% glimpse()
# df_base.line %>% View()

# lead_recodes <- names(tango)[c(5,6,7,8)]
coop_1 <- tango %>% 
    filter(coop == '6') %>% 
    mutate_at(lead_recodes,funs(ifelse(is.na(.),'',.))) %>% 
    unite(years,member.years,leadership.years,sep = '') %>% 
    unite(type,member,leadership.position,sep = '') %>% 
    mutate(type = ifelse(type == 'MemberMember In Leadership','Member',type)) %>% 
           # type = ifelse(str_detect(type,'^Member'),paste(type,1:nrow(.),sep = '.'),type)) %>% 
    arrange(type) %>% 
    mutate(type = make.unique(type)) %>% 
    select(-leadership) %>% 
    na.omit(.)
# Coop_1 ------------------------------------------------------------------

names(coop_1)
df_coop_1 <- coop_1 %>% 
    select(type,person1,person2) %>% 
    gather(key,value,-type) %>% 
    select(from = value,to = type)


# coop_1_g ----------------------------------------------------------------

g <- graph_from_data_frame(df_coop_1)

 g_data <- toVisNetworkData(g)

nodes <- g_data$nodes %>% 
    dplyr::as_data_frame() %>% 
    set_names('id','type')
edges <- g_data$edges %>% 
    dplyr::as_data_frame()

# nodes <- left_join(nodes,coop_1)


# attributes --------------------------------------------------------------

nodes <- nodes %>%
    mutate(color = ifelse(gender == 'Female','#edf8b1','#7fcdbb'),
           color = ifelse(is.na(coop),'#2c7fb8',color),
           group = ifelse(is.na(gender),'Unknown',gender),
           size = rescale(degree(g),25,80)) %>%
    rename(label = type)

visNetwork(nodes,edges) %>% 
    visNodes(shadow = T,shape = 'oval',font = list(size = 10)) %>% 
    visEdges(arrows = "to",length = .25,color = list(opacity = .50)) %>% 
    visHierarchicalLayout(parentCentralization= FALSE,levelSeparation = 100)  %>%
    visGroups(groupname = "Male", color = "#edf8b1") %>%
    visGroups(groupname = "Female", color = "#7fcdbb") %>%
    visGroups(groupname = "Unknown", color = "#2c7fb8") %>%
    visPhysics(barnesHut = list(gravitationalConstant = -50000)) 
    

# ?visPhysics()
































