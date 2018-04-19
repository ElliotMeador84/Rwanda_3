library(tidyverse)
library(visNetwork)
library(htmltools)
load('data/df_base.line.RData')

# Clean df_base.line  -----------------------------------------------------


foxtrot <- df_base.line %>% 
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
    mutate_if(is.character,clean)  %>% 
    group_by(coop) %>% 
    mutate_at(lead_recodes,funs(ifelse(is.na(.),'',.))) %>% 
    unite(years,member.years,leadership.years,sep = '') %>% 
    unite(type,member,leadership.position,sep = '') %>% 
    mutate(type = ifelse(type == 'MemberMember In Leadership','Member',type)) %>% 
    # type = ifelse(str_detect(type,'^Member'),paste(type,1:nrow(.),sep = '.'),type)) %>% 
    arrange(type) %>% 
    mutate(type = make.unique(type)) %>% 
    select(-leadership) %>% 
    na.omit(.)
   

# save foxtrot ====
save(foxtrot,file = 'html/foxtrot.RData') 

# Split by cooperative  ---------------------------------------------------


coop_df <- foxtrot %>% 
    split(.,.$coop)

# Gather ------------------------------------------------------------------

coop_df_long <- map(coop_df,function(x){
    x %>% 
        ungroup() %>% # have to ungroup even after split
        select(type,person1,person2) %>% 
        gather(key,value,-type) %>% 
        select(from = value,to = type)
})




# Make igraphs ----------------------------------------------------------------
coop_g_ls <- map(coop_df_long,function(x){
    igraph::graph_from_data_frame(x)
})

# Make visNetworks --------------------------------------------------------
coop_vsN_dfs <- map(coop_g_ls,function(x){
    toVisNetworkData(x)
})


# Create nodes data_frame -------------------------------------------------

nodes_vsN_dfs <- map(coop_vsN_dfs,function(x){
    x$nodes %>% 
        dplyr::as_data_frame() %>% 
        set_names('id','type')
})


# Create edges data_frame -------------------------------------------------


edges_vsN_dfs <- map(coop_vsN_dfs,function(x){
    as_tibble(x$edges )
})

dirname(sys.frame(1)$   ) 
# create htmls ------------------------------------------------------------

coop_visNetworks_ls <- map2(nodes_vsN_dfs,edges_vsN_dfs,function(x,y){
    visNetwork(x,y) %>% 
         visNodes(shadow = T,shape = 'oval',font = list(size = 10)) %>%
         visEdges(arrows = "to",length = .25,color = list(opacity = .50)) %>%
         visHierarchicalLayout(parentCentralization= FALSE,levelSeparation = 100)%>%
         visPhysics(barnesHut = list(gravitationalConstant = -50000)) 
        
})

names(coop_visNetworks_ls) <- c('Dairy_1',
                                'Dairy_2',
                                'Maize_1',
                                'Dairy_3',
                                'Dairy_4',
                                'Maize_2')
file.exists('scripts/david.correspondence/coop_vis.R')
file.copy('scripts/david.correspondence/coop_vis.R','html/coop_vis.R')

