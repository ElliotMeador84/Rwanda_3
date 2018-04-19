library(tidyverse)
library(visNetwork)
library(RColorBrewer)
load('foxtrot.RData')

# Split by cooperative  ---------------------------------------------------


coop_df <- foxtrot %>%
    split(., .$coop)

# Gather ------------------------------------------------------------------

coop_df_long <- map(coop_df, function(x) {
    x %>%
        ungroup() %>% # have to ungroup even after split
        select(type, person1, person2) %>%
        gather(key, value, -type) %>%
        select(from = value, to = type)
})




# Make igraphs ----------------------------------------------------------------
coop_g_ls <- map(coop_df_long, function(x) {
    igraph::graph_from_data_frame(x)
})

# Make visNetworks --------------------------------------------------------
coop_vsN_dfs <- map(coop_g_ls, function(x) {
    toVisNetworkData(x)
})


# Create nodes data_frame -------------------------------------------------

nodes_vsN_dfs <- map(coop_vsN_dfs, function(x) {
    x$nodes %>%
        dplyr::as_data_frame() %>%
        set_names('id', 'type')
})


# Create edges data_frame -------------------------------------------------


edges_vsN_dfs <- map(coop_vsN_dfs, function(x) {
    as_tibble(x$edges)
})

# Create some node attributes ---------------------------------------------

# join node dataset with main dataset ====

nodes_vsN_dfs <- map2(nodes_vsN_dfs, coop_df, function(x, y) {
    left_join(x, y)
})

# Rcolor Ramper ====

spectral_color <- colorRampPalette(brewer.pal(11, 'Spectral'))

# Add color and size attributes ====

nodes_vsN_dfs <-  map2(nodes_vsN_dfs, coop_g_ls, function(x, y) {
    set.seed(123)
    x %>% mutate(
        color = sample(spectral_color(nrow(x)), nrow(x)),
        #color
        color = ifelse(grepl('^Member', x$type), 'grey',
                              color),
        # color, change member
        size = rescale(igraph::degree(y), 25, 80),
        ## notice we are sizing by degree from the `igraph` package
        Position = gsub('\\.\\d+', '', x$type)
    )
})


# create htmls ------------------------------------------------------------

coop_visNetworks_ls <-
    map2(nodes_vsN_dfs, edges_vsN_dfs, function(x, y) {
        visNetwork(x, y,
                   submain = 'Where do you go to get information?') %>%
            visNodes(shadow = T,
                     shape = 'oval',
                     font = list(size = 10),labelHighlightBold = T) %>%
            visEdges(
                arrows = "to",
                length = .25,
                color = list(opacity = .50)
            ) %>%
            visHierarchicalLayout(parentCentralization = FALSE,
                                  levelSeparation = 100) %>%
            visPhysics(barnesHut = list(gravitationalConstant = -50000)) %>%
            visOptions(
                highlightNearest = list(
                    enabled = TRUE,
                    algorithm = "hierarchical",
                    degree = list(from = 1, to = 1)
                ),
                selectedBy = 'Position'
            ) %>%
            visInteraction(navigationButtons = TRUE)
        
        
    })

names(coop_visNetworks_ls) <- c('Dairy_1',
                                'Dairy_2',
                                'Maize_1',
                                'Dairy_3',
                                'Dairy_4',
                                'Maize_2')
