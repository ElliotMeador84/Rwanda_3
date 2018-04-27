library(igraph)
library(visNetwork)
library(tidyverse)
source('functions/functions.general.R')

mendely_citations <- read_csv('data/Mendely_Rwanda_citations.csv')


# tidy --------------------------------------------------------------------




graph_data <- data_frame(
    from = sub("^([^(]*\\(\\d{4}\\)).*", "\\1", mendely_citations$from),
    to = sub("^([^(]*\\d{4}).*", "\\1", mendely_citations$to)
) %>%
    na.omit(.)



graph_data <- map_df(graph_data, function(x) {
    a <- clean_all(x)
    a <- str_remove_all(a, '(^|\\s+).(\\s+|$)')
    str_remove_all(a, ' ')
})



n_distinct(graph_data$to)

graph_data <- graph_data %>%
    mutate(to = identicator(to))

n_distinct(graph_data$to)
# graph_data %>% View()


# quick vis ---------------------------------------------------------------
graph <- graph_from_data_frame(graph_data)


vertices <- data_frame(name = V(graph)$name,
           degree = degree(graph)) %>%
    filter(degree > 2) %>% 
    pull(name)

plot(induced_subgraph(graph,vertices),vertex.label = NA)
sug_graph <- induced_subgraph(graph,vertices)

vis_graph_data <- toVisNetworkData(sug_graph)
vis_graph_data$nodes <- vis_graph_data$nodes %>% 
    mutate(size = degree(sug_graph))

visNetwork(vis_graph_data$nodes,distinct(vis_graph_data$edges)) %>% 
    visOptions(highlightNearest = TRUE) %>%
    visLayout(improvedLayout = T)%>% 
    visInteraction(navigationButtons = TRUE) %>% 
    visHierarchicalLayout()


str_trim(vis_graph_data$nodes)






