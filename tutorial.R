#####(00) load libraries needed
packages <- c('tidyverse','conflicted',#'clipr','psych',
              'tidygraph','ggraph')

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
conflicted::conflict_prefer("filter", "dplyr")
invisible(lapply(packages, library, character.only = TRUE))

#####(01) Data Importing

# Bills - http://data.ly.gov.tw/odw/usageFile.action?id=20
# 議案提案 - 提供委員及政府之議案提案資訊。(自第8屆第1會期起)
df_index <- read_csv("http://data.ly.gov.tw/odw/usageFile.action?id=20&type=CSV&fname=20_CSV.csv")
df_index %>% head(1)
df_index %>% head(1) %>% select(-docUrl, -pdfUrl, -billNo, -sessionTimes, -billOrg, -meetingTimes)
df_index %>% glimpse()
df_index %>% head(1) %>% select(-docUrl, -pdfUrl, -billNo, -sessionTimes, -billOrg, -meetingTimes) %>% glimpse()

# 歷屆委員資料(自第2屆起)
# https://data.ly.gov.tw/getds.action?id=16
df_legislator <- read_csv("http://data.ly.gov.tw/odw/usageFile.action?id=16&type=CSV&fname=16_CSV.csv")
df_legislator %>% glimpse()

#####(02) Data Munging
# skip
df_index <- read_csv("data/df_index.csv")
df_legislator <- read_csv("data/df_legislator.csv")
df_bill_pro_cos_09 <- read_csv("data/df_bill_pro_cos_09.csv")
df_legislator_09 <- read_csv("data/df_legislator_09.csv")
df_party_english <- read_csv("data/df_party_english.csv")
value_team_color_english = c("KMT"="#000095","DPP"="#1B9431",
                             "PFP"="#FF6310","TSU"="#AB6300",
                             "MKT"="#FFEA00","NSU"="#C20F51",
                             "NPP"="#F9BE01","TPP"="#28C8C8")

#####(03) Data Transforming

###igraph

### Warnings
conflict_prefer("filter", "dplyr")
tidygraph::filter()

### tbl_graph()
# example
example_nodes <- tibble(name = c("Hadley", "David", "Romain", "Julia", "Mary"))
example_edges <- tibble(from = c(1, 1, 1, 2, 2, 3, 4, 4, 5),
                        to = c(2, 3, 5, 1, 4, 1, 1, 2, 1))
example_graph <- tbl_graph(nodes = example_nodes, edges = example_edges)
example_graph
example_graph %>%
  ggraph(layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(size = 8, colour = 'white') +
  geom_node_text(aes(label = name), colour = 'black')

example_tbl <- tibble(from = c("Hadley", "Hadley", "Hadley", "David", "David", "Romain", "Julia", "Katie"),
                      to = c("David", "Romain", "Julia", "Hadley", "Katie", "Hadley", "Hadley", "David"))

example_tbl %>% as_tbl_graph()
example_tbl %>% as_tbl_graph() %>%
  ggraph(layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(size = 8, colour = 'white') +
  geom_node_text(aes(label = name), colour = 'black')

# our case
network_bill_09 <- df_bill_pro_cos_09 %>% filter(n > 0) %>%
  select(from = pro, to = cos, weight = n, pro_party, cos_party, pro_party_s_e, cos_party_s_e) %>% 
  as_tbl_graph() %>%
  left_join(df_legislator_09 %>% select(term, name, sex, party, areaName)) %>%
  left_join(df_party_english %>% select(party = 政黨, party_s_e = Party_s))

network_bill_09

### activate() and tidyverse
network_bill_09 %>%
  activate(edges) %>%
  filter(pro_party_s_e == "KMT")

df_bill_pro_cos_09 %>%
  filter(n > 0) %>%
  filter(pro_party_s_e == "KMT")

df_bill_pro_cos_09 %>% filter(n > 0) %>%
  select(from = pro, to = cos, weight = n, pro_party, cos_party, pro_party_s_e, cos_party_s_e) %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  left_join(df_legislator_09 %>% select(term, name, sex, party, areaName)) %>%
  left_join(df_party_english %>% select(party = 政黨, party_s_e = Party_s))

network_bill_09 %>%
  activate(nodes) %>%
  left_join(df_legislator_09 %>% select(term, name, sex, party, areaName)) %>%
  left_join(df_party_english %>% select(party = 政黨, party_s_e = Party_s)) %>%
  mutate(flag_district = if_else(str_detect(areaName, "不分區"), 0, 1))

network_bill_09 %>%
  activate(edges) %>%
  as_tibble()

network_bill_09 %>%
  activate(edges) %>%
  active()

###(04) EDA 探索性分析
### importance
### 回答問題

# do their job/有在做事
# got the most cosponsor times/提案獲得最多人連署的立委
df_bill_pro_cos_09

# 連署最多/最少的立委是
df_bill_pro_cos_09 %>% group_by(cos, cos_party, cos_party_ind) %>% 
  summarise(n = sum(n)) %>% ungroup() %>% group_by(cos_party, cos_party_ind) %>%
  arrange(cos_party_ind, desc(n)) %>% filter(row_number() <= 3) 

df_bill_pro_cos_09 %>% group_by(cos, cos_party, cos_party_ind) %>% 
  summarise(n = sum(n)) %>% ungroup() %>% group_by(cos_party, cos_party_ind) %>%
  arrange(cos_party_ind, n) %>% filter(row_number() <= 3) 

# 被連署最多/最少的立委
df_bill_pro_cos_09 %>% group_by(cos, pro_party, pro_party_ind) %>% 
  summarise(n = sum(n)) %>% ungroup() %>% group_by(pro_party, pro_party_ind) %>%
  arrange(pro_party_ind, desc(n)) %>% filter(row_number() <= 3) 

df_bill_pro_cos_09 %>% group_by(pro, pro_party, pro_party_ind) %>% 
  summarise(n = sum(n)) %>% ungroup() %>% group_by(pro_party, pro_party_ind) %>%
  arrange(pro_party_ind, n) %>% filter(row_number() <= 3) 

# 被連署跟連署別人最多/少的立委

### centrality_()
network_bill_09 %>% 
  activate(edges) %>% 
  filter(weight >= 50) %>%
  activate(nodes) %>%
  mutate(centrality_degree = centrality_degree(),
         centrality_closeness = centrality_closeness(),
         centrality_betweenness  = centrality_betweenness()) %>%
  arrange(desc(centrality_degree)) %>%
  select(name, party, centrality_degree)

df_bill_pro_cos_09 %>%
  filter(n >= 50) %>%
  group_by(pro) %>% summarise(degree = n_distinct(cos)) %>%
  arrange(desc(degree))

network_bill_09 %>% 
  activate(edges) %>% 
  filter(weight >= 50) %>%
  activate(nodes) %>%
  mutate(centrality_degree = centrality_degree(),
         centrality_closeness = centrality_closeness(),
         centrality_betweenness  = centrality_betweenness()) %>%
  filter(centrality_degree > 0) %>%
  ggraph(layout = "fr") + 
  geom_edge_density(aes(fill = weight)) +
  geom_edge_link0(aes(width = weight), alpha = 0.2) +
  geom_node_point(aes(fill = factor(party_s_e), alpha = centrality_degree),size=3,shape=21,col="grey25") +
  scale_fill_manual(values=value_team_color_english) +#,labels = value_team_pair) +
  scale_edge_width_continuous(range=c(0.1,4)) +
  scale_size_continuous(range=c(1,8)) +
  theme_graph() +
  theme(text = element_text(family = "Noto Sans CJK TC Medium"))

# 各黨在連署上表現的緊密程度
# skip

#幫連署
df_bill_pro_cos_09 %>% 
  group_by(cos_party, cos_party_ind) %>% count(pro_party == cos_party) %>%
  mutate(per = n/sum(n)) %>% arrange(cos_party_ind)
#被連署
df_bill_pro_cos_09 %>% 
  group_by(pro_party, pro_party_ind) %>% count(pro_party == cos_party) %>%
  mutate(per = n/sum(n)) %>% arrange(pro_party_ind)

# 小黨在連署上有何特色: 特色怎麼定義
df_bill_pro_cos_09 %>%
  filter(!((pro_party == "中國國民黨" & cos_party == "民主進步黨") | (cos_party == "中國國民黨" & pro_party == "民主進步黨"))) %>%
  filter(!((pro_party == "中國國民黨" & cos_party == "中國國民黨") | (cos_party == "民主進步黨" & pro_party == "民主進步黨"))) %>%
  count(pro_party, pro_party_ind, cos_party, cos_party_ind, sort = T) %>%
  arrange(desc(pro_party_ind), desc(cos_party_ind))

# 跨黨派連署情形
df_bill_pro_cos_09 %>% count(pro_party == cos_party)

df_bill_pro_cos_09 %>% filter(pro_party != cos_party) %>%
  count(pro, pro_party, cos, cos_party, sort = T)

df_bill_pro_cos_09 %>% filter(pro_party != cos_party) %>%
  filter((pro_party == "中國國民黨" & cos_party == "民主進步黨") | (cos_party == "中國國民黨" & pro_party == "民主進步黨")) %>%
  count(pro, pro_party, cos, cos_party, sort = T)

#幫他黨: by 人
df_bill_pro_cos_09 %>%
  filter(pro_party_ind != cos_party_ind) %>%
  filter(n >= 10) %>%
  select(from = pro, to = cos, weight = n) %>% 
  as_tbl_graph(directed = F) %>% 
  activate(nodes) %>%
  left_join(df_legislator_09 %>% select(term, name, sex, party, areaName)) %>%
  left_join(df_party_english %>% select(party = 政黨, party_e = Party, party_s_e = Party_s, party_index = Party_importance)) %>%
  ggraph(layout = "fr") + 
  geom_edge_density(aes(fill = weight)) +
  geom_edge_link0(aes(width = weight), alpha = 0.2) +
  geom_node_point(aes(fill = factor(party_s_e)), size = 5,shape=21,col="grey25") +
  scale_fill_manual(values=value_team_color_english) +#,labels = value_team_pair) +
  scale_edge_width_continuous(range=c(0.1,1.5)) +
  scale_size_continuous(range=c(1,8)) +
  ggraph::geom_node_text(aes(label = name), size = 3, repel = TRUE, family = "Noto Sans CJK TC Medium") +
  ggraph::theme_graph() +
  labs(title = "The 9th Legislative Yuan Cosponsorship Network",
       subtitle = "Threshold: N = 10") +
  theme(text = element_text(family = "Noto Sans CJK TC Medium"),
        plot.title = element_text(family = "Noto Sans CJK TC Medium"),
        plot.subtitle = element_text(family = "Noto Sans CJK TC Medium"))


#幫他黨-DPPandKMT: by 人
df_bill_pro_cos_09 %>%
  filter(pro_party_ind != cos_party_ind) %>%
  filter(pro_party_ind == 1 | cos_party_ind == 1) %>%
  filter(pro_party_ind == 2 | cos_party_ind == 2) %>%
  filter(n >= 5) %>%
  select(from = pro, to = cos, weight = n) %>% 
  tidygraph::as_tbl_graph(directed = F) %>% 
  tidygraph::activate(nodes) %>%
  left_join(df_legislator_09 %>% select(term, name, sex, party, areaName)) %>%
  left_join(df_party_english %>% select(party = 政黨, party_e = Party, party_s_e = Party_s, party_index = Party_importance)) %>%
  ggraph(layout = "fr") + 
  geom_edge_density(aes(fill = weight)) +
  geom_edge_link0(aes(width = weight), alpha = 0.2) +
  geom_node_point(aes(fill = factor(party_s_e)), size = 5,shape=21,col="grey25") +
  scale_fill_manual(values=value_team_color_english) +#,labels = value_team_pair) +
  scale_edge_width_continuous(range=c(0.1,1.5)) +
  scale_size_continuous(range=c(1,8)) +
  geom_node_text(aes(label = name), size = 3, repel = TRUE, family = "Noto Sans CJK TC Medium") +
  theme_graph() +
  labs(title = "The 9th Legislative Yuan Cosponsorship Network",
       subtitle = "Threshold: N = 5") +
  theme(text = element_text(family = "Noto Sans CJK TC Medium"),
        plot.title = element_text(family = "Noto Sans CJK TC Medium"),
        plot.subtitle = element_text(family = "Noto Sans CJK TC Medium"))

### community detection/有沒有連署分群特性&有沒有連署集團
df_bill_pro_cos_09 %>%
  select(from = pro, to = cos, weight = n) %>% 
  tidygraph::as_tbl_graph(directed = F) %>% 
  tidygraph::activate(nodes) %>%
  left_join(df_legislator_09 %>% select(term, name, sex, party, areaName)) %>%
  left_join(df_party_english %>% select(party = 政黨, party_e = Party, party_s_e = Party_s, party_index = Party_importance)) %>%
  mutate(group_louvain = as.character(tidygraph::group_louvain(weights = weight))) %>%
  mutate(group_components = as.character(tidygraph::group_components())) %>%
  as_tibble() %>% select(party, party_index, group_louvain) %>%
  mutate(group_louvain = fct_relevel(fct_recode(group_louvain, "1" = "2", "2" = "1"),"1","2","3")) %>%
  count(party, party_index, group_louvain) %>%
  arrange(group_louvain)

layout_lou <- df_bill_pro_cos_09 %>%
  filter(n > 0) %>%
  select(from = pro, to = cos, weight = n) %>% 
  as_tbl_graph(directed = F) %>% 
  activate(nodes) %>%
  left_join(df_legislator_09 %>% select(term, name, sex, party, areaName)) %>%
  left_join(df_party_english %>% select(party = 政黨, party_e = Party, party_s_e = Party_s, party_index = Party_importance)) %>%
  mutate(group_louvain = as.character(tidygraph::group_louvain(weights = weight))) %>%
  mutate(group_louvain = fct_relevel(fct_recode(group_louvain, "1" = "2", "2" = "1"),"1","2")) %>%
  create_layout(layout = "fr")

p1 <- ggraph(graph = layout_lou) + 
  geom_node_point(aes(fill = factor(party_s_e)), size = 5,shape=21,col="grey25") +
  scale_fill_manual(values=value_team_color_english) +#,labels = value_team_pair) +
  theme_graph() +
  labs(title = "The 9th Legislative Yuan Cosponsorship Network - Reality",
       subtitle = "Threshold: N = 1") +
  theme(text = element_text(family = "Noto Sans CJK TC Medium"),
        plot.title = element_text(family = "Noto Sans CJK TC Medium"),
        plot.subtitle = element_text(family = "Noto Sans CJK TC Medium"))

value_lou_color = c("1"="#000095","2"="#1B9431","3"="#28C8C8")
p2 <- ggraph(graph = layout_lou) + 
  geom_node_point(aes(fill = factor(group_louvain)), size = 5,shape=21,col="grey25") +
  scale_fill_manual(values=value_lou_color) +#,labels = value_team_pair) +
  theme_graph() +
  labs(title = "The 9th Legislative Yuan Cosponsorship Network - Group",
       subtitle = "Threshold: N = 1") +
  theme(text = element_text(family = "Noto Sans CJK TC Medium"),
        plot.title = element_text(family = "Noto Sans CJK TC Medium"),
        plot.subtitle = element_text(family = "Noto Sans CJK TC Medium"))

gridExtra::grid.arrange(p1, p2)

# 黨內的連署集團 - 民進黨派系
df_dpp_party <- read_csv("data/df_dpp_party.csv")
df_dpp_party
df_dpp_party_tmp <- df_bill_pro_cos_09 %>%
  filter(pro_party_ind == 1 & cos_party_ind == 1) %>% 
  select(pro) %>%
  rename(name = pro) %>%
  left_join(dplyr::distinct(dplyr::select(df_dpp_party,name, sub_party)), by = "name") %>%
  distinct(name, sub_party) %>%
  mutate(sub_party = if_else(is.na(sub_party), "", sub_party)) %>%
  mutate(sub_party = str_sub(sub_party,1,1))
df_dpp_party_tmp

df_bill_pro_cos_09 %>%
  filter(pro_party_ind == 1 & cos_party_ind == 1) %>% 
  filter(n >= 1) %>%
  select(from = pro, to = cos, weight = n) %>% 
  tidygraph::as_tbl_graph(directed = F) %>% 
  tidygraph::activate(nodes) %>%
  left_join(df_legislator_09 %>% filter(term == 9) %>% 
              select(term, name, sex, party, areaName, committee, degree, leaveFlag, leaveReason)) %>%
  left_join(df_party_english %>% select(party = 政黨, party_e = Party, party_s_e = Party_s, party_index = Party_importance)) %>%
  mutate(group_louvain = as.character(tidygraph::group_louvain(weights = weight))) %>%
  left_join(df_dpp_party_tmp) %>%
  ggraph(layout = "fr") + 
  geom_node_point(aes(fill = group_louvain), size = 5,shape=21,col="grey25") +
  geom_node_text(aes(label = sub_party), size = 3, repel = TRUE, family = "Noto Sans CJK TC Medium") +
  # theme(axis.title = element_text(family = "Noto Sans CJK TC Medium", face = "plain")
  ggraph::theme_graph() +
  labs(title = "The 9th Legislative Yuan Cosponsorship Network - DPP",
       subtitle = "Threshold: N = 30") +
  theme(text = element_text(family = "Noto Sans CJK TC Medium"),
        plot.title = element_text(family = "Noto Sans CJK TC Medium"),
        plot.subtitle = element_text(family = "Noto Sans CJK TC Medium"))

df_bill_pro_cos_09 %>%
  filter(pro_party_ind == 2 & cos_party_ind == 2) %>% 
  filter(n >= 1) %>%
  select(from = pro, to = cos, weight = n) %>% 
  tidygraph::as_tbl_graph(directed = F) %>% 
  tidygraph::activate(nodes) %>%
  left_join(df_legislator_09 %>% filter(term == 9) %>% 
              select(term, name, sex, party, areaName, committee, degree, leaveFlag, leaveReason)) %>%
  left_join(df_party_english %>% select(party = 政黨, party_e = Party, party_s_e = Party_s, party_index = Party_importance)) %>%
  mutate(group_louvain = as.character(tidygraph::group_louvain(weights = weight))) %>%
  # left_join(df_dpp_party_tmp) %>%
  ggraph(layout = "fr") + 
  geom_node_point(aes(fill = group_louvain), size = 5,shape=21,col="grey25") +
  geom_node_text(aes(label = name), size = 3, repel = TRUE, family = "Noto Sans CJK TC Medium") +
  # theme(axis.title = element_text(family = "Noto Sans CJK TC Medium", face = "plain")
  theme_graph() +
  labs(title = "The 9th Legislative Yuan Cosponsorship Network - KMT",
       subtitle = "Threshold: N = 30") +
  theme(text = element_text(family = "Noto Sans CJK TC Medium"),
        plot.title = element_text(family = "Noto Sans CJK TC Medium"),
        plot.subtitle = element_text(family = "Noto Sans CJK TC Medium"))

### 與特定人士的距離
df_bill_pro_cos_09 %>%
  filter(n >= 1) %>%
  select(from = pro, to = cos, weight = n) %>% 
  tidygraph::as_tbl_graph(directed = F) %>% 
  tidygraph::activate(nodes) %>%
  left_join(df_legislator_09 %>% filter(term == 9) %>% 
              select(term, name, sex, party, areaName)) %>%
  left_join(df_party_english %>% select(party = 政黨, party_e = Party, party_s_e = Party_s, party_index = Party_importance)) %>%
  mutate(n_rank_trv = node_rank_traveller(weights = weight),
         center = node_is_center(),
         universal = node_is_universal(),
         dist_to_center = node_distance_to(node_is_center(), weights = weight)) %>%
  mutate(distance_Hsu_all = node_distance_to(name == "徐永明", mode = "all", weights = weight),
         similarity_Hsu_all = node_similarity_with(name == "徐永明", mode = "all")
  ) %>%
  mutate(name_hsu = if_else(name == "徐永明", name, "")) %>%
  mutate(clutering_coef = local_transitivity(weights = weight)) %>%
  # as_tibble() %>% select(name, matches("Hsu|center|universal")) %>%
  # filter(!center) 
  # count(similarity_Hsu_all)
  ggraph(layout = "fr") + 
  geom_node_point(aes(fill = factor(party_s_e), alpha = -distance_Hsu_all), size = 5,shape=21,col="grey25") +
  #similarity_Hsu_all
  scale_fill_manual(values=value_team_color_english) +#,labels = value_team_pair) +
  ggraph::geom_node_text(aes(label = name_hsu), size = 3, repel = TRUE, family = "Noto Sans CJK TC Medium") +
  theme_graph() +
  labs(title = "The 9th Legislative Yuan Cosponsorship Network - Distance",
       subtitle = "Threshold: N = 30") +
  theme(text = element_text(family = "Noto Sans CJK TC Medium"),
        plot.title = element_text(family = "Noto Sans CJK TC Medium"),
        plot.subtitle = element_text(family = "Noto Sans CJK TC Medium"))

### layout
example_graph %>%
  activate(nodes) %>%
  mutate(gender = c("F","M")[row_number()%%2+1],
         age = c("20","22")[row_number()%%2+1]) %>%
  ggraph(layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(size = 8, colour = 'white') +
  geom_node_text(aes(label = name), colour = 'black')

example_graph %>%
  activate(nodes) %>%
  mutate(gender = c("F","M")[row_number()%%2+1],
         age = c("20","22")[row_number()%%2+1]) %>%
  ggraph(layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(size = 8, aes(colour = gender)) +
  geom_node_text(aes(label = name), colour = 'black')

# layout 會跑掉 - 先存下來
layout_example <- example_graph %>% 
  create_layout(layout = 'igraph', algorithm = "kk")
# layout 選項
layout_example %>%
  ggraph() + 
  geom_edge_link() + 
  geom_node_point(size = 8, colour = 'white') +
  geom_node_text(aes(label = name), colour = 'black')

### node
example_graph %>% 
  mutate(gender = c("F","M")[row_number()%%2+1],
         age = c("20","22")[row_number()%%2+1]) %>%
  create_layout(layout = 'nicely') %>%
  ggraph() + 
  geom_edge_link() + 
  geom_node_point(size = 8, colour = 'white') +
  # geom_node_tile(colour = 'black', width = 0.1, height = 0.11) +
  geom_node_text(aes(label = name), colour = 'black')

# geom_node_point()
# geom_node_text()
# geom_node_tile()
# geom_node_circle()
# geom_node_arc_bar()
# geom_node_range()

### edge
example_graph %>% 
  mutate(gender = c("F","M")[row_number()%%2+1],
         age = c("20","22")[row_number()%%2+1]) %>%
  create_layout(layout = 'nicely') %>%
  ggraph() + 
  # geom_edge_link() +
  # geom_edge_parallel() +
  # geom_edge_fan() + 
  # geom_edge_diagonal() +
  geom_edge_elbow() +
  geom_node_point(size = 8, colour = 'white') +
  geom_node_text(aes(label = name), colour = 'black')

# geom_edge_link() geom_edge_link2() geom_edge_link0()
# geom_edge_arc() geom_edge_arc2() geom_edge_arc0()
# geom_edge_parallel() geom_edge_parallel2() geom_edge_parallel0()
# geom_edge_fan() geom_edge_fan2() geom_edge_fan0()
# geom_edge_diagonal() geom_edge_diagonal2() geom_edge_diagonal0()
# geom_edge_elbow()

library(visNetwork)
# minimal example
nodes <- data.frame(id = 1:3)
edges <- data.frame(from = c(1,2), to = c(1,3))
visNetwork(nodes, edges, width = "100%")

  
  