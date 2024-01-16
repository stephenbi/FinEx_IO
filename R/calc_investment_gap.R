# GLO #
q_REdir_tot_host_reg %>% filter((grepl("2022", data) | grepl("PkBud", data)) & region %in% c("GLO")) %>% group_by(data) %>% summarise(volume=sum(value),.groups="keep")

#OAS#
q_REdir_tot_host_reg %>% filter((grepl("2022", data) | grepl("PkBud", data)) & region %in% c("OAS")) %>% group_by(data) %>% summarise(volume=sum(value),.groups="keep")

# NEU #
q_REdir_tot_host_reg %>% filter((grepl("2022", data) | grepl("PkBud", data)) & region %in% c("NEU")) %>% group_by(data) %>% summarise(volume=sum(value),.groups="keep")

# MEA #
q_REdir_tot_host_reg %>% filter((grepl("2022", data) | grepl("PkBud", data)) & region %in% c("MEA")) %>% group_by(data) %>% summarise(volume=sum(value),.groups="keep")

# SSA #
q_REdir_tot_host_reg %>% filter((grepl("2022", data) | grepl("PkBud", data)) & region %in% c("SSA")) %>% group_by(data) %>% summarise(volume=sum(value),.groups="keep")

# Others #
q_REdir_tot_host_reg %>% filter((grepl("2022", data) | grepl("PkBud", data)) & region %in% c("REF", "CAZ", "LAM")) %>% group_by(data) %>% summarise(volume=sum(value),.groups="keep")

