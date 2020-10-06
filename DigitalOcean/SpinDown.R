library(analogsea)

images()

d=droplets()$`bot-test`

ret=d %>%
  droplet_power_off() %>%
  droplet_wait() %>%
  droplet_snapshot(name = "discrete-analysis") %>%
  droplet_wait() %>% 
  droplet_delete()


