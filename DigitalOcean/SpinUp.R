library(analogsea)

imgs=images(per_page=100)

my_snapshots = which(!unlist(lapply(imgs, function(x){x$public})))
  

my_snapshot = unlist(lapply(my_snapshots, function(x){imgs[[x]]$id}))["lobby-bot"]


user_data="#!/bin/bash
 sudo fallocate -l 16G /swapfile;chmod 600 /swapfile;mkswap /swapfile; swapon /swapfile;"

#user_data=""

#my_snapshot=imgs[["14.04.5 x64"]]$slug


big = "s-32vcpu-192gb"
small = "s-1vcpu-1gb"
medium = "s-8vcpu-32gb"

data_bot = "s-4vcpu-8gb"

i=0


i=i+1;
#i="small"
#d <- droplet_create(size = big, name=paste0("bot-test-",i), region="nyc3"
d <- droplet_create(size = small, name=paste0("bot-test"), region="nyc3"
                    , image=my_snapshot, user_data=user_data) %>%
  droplet_wait()

drops=droplets()
d=drops[[length(drops)]]

ip=d$networks[[1]][[1]][[1]]
ip
