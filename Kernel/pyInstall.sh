#!/usr/bin/env bash
alias py='/c/ProgramData/Anaconda3/python.exe -m conda'
# Used in China, because of The Great Fire Wall
#py config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/free/
#py config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/main/
#py config --set show_channel_urls yes
py update --all -y
py install pyzmq mxnet tensorflow keras -y
py install pytorch -c pytorch -y
py install willyd -c caffe-cpu -y
#py clean -p -y
#py clean -t -y
sleep 3600
