lf sg2137.asc
pause
clr all
pb clear
pb 7.404       9.651
pb 14.035      16.844
pb 20.609      21.733
do i 1 3 1
  ra sg2137.asc i i  new i
  po pb 3 i
end
pca 11.1
clr 
ra sg2137.asc 3 4  add 
mc pc 
sf
pl
pause
pb clear
pb 7.404       9.820
pb 13.754      14.484
po pb 
pl
pause
wa g2137-av.asc 
wa ../g2137.asc 
sy rm g2137.dat
wmca g2137.dat
