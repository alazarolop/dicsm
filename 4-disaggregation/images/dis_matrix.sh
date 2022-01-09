#!/bin/zsh

for i in 17c 23c 29c
convert $i.tif -bordercolor white -border 25x30 $i.b.tif

convert 17c.b.tif 23c.b.tif 29c.b.tif -append dis_matrix.tif

convert dis_matrix.tif  -density 150x150 -resize 50%x50% dis_matrix-low.tif