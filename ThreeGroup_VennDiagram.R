### setup working path ###
setwd("D:/Intern_application")

### load packages ###
library(VennDiagram)

### define function ###
ThreeSampleVenn = function(A,B,C,AB,BC,AC){
  # check the 6 numbers
  flag = TRUE
  if (A < 0){
    print ("Invalid group size A")
    flag = FALSE}
  if (B < 0){
    print ("Invalid group size B")
    flag = FALSE}
  if (C < 0){
    print ("Invalid group size C")
    flag = FALSE}
  if (AB < 0){
    print ("Invalid overlap between group A and B")
    flag = FALSE}
  if (BC < 0){
    print ("Invalid overlap between group B and C")
    flag = FALSE}
  if (AC < 0){
    print ("Invalid overlap between group A and C")
    flag = FALSE}
  if (AC > A | AC > B){
    print ("Invalid overlap between group A and B")
    flag = FALSE}
  if (BC > B | BC > C){
    print ("Invalid overlap between group B and C")
    flag = FALSE}
  if (AC > A | AC > C){
    print ("Invalid overlap between group A and C")
    flag = FALSE}
  
  if (flag){
    # determine the size shared by all 3 groups using random number generator
    minimum = max(AC+BC-C, AB+AC-A, AB+BC-B)
    maximum = min(AB,BC,AC)
    ABC = sample(minimum:maximum, 1)
    
    # Venn diagram plotting
    tiff(file = "ThreeClub_VennDiagram.tiff", width = 1200, height =800, units = "px", res = 300)
    draw.triple.venn(A, B, C, AB, BC, AC, ABC, category = c("Group A", "Group B", "Group C"))
    dev.off()
  }
  
}




### execution ###
A=200
B=140
C=50
AB=120
AC=35
BC=30

ThreeSampleVenn(A,B,C,AB,BC,AC)


