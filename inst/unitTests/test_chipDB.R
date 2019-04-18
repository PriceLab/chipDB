library(RUnit)
library(chipDB)
#------------------------------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#------------------------------------------------------------------------------------------------------------------------
if(!exists("cdb"))
   cdb <- chipDB(quiet=FALSE)
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_constructor()
   test_getHits()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_constructor = function ()
{
   printf("--- test_constructor")
   checkTrue(is(cdb) %in% "chipDB")

} # test_constructor
#------------------------------------------------------------------------------------------------------------------------
test_getHits <- function()
{
   printf("--- test_getHits")
   # GATA2 hg38 + some padding: chr3:128,474,823-128,512,102
   tbl <- getHits(cdb, "chr3", 128474823, 128512102)
   browser()
   xyz <- 101

} # test_getHits
#------------------------------------------------------------------------------------------------------------------------