#Big challange - what can I do in parallel?

####Q: How does marking a loop async work
- encapsulate and spawn a thread
- tim seems to know
- complier will figure out what needs to be locked?
  - very tricky (ex, looping through arrays with concurent async ops)

####Look at OpenMP
- can annotate loops as async, paralellizes them, but still blocks
  - may want to do something similar, OpenMP Lite
- look at old lecture notes from PLT

####Issues
- need to know if there are inter-thread dependencies
- focus on specific kinds of problems - ex. scientific computation
- very limited amount of async loops - ex. no pointers

####First step
- think about a simple program
- compile it w/ pthreads
- make sure we're all on the same page

#Reference manuel
- what does it mean to put async infront of a loop
  - what limits
  - semantics of loops
  
#Other thoughts
- don't worry about strings
- don't worry about too many types
  - def/var
