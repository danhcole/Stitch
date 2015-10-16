#10/15

#### for next time (10/20 - 2:45)
- settle on grammer/ syntax
- have 3 small programs

#### restriction of loops - black- and white- list.  Start with realy simple loop primitive that we know how to translate (blacklist everything), whitelist stuff we can do.  
- ex. for loop w/ step
- const. vs. var index increment
 - do you have to know stride at compile time
 - cover matrix manipulation -> maybe more
- reading / writing(?) multiplexing -> how do you split data stream
- who's responsable for knowing the size of the array
 - for (i in row of array) -> do this
- gaussian elimination, invert a matrix, any scientific computing, etc. 
 - steal example from openMP
- hello world -> +1 to every element in an array
- better array syntax then C (ex. add array literal)

#### next deliverable - lang. ref. manual
- should be exhaustive and well defined (allowed and not allowed).
- correct and unambiguous grammar
- working scanner and parser
- if we go after parallel array ops -> need ints, floating point, don't need strings, etc.
 - bools
- instead of 'async' keyword, have special for loop?

####To Ask:
- Big question: Should we do positive or negative constraints, maybe a combination? How do we start to implement constraints in our language?
- Can/should we handle I/O with multithreading? Reading is possible, but writing is not(?).
- What other applications are there besides matrix manipulation that we can do with multithreading?
- Start on the parser?  Where to start - basic math ops, etc. out of the way?

####Our next steps: 
- Take the pthread code and write same version in our language.
- Come up with a “Hello world” program for our language.

#10/6

###Big challange - what can I do in parallel?

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
- Example programs:
	-Two matrices filled with random numbers, then multiplied together.
	-Reading from a file.

Things to bring up for next meeting: 
	File I/O, writing to buffer, pointer stuff

#Reference manuel
- what does it mean to put async infront of a loop
  - what limits
  - semantics of loops
  
#Other thoughts
- don't worry about strings
- don't worry about too many types
  - def/var
