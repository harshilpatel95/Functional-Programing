all: dict myset pagerank moogle

# library
LIBS = unix.cma str.cma

# These must be in the right order--no forward refs
DICT_FILES = order.ml dict.ml

SET_FILES = $(DICT_FILES) myset.ml

RANK_FILES = $(SET_FILES) graph.ml nodescore.ml util.ml query.ml pagerank.ml

MOOGLE_FILES = $(RANK_FILES) crawl.ml moogle.ml 


# compiling options
dict: $(DICT_FILES)
	ocamlc -g -o dict $(LIBS) $(DICT_FILES)

myset: $(SET_FILES)
	ocamlc -g -o myset $(LIBS) $(SET_FILES)

pagerank: $(RANK_FILES)
	ocamlc -g -o pagerank $(LIBS) $(RANK_FILES)

moogle: $(MOOGLE_FILES)
	ocamlc -g -o moogle $(LIBS) $(MOOGLE_FILES)


# testing options
testmyset: myset
	./myset

testdict: dict
	./dict

testpagerank: pagerank 
	./pagerank 8080 200 wiki/Teenage_Mutant_Ninja_Turtles

test: dict myset pagerank 
	./myset
	./dict
	./pagerank 8080 200 wiki/Teenage_Mutant_Ninja_Turtles


# start Moogle web server (with different searching source options)
servesimplehtml: moogle
	./moogle 8080 7 simple-html/index.html

servehtml: moogle
	./moogle 8080 20 html/index.html

servewikismall: moogle
	./moogle 8080 45 wiki/Teenage_Mutant_Ninja_Turtles

serve: moogle
	./moogle 8080 200 wiki/Teenage_Mutant_Ninja_Turtles


# Clean moogle project folder
# Submission: (1) run "make clean", (2) zip your moogle folder, (3) submit zip file to CSNS
clean: 
	rm -f dict myset pagerank moogle *.cmi *.cmo
	
	
	
	
	
	
	







