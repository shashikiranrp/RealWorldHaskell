all: compile link

compile: SimpleJSON.hs PutJSON.hs Main.hs
	ghc -c SimpleJSON.hs PutJSON.hs Main.hs

link: SimpleJSON.o Main.o PutJSON.o
	ghc -o simple SimpleJSON.o PutJSON.o Main.o

clean: 
	rm -f *hi *o simple

