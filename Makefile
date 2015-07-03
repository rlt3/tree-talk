all:

engine:
	clang -c class.c -o obj/class.o -I/usr/local/include/ -F/Library/Frameworks
	clang -c instance.c -o obj/instance.o -I/usr/local/include/ -F/Library/Frameworks
	clang -c script.c -o obj/script.o -I/usr/local/include/ -F/Library/Frameworks
	clang -c main.c -o obj/main.o -F/Library/Frameworks -I/usr/local/include/
	clang -o engine obj/main.o obj/instance.o obj/class.o obj/script.o -I/usr/include/ -L/usr/local/lib/ -llua -I/usr/include/ 

clean:
	rm -f engine obj/*.o
