engine:
	clang -c class.c -o obj/class.o -I/usr/local/include/ -F/Library/Frameworks
	clang -c instance.c -o obj/instance.o -I/usr/local/include/ -F/Library/Frameworks
	clang -c script.c -o obj/script.o -I/usr/local/include/ -F/Library/Frameworks
	clang -c main.c -o obj/main.o -F/Library/Frameworks -I/usr/local/include/
	clang -o engine obj/main.o obj/class.o obj/instance.o obj/script.o -I/usr/include/ -L/usr/local/lib/ -llua -I/usr/include/ 

new:
	clang -c instance.c -o obj/instance.o -I/usr/local/include/ -F/Library/Frameworks
	clang -c component.c -o obj/component.o -I/usr/local/include/ -F/Library/Frameworks
	clang -c script.c -o obj/script.o -I/usr/local/include/ -F/Library/Frameworks
	clang -c main.c -o obj/main.o -F/Library/Frameworks -I/usr/local/include/
	clang -o engine obj/main.o obj/component.o obj/instance.o obj/script.o -I/usr/include/ -L/usr/local/lib/ -llua -I/usr/include/ 

clean:
	rm -f engine obj/*.o
